(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Link a set of .cmx/.o files and produce an executable *)

open Misc
open Config
open Cmx_format
open Compilenv

module String = Misc.Stdlib.String

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Missing_cmx of filepath * modname
  | Link_error of Linkdeps.error

exception Error of error

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = ref ([] : string list)

module Cmx_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : string list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc -> Cmi_consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  implementations := unit.ui_name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.ui_name crc file_name;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Cmi_consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

let runtime_lib () =
  let libname = "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let object_file_name_of_file = function
  | Unit (fname, _, _) -> Some (Filename.chop_suffix fname ".cmx" ^ ext_obj)
  | Library (fname, infos) ->
      let obj_file = Filename.chop_suffix fname ".cmxa" ^ ext_lib in
      (* MSVC doesn't support empty .lib files, and macOS struggles to make
         them (#6550), so there shouldn't be one if the .cmxa contains no
         units. The file_exists check is added to be ultra-defensive for the
         case where a user has manually added things to the .a/.lib file *)
      if infos.lib_units = [] && not (Sys.file_exists obj_file) then None else
      Some obj_file

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file ldeps file tolink = match file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      Linkdeps.add ldeps
        ~filename:file_name ~compunit:info.ui_name
        ~provides:[info.ui_name]
        ~requires:(List.map fst info.ui_imports_cmx);
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
           || !Clflags.link_everything
           || Linkdeps.required ldeps info.ui_name
           then begin
             Linkdeps.add ldeps
               ~filename:file_name ~compunit:info.ui_name
               ~provides:[info.ui_name]
               ~requires:(List.map fst info.ui_imports_cmx);
             (info, file_name, crc) :: reqd
           end else
           reqd)
        infos.lib_units tolink

(* Second pass: generate the startup file and link it with everything else *)

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata ([Cmm.Csymbol_address "caml_startup"]))

let make_globals_map units_list ~crc_interfaces =
  let crc_interfaces = String.Tbl.of_seq (List.to_seq crc_interfaces) in
  let defined =
    List.map (fun (unit, _, impl_crc) ->
        let intf_crc = String.Tbl.find crc_interfaces unit.ui_name in
        String.Tbl.remove crc_interfaces unit.ui_name;
        (unit.ui_name, intf_crc, Some impl_crc, unit.ui_defines))
      units_list
  in
  String.Tbl.fold (fun name intf acc ->
      (name, intf, None, []) :: acc)
    crc_interfaces defined

let make_startup_file ~ppf_dump ~no_global_map ~crc_interfaces units_list =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup";
  (* set the name of the "current" compunit *)
  Emit.begin_assembly ();
  let name_list =
    List.flatten (List.map (fun (info,_,_) -> info.ui_defines) units_list) in
  let entry = Cmm_helpers.entry_point name_list in
  let entry =
    if Config.tsan then
      match entry with
      | Cfunction ({ fun_body; _ } as cf) ->
          Cmm.Cfunction
            { cf with fun_body = Thread_sanitizer.wrap_entry_exit fun_body }
      | _ -> assert false
    else
      entry
  in
  compile_phrase entry;
  let units = List.map (fun (info,_,_) -> info) units_list in
  List.iter compile_phrase
    (Cmm_helpers.emit_preallocated_blocks [] (* add gc_roots (for dynlink) *)
      (Cmm_helpers.generic_functions false units));
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table name_list);
  if not no_global_map then begin
    let globals_map = make_globals_map units_list ~crc_interfaces in
    compile_phrase (Cmm_helpers.globals_map globals_map);
  end else begin
    compile_phrase (Cmm_helpers.globals_map [])
  end;
  compile_phrase(Cmm_helpers.data_segment_table ("_startup" :: name_list));
  if !Clflags.function_sections then
    compile_phrase
      (Cmm_helpers.code_segment_table("_hot" :: "_startup" :: name_list))
  else
    compile_phrase(Cmm_helpers.code_segment_table("_startup" :: name_list));
  let all_names = "_startup" :: "_system" :: name_list in
  compile_phrase (Cmm_helpers.frame_table all_names);
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  Emit.end_assembly ()

let make_shared_startup_file ~ppf_dump units =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ();
  List.iter compile_phrase
    (Cmm_helpers.emit_preallocated_blocks [] (* add gc_roots (for dynlink) *)
      (Cmm_helpers.generic_functions true (List.map fst units)));
  compile_phrase (Cmm_helpers.plugin_header units);
  compile_phrase
    (Cmm_helpers.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared file_list output_name =
  let exitcode = Ccomp.call_linker Ccomp.Dll output_name file_list "" in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let obj_infos = List.map read_file objfiles in
    let ldeps = Linkdeps.create ~complete:false in
    let units_tolink = List.fold_right (scan_file ldeps) obj_infos [] in
    (match Linkdeps.check ldeps with
     | None -> ()
     | Some e -> raise (Error (Link_error e)));
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles =
      List.rev (List.filter_map object_file_name_of_file obj_infos) @
      (List.rev !Clflags.ccobjs) in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      (fun () ->
         make_shared_startup_file ~ppf_dump
           (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink)
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list) in
  let files, ldflags =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib (),
      native_ldflags ^ " " ^
      (if !Clflags.nopervasives || (main_obj_runtime && not main_dll)
       then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  let exitcode = Ccomp.call_linker mode output_name files ldflags in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let units_without_stored_code units_to_link =
  assert(Config.flambda);
  List.filter_map (fun (info, _, _) ->
    match info.Cmx_format.ui_export_info with
    | Clambda _ -> assert false
    | Flambda { Export_info.code = None } -> Some info.Cmx_format.ui_name
    | Flambda { Export_info.code = Some _ } -> None)
    units_to_link

let get_flambda_codes units_to_link =
  assert(Config.flambda);
  List.map (fun (info, _, _) ->
    match info.Cmx_format.ui_export_info with
    | Clambda _ -> assert false
    | Flambda { Export_info.code } ->
      match code with
      | None ->
        (* [link] only starts a whole-program rebuild once
           [units_without_stored_code] came back empty. *)
        assert false
      | Some code ->
        code)
    units_to_link

let copy_unit_info unit =
  { unit with ui_name = unit.Cmx_format.ui_name }

let compile_implementation_flambda ~unit_prefix ~backend
  ~ppf_dump (program : Flambda.program) =
  Asmgen.compile_unit
    ~output_prefix:unit_prefix
    ~asm_filename:(unit_prefix ^ ext_asm)
    ~keep_asm:!Clflags.keep_asm_file
    ~obj_filename:(unit_prefix ^ ext_obj)
  (fun () ->
    let clambda_with_constants =
      Flambda_middle_end.flambda_to_clambda ~backend ~ppf_dump program
    in
    Asmgen.end_gen_implementation ~ppf_dump clambda_with_constants)

(* Dead-code-elimination statistics for the -use-lto report.  Functions are
   counted with [Flambda_iterators] over nested and top-level closures alike,
   so the figures are stable across the [Lift_constants] pass.  Sizes use the
   inlining cost model, an architecture-independent proxy for generated-code
   bytes.  Functions are keyed by closure origin, which the cleaning passes
   preserve even as variables are freshened, so a function present before and
   after cleaning is recognised as kept and an eliminated one can be named. *)
type lto_stats = {
  fun_count : int;
  total_size : int;
  by_origin : (int * int) Closure_origin.Map.t;
  (* per origin: number of function declarations and their cumulative size *)
}

let program_stats program =
  let fun_count = ref 0 in
  let total_size = ref 0 in
  let by_origin = ref Closure_origin.Map.empty in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant:_ (set : Flambda.set_of_closures) ->
      Variable.Map.iter
        (fun _fun_var (decl : Flambda.function_declaration) ->
          let size =
            match Inlining_cost.lambda_smaller' decl.body ~than:max_int with
            | Some size -> size
            | None -> 0
          in
          incr fun_count;
          total_size := !total_size + size;
          by_origin :=
            Closure_origin.Map.update decl.closure_origin
              (function
                | None -> Some (1, size)
                | Some (n, s) -> Some (n + 1, s + size))
              !by_origin)
        set.function_decls.funs);
  { fun_count = !fun_count; total_size = !total_size; by_origin = !by_origin }

let unit_of_origin origin =
  Ident.name
    (Compilation_unit.get_persistent_ident
       (Closure_origin.get_compilation_unit origin))

let report_dce ~ppf_dump ~(before : lto_stats) ~(after : lto_stats) =
  let removed = before.fun_count - after.fun_count in
  let removed_size = before.total_size - after.total_size in
  let pct part whole =
    if whole = 0 then 0.
    else 100. *. float_of_int part /. float_of_int whole
  in
  Printf.eprintf
    "-use-lto: kept %d of %d functions, eliminated %d as dead code \
     (%.1f%% of functions, %.1f%% of code size)\n%!"
    after.fun_count before.fun_count removed
    (pct removed before.fun_count) (pct removed_size before.total_size);
  if !Clflags.dump_lto_dce then begin
    let kept origin = Closure_origin.Map.find_opt origin after.by_origin in
    (* Aggregate per originating compilation unit. *)
    let units = Hashtbl.create 42 in
    Closure_origin.Map.iter
      (fun origin (n, size) ->
        let u = unit_of_origin origin in
        let kn, ks = match kept origin with
          | None -> 0, 0
          | Some (kn, ks) -> kn, ks
        in
        let dn, ds, dkn, dks =
          try Hashtbl.find units u with Not_found -> 0, 0, 0, 0
        in
        Hashtbl.replace units u (dn + n, ds + size, dkn + kn, dks + ks))
      before.by_origin;
    let units =
      List.sort (fun (_, (_, s1, _, k1)) (_, (_, s2, _, k2)) ->
          Int.compare (s2 - k2) (s1 - k1))
        (Hashtbl.fold (fun u x acc -> (u, x) :: acc) units [])
    in
    Format.fprintf ppf_dump
      "@.-use-lto dead-code elimination by unit \
       (sizes in inlining-cost units):@.";
    Format.fprintf ppf_dump "  %10s %12s  %s@." "dropped" "kept" "unit";
    List.iter
      (fun (u, (n, s, kn, ks)) ->
        Format.fprintf ppf_dump "  %10d %12s  %s@." (s - ks)
          (Printf.sprintf "%d/%d" kn n) u)
      units;
    let dropped =
      Closure_origin.Map.fold
        (fun origin (_, size) acc ->
          match kept origin with
          | Some _ -> acc
          | None -> (size, origin) :: acc)
        before.by_origin []
    in
    let dropped =
      List.sort (fun (s1, _) (s2, _) -> Int.compare s2 s1) dropped
    in
    Format.fprintf ppf_dump
      "@.-use-lto eliminated functions (size, closure origin):@.";
    List.iter
      (fun (size, origin) ->
        Format.fprintf ppf_dump "  %10d  %a@." size
          Closure_origin.print origin)
      dropped;
    Format.fprintf ppf_dump "@."
  end

(* Retention tracing for -dlto-why-live: rerun the reachability walk the
   cleanup performs, this time remembering for every symbol which construct
   first reached it, and print the chain of retainers for each symbol whose
   linkage name contains the requested substring.  Runs on the cleaned
   program, where every defined symbol is live, so the same roots (the
   program result and the surviving top-level effects) reach them all. *)
let why_live ~ppf_dump ~pattern (program : Flambda.program) =
  let constant_deps (const : Flambda.constant_defining_value) =
    match const with
    | Allocated_const _ -> Symbol.Set.empty
    | Block (_, fields) ->
      Symbol.Set.of_list
        (List.filter_map
           (function
             | (Symbol s : Flambda.constant_defining_value_block_field) ->
               Some s
             | Flambda.Const _ -> None)
           fields)
    | Set_of_closures set ->
      Flambda.free_symbols_named (Set_of_closures set)
    | Project_closure (s, _) -> Symbol.Set.singleton s
  in
  let defs = ref Symbol.Map.empty in
  let add_def sym deps = defs := Symbol.Map.add sym deps !defs in
  let roots = ref Symbol.Set.empty in
  let rec walk (body : Flambda.program_body) =
    match body with
    | Let_symbol (s, def, k) -> add_def s (constant_deps def); walk k
    | Let_rec_symbol (l, k) ->
      List.iter (fun (s, def) -> add_def s (constant_deps def)) l;
      walk k
    | Initialize_symbol (s, _, fields, k) ->
      add_def s
        (List.fold_left
           (fun acc field -> Symbol.Set.union acc (Flambda.free_symbols field))
           Symbol.Set.empty fields);
      walk k
    | Effect (e, k) ->
      roots := Symbol.Set.union (Flambda.free_symbols e) !roots;
      walk k
    | End syms -> roots := Symbol.Set.union syms !roots
  in
  walk program.program_body;
  (* Breadth-first from the roots, recording the first retainer of each
     symbol; first visits give shortest retention chains. *)
  let parent = Symbol.Tbl.create 42 in
  let queue = Queue.create () in
  Symbol.Set.iter
    (fun s ->
      if not (Symbol.Tbl.mem parent s) then begin
        Symbol.Tbl.add parent s None;
        Queue.add s queue
      end)
    !roots;
  while not (Queue.is_empty queue) do
    let s = Queue.take queue in
    match Symbol.Map.find_opt s !defs with
    | None -> ()
    | Some deps ->
      Symbol.Set.iter
        (fun dep ->
          if not (Symbol.Tbl.mem parent dep) then begin
            Symbol.Tbl.add parent dep (Some s);
            Queue.add dep queue
          end)
        deps
  done;
  let name s = Linkage_name.to_string (Symbol.label s) in
  let matches s =
    let sym = name s and pat = pattern in
    let sl = String.length sym and pl = String.length pat in
    let rec at i = i + pl <= sl
      && (String.equal (String.sub sym i pl) pat || at (i + 1))
    in
    pl > 0 && at 0
  in
  Format.fprintf ppf_dump "@.-use-lto why-live %S:@." pattern;
  let found = ref false in
  Symbol.Map.iter
    (fun s _ ->
      if matches s then begin
        found := true;
        Format.fprintf ppf_dump "  %s@." (name s);
        let rec chain s =
          match Symbol.Tbl.find parent s with
          | None -> Format.fprintf ppf_dump "    <- kept by a root \
              (the program result or a top-level effect)@."
          | Some p -> Format.fprintf ppf_dump "    <- %s@." (name p); chain p
          | exception Not_found ->
            (* Unreachable from the roots: only referenced from function
               bodies, e.g. a direct call.  Code references do not show in
               the symbol graph; the caller is in the -dlto-dce listing. *)
            Format.fprintf ppf_dump "    <- referenced directly from code \
              (not through a symbol definition)@."
        in
        chain s
      end)
    !defs;
  if not !found then
    Format.fprintf ppf_dump "  (no defined symbol matches)@.";
  Format.fprintf ppf_dump "@."

let link_whole_program ~backend ~ppf_dump ~crc_interfaces units_to_link =
  let codes = get_flambda_codes units_to_link in
  let program =
    Flambda_utils.clear_all_exported_symbols
      (Flambda_utils.concatenate codes)
  in
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent "_link_")
      (Linkage_name.create "_link_");
  in
  Compilation_unit.set_current compilation_unit;
  let program = Flambda_utils.replace_compilation_unit_of_symbols compilation_unit program in
  (* Static exceptions in the concatenated program keep the numbering of
     their producing compilations, each of which counted from zero -- as
     does this process's [Lambda.next_raise_count], from which the
     simplifier's freshening renames the static exceptions of every body it
     duplicates.  A minted id equal to a deserialised one puts two unrelated
     catches with the same number into one scope tree, and the next
     duplication that renames either of them captures or orphans the raises
     of the other, since the freshening map is keyed by the id.  Move the
     counter past every deserialised id before any simplification runs. *)
  let () =
    let max_id = ref 0 in
    let note (expr : Flambda.t) =
      match expr with
      | Static_raise (i, _) | Static_catch (i, _, _, _) ->
        let i = Static_exception.to_int i in
        if i > !max_id then max_id := i
      | _ -> ()
    in
    Flambda_iterators.iter_exprs_at_toplevel_of_program program
      ~f:(fun expr -> Flambda_iterators.iter note (fun _ -> ()) expr);
    Lambda.ensure_raise_count !max_id
  in
  let stats_before = program_stats program in
  (* No [Flambda_invariants.check_exn] here: the concatenated program mixes
     variables from several compilation units (their [Set_of_closures_id] etc.
     deliberately keep their originating unit, which [Flambda_to_clambda] relies
     on), so it does not satisfy the single-unit invariant. *)
  if !Clflags.dump_rawflambda then
    Format.fprintf ppf_dump "After concatenation:@ %a@."
      Flambda.print_program program;
  let cleaned_program =
    Remove_unused_program_constructs.remove_unused_program_constructs program
  in
  let cleaned_program =
    Inline_and_simplify.run
      ~never_inline:true
      ~ppf_dump
      ~backend
      ~prefixname:"_link_"
      ~round:0
      cleaned_program
  in
  let cleaned_program = Lift_constants.lift_constants ~backend cleaned_program in
  let cleaned_program = Share_constants.share_constants cleaned_program in
  let cleaned_program = Remove_unused_program_constructs.remove_unused_program_constructs cleaned_program in
  if !Clflags.dump_flambda then
    Format.fprintf ppf_dump "After cleaning:@ %a@."
      Flambda.print_program cleaned_program;
  Compilenv.reset "_link_";
  let unit_prefix = Filename.temp_file "caml_link" "" in
  let program_body =
    let open Flambda in
    Let_symbol (Compilenv.current_unit_symbol (), Block (Tag.create_exn 0, []),
                cleaned_program.program_body) in
  let cleaned_program = { cleaned_program with program_body } in
  let stats_after = program_stats cleaned_program in
  report_dce ~ppf_dump ~before:stats_before ~after:stats_after;
  (match !Clflags.lto_why_live with
   | None -> ()
   | Some pattern -> why_live ~ppf_dump ~pattern cleaned_program);
  compile_implementation_flambda
    ~unit_prefix
    ~backend
    ~ppf_dump
    cleaned_program;
  (* This cmx file is never written. *)
  let unit_filename = unit_prefix ^ ".cmx" in
  let object_filename = unit_prefix ^ ext_obj in
  (* cmx information are mutable, we need to copy them
     to prevent clobering from startup compilation. *)
  let unit_infos = copy_unit_info (Compilenv.current_unit_infos ()) in
  (* This synthetic cmx is never written and its CRC is never read: the startup
     file below is built with [~no_global_map:true], which is what would consume
     it. A -use-lto executable therefore has an empty globals map, so [Dynlink]
     cannot see the statically linked units; that is fine for whole-program
     targets (unikernels), which do not dynlink. *)
  let digest = "----------------" in
  let single_unit = [unit_infos, unit_filename, digest] in
  (* [unit_prefix] is an empty temp file created by [Filename.temp_file]; remove
     it along with the object we assembled. *)
  [object_filename; unit_prefix], [object_filename],
  (fun () -> make_startup_file ~ppf_dump ~no_global_map:true ~crc_interfaces single_unit)

(* Main entry point *)

let compile_startup_and_call_linker
    ~removed_objects ~object_files ~output_name
    ~make_startup =
  let startup =
    if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
    then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm in
  let startup_obj = Filename.temp_file "camlstartup" ext_obj in
  Asmgen.compile_unit ~output_prefix:output_name
    ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
    ~obj_filename:startup_obj
    make_startup;
  Misc.try_finally
    (fun () ->
      call_linker object_files startup_obj output_name)
    ~always:(fun () ->
       remove_file startup_obj;
       List.iter remove_file removed_objects)

let link ~backend ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let obj_infos = List.map read_file objfiles in
    let ldeps = Linkdeps.create ~complete:true in
    let units_tolink = List.fold_right (scan_file ldeps) obj_infos [] in
    (match Linkdeps.check ldeps with
     | None -> ()
     | Some e -> raise (Error (Link_error e)));
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    let crc_interfaces = extract_crc_interfaces () in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let removed_objects, object_files, make_startup =
      let whole_program_rebuild =
        !Clflags.whole_program_rebuild && Config.flambda
        && (match units_without_stored_code units_tolink with
            | [] -> true
            | missing ->
                (* The only point where -use-lto actually meets each module:
                   report the ones that cannot take part and link normally.
                   [-warn-error +76] turns the fallback into a failure. *)
                List.iter
                  (fun name ->
                     Location.prerr_warning Location.none
                       (Warnings.Module_compiled_without_lto name))
                  missing;
                false)
      in
      if whole_program_rebuild then
        link_whole_program ~backend ~ppf_dump ~crc_interfaces units_tolink
      else
        [],
        List.filter_map object_file_name_of_file obj_infos,
        (fun () ->
           make_startup_file ~ppf_dump ~no_global_map:false ~crc_interfaces
             units_tolink)
    in
    compile_startup_and_call_linker
      ~removed_objects
      ~object_files
      ~output_name
      ~make_startup
  )

(* Error report *)

module Style = Misc.Style
open Format_doc

let report_error_doc ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %a" Style.inline_code name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.Doc.quoted_filename name
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %a@]"
       Location.Doc.quoted_filename file1
       Location.Doc.quoted_filename file2
       Style.inline_code intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %a@]"
       Location.Doc.quoted_filename file1
       Location.Doc.quoted_filename file2
       Style.inline_code intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a"
        Location.Doc.quoted_filename file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the %a file@ for module %a,@ \
         which was produced by %a.@ \
         Please recompile %a@ with the correct %a option@ \
         so that %a@ is found.@]"
        Location.Doc.quoted_filename filename
        Style.inline_code ".cmx"
        Style.inline_code name
        Style.inline_code "ocamlopt -for-pack"
        Location.Doc.quoted_filename filename
        Style.inline_code "-I"
        Style.inline_code (name^".cmx")
  | Link_error e ->
      Linkdeps.report_error_doc ~print_filename:Location.Doc.filename ppf e

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  cmx_required := [];
  interfaces := [];
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []
