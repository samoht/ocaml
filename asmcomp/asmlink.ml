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
  if !Clflags.runtime_variant = "_shared" then
    if Config.suffixing then
      [Misc.RuntimeID.shared_runtime Sys.Native]
    else
      ["-lasmrun_shared"]
  else
    let libname = "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
    try
      if !Clflags.nopervasives || not !Clflags.with_runtime then []
      else [ Load_path.find libname ]
    with Not_found ->
      raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

type file =
  | Unit of string * unit_infos * Digest.BLAKE128.t
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
  let need_stdlib =
    let needs_stdlib ({ui_need_stdlib; _}, _, _) = ui_need_stdlib in
    List.exists needs_stdlib units_list
  in
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
  if need_stdlib then begin
    let standard_library_default =
      Option.value ~default:Config.standard_library_default
                   !Clflags.standard_library_default in
    compile_phrase
      (Cmm_helpers.emit_global_string_constant
        "caml_standard_library_nat" standard_library_default)
  end;
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

(* Whole-program purity through calls.

   [Effect_analysis] must treat every [Apply] as effectful because a single
   unit cannot see callee bodies, so a module initializer computed by a
   function call ([let cli = make_cli ()]) is unremovable even when nothing
   reads it.  At the -use-lto link the whole call graph is present:
   [expr_no_effects] mirrors [Effect_analysis.no_effects] but resolves
   direct calls through a set of known-pure functions, and
   [compute_pure_functions] builds that set by monotone growth from the
   pessimistic start.  Starting empty keeps every (mutually) recursive
   function impure, which is the sound choice for termination: an
   initializer that diverges must not be removed.  Indirect calls, sends,
   and loops remain impure, as in the base analysis. *)
let rec expr_no_effects ~pure (expr : Flambda.t) =
  match expr with
  | Var _ | Proved_unreachable -> true
  | Apply { kind = Direct closure_id; _ } ->
    Closure_id.Set.mem closure_id pure
  | Apply { kind = Indirect; _ } -> false
  | Let { defining_expr; body; _ } ->
    named_no_effects ~pure defining_expr && expr_no_effects ~pure body
  | Let_mutable { body; _ } -> expr_no_effects ~pure body
  | If_then_else (_, ifso, ifnot) ->
    expr_no_effects ~pure ifso && expr_no_effects ~pure ifnot
  | Switch (_, sw) ->
    List.for_all (fun (_, e) -> expr_no_effects ~pure e) sw.consts
    && List.for_all (fun (_, e) -> expr_no_effects ~pure e) sw.blocks
    && Option.fold ~some:(expr_no_effects ~pure) ~none:true sw.failaction
  | String_switch (_, sw, def) ->
    List.for_all (fun (_, e) -> expr_no_effects ~pure e) sw
    && Option.fold ~some:(expr_no_effects ~pure) ~none:true def
  | Static_catch (_, _, body, _) | Try_with (body, _, _) ->
    (* As in [Effect_analysis]: a raise in [body] makes the whole
       expression effectful, so the handler need not be examined. *)
    expr_no_effects ~pure body
  | While _ | For _ | Send _ | Assign _ | Static_raise _ -> false

and named_no_effects ~pure (named : Flambda.named) =
  match named with
  | Expr e -> expr_no_effects ~pure e
  | named -> Effect_analysis.no_effects_named named

let compute_pure_functions program =
  let bodies = ref Closure_id.Map.empty in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant:_ (set : Flambda.set_of_closures) ->
      Variable.Map.iter
        (fun fun_var (decl : Flambda.function_declaration) ->
          bodies :=
            Closure_id.Map.add (Closure_id.wrap fun_var) decl.body !bodies)
        set.function_decls.funs);
  let bodies = !bodies in
  let rec grow pure =
    let pure' =
      Closure_id.Map.fold
        (fun closure_id body acc ->
          if Closure_id.Set.mem closure_id acc then acc
          else if expr_no_effects ~pure:acc body then
            Closure_id.Set.add closure_id acc
          else acc)
        bodies pure
    in
    if Closure_id.Set.cardinal pure' = Closure_id.Set.cardinal pure then pure
    else grow pure'
  in
  grow Closure_id.Set.empty

(* How each symbol is used by the rest of the program: the set of field
   indices read from it, and whether it escapes as a first-class value
   (any occurrence other than a constant-index [Read_symbol_field]:
   [Symbol] in code, a constant block field, a [Project_closure], or being
   a program result).  Runtime reads of a block field require the block as
   a value first, so a non-escaping symbol's unread fields are provably
   never consumed. *)
let symbol_uses (program : Flambda.program) =
  let escaped = ref Symbol.Set.empty in
  let reads = ref Symbol.Map.empty in
  let escape s = escaped := Symbol.Set.add s !escaped in
  let read s i =
    reads :=
      Symbol.Map.update s
        (function
          | None -> Some (Numbers.Int.Set.singleton i)
          | Some set -> Some (Numbers.Int.Set.add i set))
        !reads
  in
  Flambda_iterators.iter_named_of_program program
    ~f:(function
      | Symbol s -> escape s
      | Read_symbol_field (s, i) -> read s i
      | _ -> ());
  Flambda_iterators.iter_constant_defining_values_on_program program
    ~f:(function
      | Block (_, fields) ->
        List.iter
          (function
            | (Symbol s : Flambda.constant_defining_value_block_field) ->
              escape s
            | Flambda.Const _ -> ())
          fields
      | Project_closure (s, _) -> escape s
      | Allocated_const _ | Set_of_closures _ -> ());
  let rec ends (body : Flambda.program_body) =
    match body with
    | Let_symbol (_, _, k) | Let_rec_symbol (_, k)
    | Initialize_symbol (_, _, _, k) | Effect (_, k) -> ends k
    | End syms -> Symbol.Set.iter escape syms
  in
  ends program.program_body;
  !escaped, !reads

(* Remove initialization work whose result is never consumed: fields of
   non-escaping module blocks that no [Read_symbol_field] mentions are
   replaced by a constant when their computation is pure (per
   [expr_no_effects], so calls to pure functions count), and [Effect]
   constructs that are pure under the same analysis are dropped.  The
   references thus removed let the next cleanup round collect the closures
   they kept alive. *)
let remove_pure_initializers ~pure (program : Flambda.program) =
  let escaped, reads = symbol_uses program in
  let field_read s i =
    match Symbol.Map.find_opt s reads with
    | None -> false
    | Some set -> Numbers.Int.Set.mem i set
  in
  let dummy () =
    Flambda_utils.name_expr (Const (Int 0))
      ~name:Internal_variable_names.const_zero
  in
  let rec walk (body : Flambda.program_body) : Flambda.program_body =
    match body with
    | Let_symbol (s, Block (tag, fields), k)
      when not (Symbol.Set.mem s escaped) ->
      let fields =
        List.mapi
          (fun i (field : Flambda.constant_defining_value_block_field) ->
            match field with
            | Symbol _ when not (field_read s i) ->
              (Const (Int 0) : Flambda.constant_defining_value_block_field)
            | field -> field)
          fields
      in
      Let_symbol (s, Block (tag, fields), walk k)
    | Let_symbol (s, def, k) -> Let_symbol (s, def, walk k)
    | Let_rec_symbol (l, k) -> Let_rec_symbol (l, walk k)
    | Initialize_symbol (s, tag, fields, k)
      when not (Symbol.Set.mem s escaped) ->
      let fields =
        List.mapi
          (fun i field ->
            if field_read s i || not (expr_no_effects ~pure field) then field
            else dummy ())
          fields
      in
      Initialize_symbol (s, tag, fields, walk k)
    | Initialize_symbol (s, tag, fields, k) ->
      Initialize_symbol (s, tag, fields, walk k)
    | Effect (e, k) when expr_no_effects ~pure e -> walk k
    | Effect (e, k) -> Effect (e, walk k)
    | End _ as body -> body
  in
  { program with program_body = walk program.program_body }

(* Link-time evaluation of module initializers ("partial evaluation").

   A fuel-bounded interpreter for the effect-free fragment of Flambda
   evaluates [Initialize_symbol] field computations at link time: constants,
   blocks, immutable strings and floats, arithmetic, matches (including
   static catches), local mutable state ([Let_mutable]/[Assign], invisible
   from outside), and direct calls -- through recursion, since a [Direct]
   apply needs only the callee's body, never the closure value.  Anything
   else (indirect calls, heap effects, closures or mutable data as results,
   out-of-range accesses, exhausted fuel) aborts that field, which then
   keeps its runtime computation.

   Successful evaluations are residualized as constant expression trees;
   the subsequent [Lift_constants]/[Share_constants] turn those into static
   data and [Initialize_symbol_to_let_symbol] converts fully-evaluated
   blocks into [Let_symbol], so the work is done once at link time and
   shipped as preinitialized memory instead of startup code.  Evaluation
   assumes host and target integers agree, so it is disabled when
   [Targetint.size] differs from the host's word size. *)

type preeval_value =
  | VInt of int
  | VFloat of float
  | VString of string
  | VBlock of int * preeval_value array
  | VOpaque
  (** A value that may be bound but never consumed: a closure or an
      arbitrary symbol.  A [Direct] apply does not consume the callee
      closure, so [let f = project_closure ... in f x y] evaluates; any
      real consumption of a [VOpaque] gives up. *)

exception Preeval_give_up
exception Preeval_static_exit of Static_exception.t * preeval_value list

let preeval_fuel = 100_000
let preeval_max_nodes = 5_000

let function_bodies program =
  let bodies = ref Closure_id.Map.empty in
  Flambda_iterators.iter_on_set_of_closures_of_program program
    ~f:(fun ~constant:_ (set : Flambda.set_of_closures) ->
      Variable.Map.iter
        (fun fun_var (decl : Flambda.function_declaration) ->
          bodies :=
            Closure_id.Map.add (Closure_id.wrap fun_var) decl !bodies)
        set.function_decls.funs);
  !bodies

let preeval_field ~bodies ~sym_env expr =
  let fuel = ref preeval_fuel in
  let step () =
    decr fuel;
    if !fuel <= 0 then raise Preeval_give_up
  in
  let as_int = function VInt n -> n | _ -> raise Preeval_give_up in
  let find env v =
    match Variable.Map.find_opt v env with
    | Some value -> value
    | None -> raise Preeval_give_up
  in
  let eval_prim (prim : Clambda_primitives.primitive) args =
    match prim, args with
    | Pmakeblock (tag, Immutable, _), _ ->
      VBlock (tag, Array.of_list args)
    | Pfield (i, _, _), [VBlock (_, elems)] when i >= 0 && i < Array.length elems ->
      elems.(i)
    | Pnegint, [a] -> VInt (- (as_int a))
    | Paddint, [a; b] -> VInt (as_int a + as_int b)
    | Psubint, [a; b] -> VInt (as_int a - as_int b)
    | Pmulint, [a; b] -> VInt (as_int a * as_int b)
    | Pdivint _, [a; b] when as_int b <> 0 -> VInt (as_int a / as_int b)
    | Pmodint _, [a; b] when as_int b <> 0 -> VInt (as_int a mod as_int b)
    | Pandint, [a; b] -> VInt (as_int a land as_int b)
    | Porint, [a; b] -> VInt (as_int a lor as_int b)
    | Pxorint, [a; b] -> VInt (as_int a lxor as_int b)
    | Plslint, [a; b] when as_int b >= 0 && as_int b < 63 ->
      VInt (as_int a lsl as_int b)
    | Plsrint, [a; b] when as_int b >= 0 && as_int b < 63 ->
      VInt (as_int a lsr as_int b)
    | Pasrint, [a; b] when as_int b >= 0 && as_int b < 63 ->
      VInt (as_int a asr as_int b)
    | Poffsetint n, [a] -> VInt (as_int a + n)
    | Pintcomp c, [VInt a; VInt b] ->
      let r =
        match c with
        | Ceq -> a = b | Cne -> a <> b
        | Clt -> a < b | Cgt -> a > b
        | Cle -> a <= b | Cge -> a >= b
      in
      VInt (if r then 1 else 0)
    | Pnot, [a] -> VInt (if as_int a = 0 then 1 else 0)
    | Psequand, [a; b] -> VInt (if as_int a <> 0 && as_int b <> 0 then 1 else 0)
    | Psequor, [a; b] -> VInt (if as_int a <> 0 || as_int b <> 0 then 1 else 0)
    | Pisint, [VInt _] -> VInt 1
    | Pisint, [(VBlock _ | VString _ | VFloat _)] -> VInt 0
    | Pisout, [h; x] ->
      let h = as_int h and x = as_int x in
      VInt (if x < 0 || x > h then 1 else 0)
    | Pstringlength, [VString s] -> VInt (String.length s)
    | (Pstringrefu | Pstringrefs), [VString s; VInt i]
      when i >= 0 && i < String.length s ->
      VInt (Char.code s.[i])
    | _ -> raise Preeval_give_up
  in
  let rec eval env mut (expr : Flambda.t) =
    step ();
    match expr with
    | Var v -> find env v
    | Let { var; defining_expr; body; _ } ->
      let value = eval_named env mut defining_expr in
      eval (Variable.Map.add var value env) mut body
    | Let_mutable { var; initial_value; contents_kind = _; body } ->
      let cell = ref (find env initial_value) in
      eval env (Mutable_variable.Map.add var cell mut) body
    | Assign { being_assigned; new_value } ->
      (match Mutable_variable.Map.find_opt being_assigned mut with
       | Some cell -> cell := find env new_value; VInt 0
       | None -> raise Preeval_give_up)
    | If_then_else (cond, ifso, ifnot) ->
      if as_int (find env cond) <> 0 then eval env mut ifso
      else eval env mut ifnot
    | Switch (arg, sw) ->
      let branch =
        match find env arg with
        | VInt n -> List.assoc_opt n sw.consts
        | VBlock (tag, _) -> List.assoc_opt tag sw.blocks
        | _ -> raise Preeval_give_up
      in
      (match branch, sw.failaction with
       | Some e, _ -> eval env mut e
       | None, Some e -> eval env mut e
       | None, None -> raise Preeval_give_up)
    | String_switch (arg, cases, default) ->
      (match find env arg with
       | VString s ->
         (match List.assoc_opt s cases, default with
          | Some e, _ -> eval env mut e
          | None, Some e -> eval env mut e
          | None, None -> raise Preeval_give_up)
       | _ -> raise Preeval_give_up)
    | Static_raise (exn, args) ->
      raise (Preeval_static_exit (exn, List.map (find env) args))
    | Static_catch (exn, vars, body, handler) ->
      (try eval env mut body with
       | Preeval_static_exit (exn', values)
         when Static_exception.equal exn exn' ->
         (match
            List.fold_left2
              (fun env (var, _kind) value -> Variable.Map.add var value env)
              env vars values
          with
          | env -> eval env mut handler
          | exception Invalid_argument _ -> raise Preeval_give_up))
    | While (cond, body) ->
      while as_int (eval env mut cond) <> 0 do
        ignore (eval env mut body : preeval_value)
      done;
      VInt 0
    | For { bound_var; from_value; to_value; direction; body } ->
      let lo = as_int (find env from_value)
      and hi = as_int (find env to_value) in
      let iter i = ignore (eval (Variable.Map.add bound_var (VInt i) env) mut body : preeval_value) in
      (match direction with
       | Upto -> for i = lo to hi do iter i done
       | Downto -> for i = lo downto hi do iter i done);
      VInt 0
    | Apply { func = _; args; kind = Direct closure_id; _ } ->
      (match Closure_id.Map.find_opt closure_id bodies with
       | None -> raise Preeval_give_up
       | Some (decl : Flambda.function_declaration) ->
         let args = List.map (find env) args in
         (match
            List.fold_left2
              (fun env param value ->
                Variable.Map.add (Parameter.var param) value env)
              Variable.Map.empty decl.params args
          with
          | env -> eval env Mutable_variable.Map.empty decl.body
          | exception Invalid_argument _ -> raise Preeval_give_up))
    | Apply { kind = Indirect; _ } | Send _ | Try_with _
    | Proved_unreachable -> raise Preeval_give_up
  and eval_named env mut (named : Flambda.named) =
    step ();
    match named with
    | Const (Int n) -> VInt n
    | Const (Char c) -> VInt (Char.code c)
    | Allocated_const (Immutable_string s) -> VString s
    | Allocated_const (Float f) -> VFloat f
    | Read_mutable var ->
      (match Mutable_variable.Map.find_opt var mut with
       | Some cell -> !cell
       | None -> raise Preeval_give_up)
    | Read_symbol_field (s, i) ->
      (match Symbol.Map.find_opt s sym_env with
       | Some fields when i >= 0 && i < Array.length fields ->
         (match fields.(i) with
          | Some value -> value
          | None -> raise Preeval_give_up)
       | _ -> raise Preeval_give_up)
    | Prim (prim, args, _) ->
      eval_prim prim (List.map (find env) args)
    | Expr e -> eval env mut e
    | Symbol _ | Set_of_closures _ | Project_closure _
    | Project_var _ | Move_within_set_of_closures _ -> VOpaque
    | Allocated_const _ -> raise Preeval_give_up
  in
  let rec count_nodes v =
    match v with
    | VInt _ | VFloat _ | VString _ -> 1
    | VOpaque ->
      (* An opaque value cannot be residualized. *)
      raise Preeval_give_up
    | VBlock (_, elems) ->
      Array.fold_left (fun acc e -> acc + count_nodes e) 1 elems
  in
  try
    let value = eval Variable.Map.empty Mutable_variable.Map.empty expr in
    if count_nodes value <= preeval_max_nodes then Some value else None
  with Preeval_give_up | Preeval_static_exit _ -> None

(* [preeval_value] back to Flambda: a tree of lets ending in the value's
   variable, which [Lift_constants] then hoists to static data. *)
let rec preeval_bind value (k : Variable.t -> Flambda.t) : Flambda.t =
  match value with
  | VOpaque -> assert false  (* excluded by [count_nodes] before residualizing *)
  | VInt n ->
    let var = Variable.create Internal_variable_names.const_int in
    Flambda.create_let var (Const (Int n)) (k var)
  | VFloat f ->
    let var = Variable.create Internal_variable_names.const_float in
    Flambda.create_let var (Allocated_const (Float f)) (k var)
  | VString s ->
    let var = Variable.create Internal_variable_names.const_string in
    Flambda.create_let var (Allocated_const (Immutable_string s)) (k var)
  | VBlock (tag, elems) ->
    let rec bind_elems acc = function
      | [] ->
        let var = Variable.create Internal_variable_names.const_block in
        Flambda.create_let var
          (Prim (Pmakeblock (tag, Immutable, None), List.rev acc,
                 Debuginfo.none))
          (k var)
      | elem :: rest ->
        preeval_bind elem (fun var -> bind_elems (var :: acc) rest)
    in
    bind_elems [] (Array.to_list elems)

let preeval_residual value =
  preeval_bind value (fun var -> Flambda.Var var)

let preeval_initializers (program : Flambda.program) =
  if Targetint.size <> Sys.word_size then program
  else begin
    let bodies = function_bodies program in
    let sym_env = ref Symbol.Map.empty in
    let record_symbol s fields =
      sym_env := Symbol.Map.add s (Array.of_list fields) !sym_env
    in
    let rec walk (body : Flambda.program_body) : Flambda.program_body =
      match body with
      | Let_symbol (s, (Block (_, fields) as def), k) ->
        record_symbol s
          (List.map
             (function
               | (Flambda.Const (Int n)
                   : Flambda.constant_defining_value_block_field) ->
                 Some (VInt n)
               | Flambda.Const (Char c) -> Some (VInt (Char.code c))
               | Flambda.Symbol _ -> None)
             fields);
        Let_symbol (s, def, walk k)
      | Let_symbol (s, def, k) -> Let_symbol (s, def, walk k)
      | Let_rec_symbol (l, k) -> Let_rec_symbol (l, walk k)
      | Initialize_symbol (s, tag, fields, k) ->
        let values_and_fields =
          List.map
            (fun field ->
              match preeval_field ~bodies ~sym_env:!sym_env field with
              | Some value -> Some value, preeval_residual value
              | None -> None, field)
            fields
        in
        record_symbol s (List.map fst values_and_fields);
        Initialize_symbol (s, tag, List.map snd values_and_fields, walk k)
      | Effect (e, k) -> Effect (e, walk k)
      | End _ as body -> body
    in
    { program with program_body = walk program.program_body }
  end

(* Always-on specialisation of the stdlib's format interpreters.

   printf retention is the interpretive [make_printf] web: far too large for
   stdlib to export as inlinable, so no per-unit flag can specialise it --
   but the -use-lto link holds every body, and one simplification round with
   inlining enabled and aggressive budgets is demonstrated to unroll the
   interpreter over each statically known format literal into straight-line
   output code, after which the whole web is collected as dead.

   Whether that round runs is decided by counting the program's entry calls
   into the format machinery that carry a statically known constant (the
   format literal, in practice): specialisation multiplies code per entry
   site while the shared interpreter is paid once, so it runs only when
   there is at least one such site and few enough that killing the
   interpreter is worth the copies.  Programs that never touch the format
   machinery are unaffected, and format-heavy programs (an ocamlc-sized
   link has hundreds of sites) keep today's behaviour.  A call on a dynamic
   format has no constant argument and does not count. *)

let format_machinery_units =
  [ "CamlinternalFormat"; "CamlinternalFormatBasics";
    "Stdlib__Printf"; "Stdlib__Format"; "Stdlib__Scanf" ]

let format_specialise_max_sites = 50

(* The aggressive budgets used for format specialisation run the inliner over
   the whole linked program, not just the format machinery.  A low site count
   alone is therefore not a sufficient bound: a large application with one
   format literal can still expand by orders of magnitude before the size
   guard gets a chance to discard the result. *)
let format_specialise_max_program_size = 250_000

let count_format_entry_sites (program : Flambda.program) =
  let const_vars = ref Variable.Set.empty in
  let aliases = ref [] in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program
    ~f:(fun expr ->
      Flambda_iterators.iter_all_immutable_let_bindings expr
        ~f:(fun var (named : Flambda.named) ->
          match named with
          | Symbol _ | Const _ | Allocated_const _ | Read_symbol_field _ ->
            const_vars := Variable.Set.add var !const_vars
          | Expr (Var alias_of) -> aliases := (var, alias_of) :: !aliases
          | Prim (Pfield _, [alias_of], _) ->
            (* A field of a constant is a constant: the wrappers destructure
               the format6 block before handing the bare format inward. *)
            aliases := (var, alias_of) :: !aliases
          | _ -> ()));
  let rec close_aliases () =
    let changed = ref false in
    List.iter
      (fun (var, alias_of) ->
        if Variable.Set.mem alias_of !const_vars
        && not (Variable.Set.mem var !const_vars) then begin
          const_vars := Variable.Set.add var !const_vars;
          changed := true
        end)
      !aliases;
    if !changed then close_aliases ()
  in
  close_aliases ();
  let unit_name_of compilation_unit =
    Ident.name (Compilation_unit.get_persistent_ident compilation_unit)
  in
  let in_machinery name = List.mem name format_machinery_units in
  let sites = ref 0 in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program
    ~f:(fun expr ->
      Flambda_iterators.iter
        (fun (expr : Flambda.t) ->
          match expr with
          | Apply { func; kind = Direct closure_id; args;
                    inline = Default_inline; _ }
            when in_machinery
                   (unit_name_of (Closure_id.get_compilation_unit closure_id))
              && not (in_machinery
                        (unit_name_of (Variable.get_compilation_unit func)))
              && List.exists (fun v -> Variable.Set.mem v !const_vars) args ->
            incr sites
          | _ -> ())
        (fun _ -> ())
        expr);
  !sites

(* Field-aware single-use collapse.

   After symbol lifting, every function's closure escapes through symbols,
   so the inliner's only_use_of_function heuristic can never fire at the
   link even for a function with one reachable call site.  Recover it
   structurally: a closure applied directly at exactly one site, whose
   closure value is only ever let-bound to feed such applies, is marked
   Always_inline at that site.  The zero-budget round then moves the body
   into the caller (size-neutral: the projection that fed the call becomes
   dead, the block field that fed the projection is dummied by the purity
   pass, and the original declaration dies on the next cleanup round), and
   the constants of the call site fold through the moved body -- a backend
   wrapper chain applied once to a literal device list collapses one layer
   per round, taking gated cones with it.  Explicit [@inline never],
   self-recursive functions, closures also referenced as values, and
   callees that declare their own sets of closures are left alone; an
   indirect call missed by the count only costs a duplicated body until
   the field-level cleanup catches up, never correctness.

   Only size-neutral moves are marked here, so the zero-budget cleanup
   cannot grow the program.  Unrolling a recursive callee over a literal
   argument is [-lto-inline]'s job, driven by the ordinary inliner's
   budgets. *)

let mark_single_use_applies (program : Flambda.program) =
  let bodies = function_bodies program in
  let closure_syms = ref Symbol.Map.empty in
  let rec collect_syms (body : Flambda.program_body) =
    match body with
    | Let_symbol (s, Project_closure (_, cid), k) ->
      closure_syms := Symbol.Map.add s cid !closure_syms; collect_syms k
    | Let_symbol (_, _, k) | Initialize_symbol (_, _, _, k)
    | Effect (_, k) -> collect_syms k
    | Let_rec_symbol (l, k) ->
      List.iter
        (function
          | (s, Flambda.Project_closure (_, cid)) ->
            closure_syms := Symbol.Map.add s cid !closure_syms
          | _ -> ())
        l;
      collect_syms k
    | End _ -> ()
  in
  collect_syms program.program_body;
  let apply_count = ref Closure_id.Map.empty in
  let bound_to = ref Variable.Map.empty in
  let value_use = ref Variable.Map.empty in
  let bump m k =
    m := Variable.Map.update k
        (function None -> Some 1 | Some n -> Some (n + 1)) !m
  in
  let use_var v = bump value_use v in
  let visit_expr (expr : Flambda.t) =
    match expr with
    | Apply { func; args; kind; _ } ->
      List.iter use_var args;
      (match kind with
       | Direct cid ->
         apply_count :=
           Closure_id.Map.update cid
             (function None -> Some 1 | Some n -> Some (n + 1)) !apply_count;
         (* [func] counted separately below as a func-position use. *)
         bump value_use func;
         value_use := Variable.Map.update func
             (function Some n -> Some (n - 1) | None -> None) !value_use
       | Indirect -> use_var func)
    | Var v -> use_var v
    | Assign { new_value; _ } -> use_var new_value
    | If_then_else (v, _, _) | Switch (v, _) | String_switch (v, _, _) ->
      use_var v
    | Static_raise (_, vs) -> List.iter use_var vs
    | Send { meth; obj; args; _ } ->
      use_var meth; use_var obj; List.iter use_var args
    | For { from_value; to_value; _ } -> use_var from_value; use_var to_value
    | Let _ | Let_mutable _ | Static_catch _ | Try_with _ | While _
    | Proved_unreachable -> ()
  and visit_named (named : Flambda.named) =
    match named with
    | Prim (_, vs, _) -> List.iter use_var vs
    | Expr _ | Symbol _ | Const _ | Allocated_const _ | Read_mutable _
    | Read_symbol_field _ | Set_of_closures _ | Project_closure _
    | Project_var _ | Move_within_set_of_closures _ -> ()
  in
  let record_binding var (named : Flambda.named) =
    match named with
    | Symbol s ->
      (match Symbol.Map.find_opt s !closure_syms with
       | Some cid -> bound_to := Variable.Map.add var cid !bound_to
       | None -> ())
    | Project_closure { closure_id; _ } ->
      bound_to := Variable.Map.add var closure_id !bound_to
    | _ -> ()
  in
  Flambda_iterators.iter_exprs_at_toplevel_of_program program
    ~f:(fun expr ->
      Flambda_iterators.iter visit_expr visit_named expr;
      Flambda_iterators.iter_all_immutable_let_bindings expr
        ~f:record_binding);
  let self_applies cid =
    match Closure_id.Map.find_opt cid bodies with
    | None -> max_int
    | Some (decl : Flambda.function_declaration) ->
      let n = ref 0 in
      Flambda_iterators.iter
        (fun (e : Flambda.t) ->
          match e with
          | Apply { kind = Direct cid'; _ }
            when Closure_id.equal cid cid' -> incr n
          | _ -> ())
        (fun _ -> ())
        decl.body;
      !n
  in
  let self_recursive cid = self_applies cid > 0 in
  (* Moving a body that declares its own set of closures duplicates the
     declaration under fresh closure ids.  When such a closure escapes the
     callee -- the generative-injector shape [let inj x = M.V x in
     (inj, proj)] returns its closures -- other units were compiled
     against the original ids: their bodies hold [Project_var]s naming
     closure ids that the renamed copy does not answer to, and the
     whole-program simplifier faults on the mismatch.  Per-unit
     compilation cannot hit this (export info is final once a consumer
     compiles against it); only the link re-optimizes a producer out from
     under an already-compiled consumer.  Leave such callees where they
     are declared. *)
  let declares_closures cid =
    match Closure_id.Map.find_opt cid bodies with
    | None -> true
    | Some (decl : Flambda.function_declaration) ->
      let found = ref false in
      Flambda_iterators.iter
        (fun (_ : Flambda.t) -> ())
        (fun (named : Flambda.named) ->
          match named with
          | Set_of_closures _ -> found := true
          | _ -> ())
        decl.body;
      !found
  in
  let eligible cid =
    (match Closure_id.Map.find_opt cid !apply_count with
     | Some 1 -> true | _ -> false)
    && (match Closure_id.Map.find_opt cid bodies with
        | Some decl ->
          (match decl.inline with
           | Never_inline -> false
           | Default_inline | Always_inline | Hint_inline | Unroll _ -> true)
          && not decl.stub
        | None -> false)
    && Variable.Map.for_all
         (fun var cid' ->
           not (Closure_id.equal cid cid')
           || (match Variable.Map.find_opt var !value_use with
               | None -> true | Some n -> n <= 0))
         !bound_to
    && not (self_recursive cid)
    && not (declares_closures cid)
  in
  let marked = ref 0 in
  let rewrite (expr : Flambda.t) =
    match expr with
    | Apply ({ kind = Direct cid; inline = Default_inline; _ } as apply)
      when eligible cid ->
      incr marked;
      Flambda.Apply { apply with inline = Always_inline }
    | e -> e
  in
  let program =
    Flambda_iterators.map_exprs_at_toplevel_of_program program
      ~f:(fun expr -> Flambda_iterators.map rewrite (fun n -> n) expr)
  in
  program, !marked

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
  (* When the program has a workable number of constant-format entry sites
     into the stdlib's format machinery, the whole cleanup runs under the
     budgets that specialise the interpreters over those constants; the
     parameters are restored afterwards.  See [count_format_entry_sites]. *)
  let auto_specialise =
    (not !Clflags.lto_inline)
    && stats_before.total_size <= format_specialise_max_program_size
    && (let sites = count_format_entry_sites program in
        sites >= 1 && sites <= format_specialise_max_sites)
  in
  let with_specialise_budgets f =
    if !Clflags.lto_inline then
      (* An explicit request keeps the budgets supplied by the user. *)
      f ()
    else if not auto_specialise then begin
      (* Even without format specialisation, the cleanup rounds run with the
         inliner enabled and every budget at zero: the only decisions that
         fire below the thresholds are collapsing a function into its single
         reachable call site (size-neutral: the original dies with its one
         use) and what constant folding then exposes.  A device list that is
         a literal at the program's only entry thus folds through the
         backend's wrapper chain, and the branches it kills take their
         cones with them. *)
      let r x = Misc.R (x, !x) in
      Misc.protect_refs
        [ r Clflags.inline_threshold; r Clflags.inline_toplevel_threshold ]
        (fun () ->
          Clflags.Float_arg_helper.parse "0"
            "single-use collapse" Clflags.inline_threshold;
          Clflags.Int_arg_helper.parse "0"
            "single-use collapse" Clflags.inline_toplevel_threshold;
          f ())
    end
    else begin
      let r x = Misc.R (x, !x) in
      Misc.protect_refs
        [ r Clflags.inline_call_cost; r Clflags.inline_alloc_cost;
          r Clflags.inline_prim_cost; r Clflags.inline_branch_cost;
          r Clflags.inline_indirect_cost; r Clflags.inline_lifting_benefit;
          r Clflags.inline_branch_factor; r Clflags.inline_max_depth;
          r Clflags.inline_max_unroll; r Clflags.inline_threshold;
          r Clflags.inline_toplevel_threshold ]
        (fun () ->
          Clflags.use_inlining_arguments_set Clflags.o3_arguments;
          Clflags.Float_arg_helper.parse "1000"
            "format specialisation" Clflags.inline_threshold;
          Clflags.Int_arg_helper.parse "5"
            "format specialisation" Clflags.inline_max_unroll;
          f ())
    end
  in
  (* Iterate the cleanup to a fixpoint: eliminating a function or an unused
     closure variable can drop the last reference to another symbol, so each
     round can expose more dead code.  Terminates because a further round is
     only attempted while the function count strictly shrinks. *)
  let rec clean program =
    let count = (program_stats program).fun_count in
    let program =
      Remove_unused_program_constructs.remove_unused_program_constructs program
    in
    let program = preeval_initializers program in
    let program, _marked_single_use = mark_single_use_applies program in
    let program =
      let pure = compute_pure_functions program in
      remove_pure_initializers ~pure program
    in
    let program =
      Remove_unused_closure_vars.remove_unused_closure_variables
        ~remove_direct_call_surrogates:false program
    in
    (* Everything up to here only removes; it is committed unconditionally.
       The inlining stage below is the only one that can grow the program,
       so it alone is guarded: if its result is materially larger than the
       snapshot, the stage is discarded and the round keeps the cleanups. *)
    let shrunk = program in
    let size_shrunk = (program_stats shrunk).total_size in
    let program =
      (* Inlining here is opt-in: with the whole program in view it can
         specialise an interpreter over its statically known data (unrolling
         make_printf over a format literal, say), after which the interpreter
         itself is collected -- a large win on small images -- but its
         speed-oriented duplication grows large programs. *)
      Inline_and_simplify.run
        ~never_inline:false
        ~ppf_dump
        ~backend
        ~prefixname:"_link_"
        ~round:0
        program
    in
    let program =
      (* Collect before judging: a single-use move counts twice until the
         abandoned original is removed, so growth is only real if it
         survives a cleanup of the inlined result. *)
      Remove_unused_program_constructs.remove_unused_program_constructs
        program
    in
    let program =
      let sz = (program_stats program).total_size in
      if sz > size_shrunk + size_shrunk / 4 then begin
        Printf.eprintf
          "-use-lto: inlining stage grew the program (%d -> %d size \
           units); keeping the cleanups and discarding it\n%!"
          size_shrunk sz;
        shrunk
      end
      else program
    in
    if (program_stats program).fun_count < count then clean program
    else program
  in
  let cleaned_program = with_specialise_budgets (fun () -> clean program) in
  let cleaned_program =
    Remove_unused_closure_vars.remove_unused_closure_variables
      ~remove_direct_call_surrogates:true cleaned_program
  in
  let cleaned_program = Lift_constants.lift_constants ~backend cleaned_program in
  let cleaned_program = Share_constants.share_constants cleaned_program in
  (* After lifting, a fully preevaluated initializer's fields are single
     [Symbol]/[Const] lets, which this converts to a static [Let_symbol]
     block: no startup code remains for it at all. *)
  let cleaned_program = Initialize_symbol_to_let_symbol.run cleaned_program in
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
  (* The whole-program path deliberately skips [Flambda_invariants], so lower
     with the Cmm invariant check always on: it catches malformed control flow
     (e.g. duplicated continuation labels) at link time, where the alternative
     is a Mach-level fatal error or a silent miscompilation. *)
  Misc.protect_refs [Misc.R (Clflags.cmm_invariants, true)] (fun () ->
    compile_implementation_flambda
      ~unit_prefix
      ~backend
      ~ppf_dump
      cleaned_program);
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
