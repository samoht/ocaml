(* TEST
 flambda;
 readonly_files = "lib_gen_meta.ml lib_gen_bind.ml lib_gen_use.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto";
 all_modules = "lib_gen_meta.ml lib_gen_bind.ml lib_gen_use.ml lto_generative.ml";
 program = "${test_build_directory}/lto_generative.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* [Lib_gen_meta.meta] is applied exactly once, so the -use-lto cleanup's
   single-use collapse wants to move its body into [Lib_gen_bind]'s
   initializer.  Doing so duplicates the [inj]/[proj] set of closures
   under fresh closure ids, and the copy escapes through the initialized
   symbol; [Lib_gen_use]'s body, compiled against [Lib_gen_meta]'s export
   info, still projects [M] out of the original closure id, and the
   whole-program simplifier faulted on the mismatch.  The collapse now
   refuses callees that declare their own set of closures; the link must
   succeed and the injection must round-trip. *)

let () =
  let m = Lib_gen_use.tag "x" in
  match Lib_gen_bind.info_of_meta m with
  | Some i -> print_endline i.Lib_gen_bind.name
  | None -> print_endline "none"
