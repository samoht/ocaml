(* TEST
 flambda;
 readonly_files = "lib_match.ml lib_loops.ml lib_exn.ml lib_escape.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto -dcmm-invariants";
 all_modules = "lib_match.ml lib_loops.ml lib_exn.ml lib_escape.ml lto_static_exn.ml";
 program = "${test_build_directory}/lto_static_exn.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Whole-program -use-lto links the static exceptions from each of these units
   into one program.  Before the fresh-label fix in [Flambda_to_clambda] their
   deserialised continuation ids collided; linking with [-dcmm-invariants] fails
   the Cmm invariant check on any such collision, and the checked output guards
   against a silent miscompilation.  [lib_escape] exercises
   [Stdlib__Bytes.unsafe_escape], the function whose duplicated labels first
   exposed the bug. *)

let () =
  print_endline (Lib_match.run ());
  print_endline (Lib_loops.run ());
  print_endline (Lib_exn.run ());
  print_endline (Lib_escape.run ())
