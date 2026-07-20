(* TEST
 flambda;
 readonly_files = "lib_dup1.ml lib_dup2.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto -nopervasives";
 compile_only = "true";
 all_modules = "lib_dup1.ml";
 ocamlopt.byte;
 all_modules = "lib_dup2.ml";
 ocamlopt.byte;
 all_modules = "lto_dup_labels.ml";
 ocamlopt.byte;
 compile_only = "false";
 flags = "-use-lto -nopervasives -output-obj";
 all_modules = "lib_dup1.cmx lib_dup2.cmx lto_dup_labels.cmx";
 program = "${test_build_directory}/lto_dup_labels.o";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Regression test for the -use-lto static-exception label collision.

   lib_dup1 and lib_dup2 are byte-for-byte identical and each module is
   compiled by its own compiler invocation, the way a build system compiles a
   project, so the static exceptions in their toplevel initializers get
   identical unit-local numbers (the counter restarts with each process; a
   single invocation compiling every module would number them apart and hide
   the bug).  The whole-program link concatenates both initializers into one
   startup function, where the labels collide unless [Flambda_to_clambda]
   allocates a fresh label per catch.  Before the fix this failed the
   always-on Cmm invariant check with "Continuation N was declared in more
   than one handler" on caml_link_$entry. *)

let _use = (Lib_dup1.r, Lib_dup2.s)
