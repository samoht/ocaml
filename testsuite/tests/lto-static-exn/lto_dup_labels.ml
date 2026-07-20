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
   than one handler" on caml_link_$entry.

   Both units must contribute the SAME field, and the reads must feed an
   observable effect: the whole-program purity pass replaces unread pure
   initializer fields with constants (and drops pure toplevel bindings), so
   a plain [let _use = ...] would erase the reads and then the matches,
   leaving no collision to detect; and reading [r] from one unit but [s]
   from the other would leave the surviving matches with disjoint label
   sets. *)

external raise : exn -> 'a = "%raise"

exception Check

let () =
  match Lib_dup1.r, Lib_dup2.r with
  | (0, _), _ -> raise Check
  | _ -> ()
