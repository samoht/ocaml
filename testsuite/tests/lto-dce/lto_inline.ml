(* TEST
 flambda;
 readonly_files = "lib_interp.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto -lto-inline -O3 -inline 1000 -inline-max-unroll 5 -nopervasives -output-obj";
 all_modules = "lib_interp.ml lto_inline.ml";
 program = "${test_build_directory}/lto_inline.o";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* -lto-inline specialisation: [Lib_interp.run] is a recursive interpreter
   applied to the statically known [prog] but an %opaque runtime input, so
   neither the purity pass nor the link-time evaluator can touch it.  With
   the inliner enabled at the link and enough unrolling, simplification
   peels [run] over [prog] constructor by constructor into straight-line
   arithmetic on the opaque value, after which the interpreter itself is
   dead: the reference output shows every function eliminated. *)

external raise : exn -> 'a = "%raise"

external opaque : int -> int = "%opaque"

exception Check

let () =
  match Lib_interp.run Lib_interp.prog (opaque 5) with
  | -11 -> ()
  | _ -> raise Check
