(* TEST
 flambda;
 readonly_files = "lib_inline_budget.ml";
 setup-ocamlopt.byte-build-env;

 flags = "-use-lto -Oclassic -inline 0 -nopervasives";
 module = "lib_inline_budget.ml";
 ocamlopt.byte;
 module = "lto_inline_budget.ml";
 ocamlopt.byte;

 module = "";
 flags = "-use-lto -lto-inline -O3 -inline 1000 -nopervasives -output-obj";
 all_modules = "lib_inline_budget.cmx lto_inline_budget.cmx";
 program = "${test_build_directory}/lto_inline_budget.o";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* [transform] is compiled with a zero inlining budget and then called at two
   different sites, so it is not eligible for the linker's single-use
   collapse.  The positive budget explicitly supplied to the link must inline
   both calls and make the original function dead.  This regresses accidentally
   replacing explicit [-lto-inline] budgets with the zero-budget defaults. *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external raise : exn -> 'a = "%raise"
external opaque : int -> int = "%opaque"

exception Check

let () =
  if Lib_inline_budget.transform (opaque 1)
     = Lib_inline_budget.transform (opaque 2)
  then raise Check
