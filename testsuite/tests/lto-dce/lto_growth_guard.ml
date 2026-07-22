(* TEST
 flambda;
 readonly_files = "lib_growth_guard.ml";
 setup-ocamlopt.byte-build-env;

 flags = "-use-lto -Oclassic -inline 0 -nopervasives";
 module = "lib_growth_guard.ml";
 ocamlopt.byte;
 module = "lto_growth_guard.ml";
 ocamlopt.byte;

 module = "";
 flags = "-use-lto -lto-inline -O3 -inline 1000 -nopervasives -output-obj";
 all_modules = "lib_growth_guard.cmx lto_growth_guard.cmx";
 program = "${test_build_directory}/lto_growth_guard.o";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Sixteen call sites make the explicit inlining round expand [caller] far
   beyond the 25% growth allowance.  The linker must discard that inlining
   result while keeping the safe cleanup work that preceded it. *)

external ( + ) : int -> int -> int = "%addint"
external ( = ) : 'a -> 'a -> bool = "%equal"
external raise : exn -> 'a = "%raise"
external opaque : int -> int = "%opaque"

exception Check

let[@inline never] caller x =
  let total =
    Lib_growth_guard.transform (x + 0) + Lib_growth_guard.transform (x + 1)
    + Lib_growth_guard.transform (x + 2)
    + Lib_growth_guard.transform (x + 3)
    + Lib_growth_guard.transform (x + 4)
    + Lib_growth_guard.transform (x + 5)
    + Lib_growth_guard.transform (x + 6)
    + Lib_growth_guard.transform (x + 7)
    + Lib_growth_guard.transform (x + 8)
    + Lib_growth_guard.transform (x + 9)
    + Lib_growth_guard.transform (x + 10)
    + Lib_growth_guard.transform (x + 11)
    + Lib_growth_guard.transform (x + 12)
    + Lib_growth_guard.transform (x + 13)
    + Lib_growth_guard.transform (x + 14)
    + Lib_growth_guard.transform (x + 15)
  in
  opaque total

let () = if caller (opaque 0) = 0 then raise Check
