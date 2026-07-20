(* TEST
 flambda;
 readonly_files = "lib_dce.ml";
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto -dlto-dce -dlto-why-live=used -nopervasives -output-obj";
 all_modules = "lib_dce.ml lto_dce.ml";
 program = "${test_build_directory}/lto_dce.o";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Linked with -nopervasives so the whole program is only these two units and
   the -use-lto report is deterministic: the summary counts, the per-unit
   -dlto-dce table, the eliminated-function listing, and the -dlto-why-live
   retention chain are all checked against the reference output.  -output-obj
   stops at a partial link, which is the only kind a -nopervasives program can
   complete (no runtime library is linked in).

   The conditional raise is an observable effect on a value the compiler
   cannot see ([%opaque]), so the [used] call feeding it survives -- without
   the barrier the link-time partial evaluator would decide the match and
   discharge the guard entirely.  [_pure] is dead pure initialization work
   (a call, so beyond [Effect_analysis]) that the whole-program cleanup
   removes. *)

external raise : exn -> 'a = "%raise"

external opaque : int -> int = "%opaque"

exception Check

let () = match Lib_dce.used (opaque 41) with 0 -> raise Check | _ -> ()

let _pure = Lib_dce.used 7
