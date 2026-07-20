(* TEST
 flambda;
 readonly_files = "a.ml b.ml";
 setup-ocamlopt.byte-build-env;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -use-lto -c -for-pack Pack a.ml";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -use-lto -c -for-pack Pack b.ml";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -use-lto -pack a.cmx b.cmx -o pack.cmx";
 script;
 flags = "-use-lto";
 module = "test.ml";
 ocamlopt.byte;
 module = "";
 all_modules = "pack.cmx test.cmx";
 program = "${test_build_directory}/test.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* A packed unit concatenates its members' stored flambda bodies, so a pack
   can take part in a whole-program -use-lto link like any other unit. *)

let () = print_endline "Test entry"
let () = Pack.B.b ()
let () = Pack.A.a ()
