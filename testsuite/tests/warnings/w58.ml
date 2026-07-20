(* TEST
 flags = "-w +A-70";
 readonly_files = "module_without_cmx.mli";
 {
   setup-ocamlc.byte-build-env;
   module = "module_without_cmx.mli";
   ocamlc.byte;
   module = "w58.ml";
   ocamlc.byte;
   check-ocamlc.byte-output;
 }{
   no-lto;
   setup-ocamlopt.byte-build-env;
   module = "module_without_cmx.mli";
   ocamlopt.byte;
   module = "w58.ml";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
 }{
   lto;
   setup-ocamlopt.byte-build-env;
   module = "module_without_cmx.mli";
   ocamlopt.byte;
   module = "w58.ml";
   ocamlopt.byte;
   compiler_reference = "${test_source_directory}/w58.lto.reference";
   check-ocamlopt.byte-output;
 }
*)

let () = print_endline (Module_without_cmx.id "Hello World")
