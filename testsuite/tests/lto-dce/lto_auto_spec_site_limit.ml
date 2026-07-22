(* TEST
 flambda;
 readonly_files = "gen_lto_adversarial.sh";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/gen_lto_adversarial.sh format-sites ${test_build_directory}/lto_auto_spec_site_limit_generated.ml 51";
 script;
 flags = "-use-lto";
 all_modules = "lto_auto_spec_site_limit_generated.ml";
 program = "${test_build_directory}/lto_auto_spec_site_limit.exe";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* The automatic format-specialisation path accepts at most 50 constant-format
   entry sites.  This generated program sits immediately above that boundary,
   where the linker must retain its bounded zero-budget behaviour. *)
