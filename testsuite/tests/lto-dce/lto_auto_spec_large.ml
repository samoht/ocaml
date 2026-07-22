(* TEST
 flambda;
 readonly_files = "gen_lto_adversarial.sh";
 setup-ocamlopt.byte-build-env;
 script = "sh ${test_source_directory}/gen_lto_adversarial.sh live-functions ${test_build_directory}/lto_auto_spec_large_generated.ml 22000";
 script;
 flags = "-use-lto";
 all_modules = "lto_auto_spec_large_generated.ml";
 program = "${test_build_directory}/lto_auto_spec_large.exe";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* All 22,000 generated functions stay reachable through an opaque table
   lookup.  Together with one constant-format site they reproduce the hostile
   shape that used to run the aggressive format-specialisation budgets over a
   large application.  The checked function counts distinguish the bounded
   path from that expensive path. *)
