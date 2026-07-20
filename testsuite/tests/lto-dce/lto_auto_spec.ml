(* TEST
 flambda;
 setup-ocamlopt.byte-build-env;
 flags = "-use-lto";
 all_modules = "lto_auto_spec.ml";
 program = "${test_build_directory}/lto_auto_spec.exe";
 ocamlopt.byte;
 run;
 check-program-output;
*)

(* Automatic format specialisation: this program's constant-format entries
   put it under the site cap, so the whole-program cleanup runs with the
   specialising inliner budgets and the printf interpreters unroll over the
   literals below.  The checked output guards against the specialised code
   diverging from the interpreter.  The lazies exercise the other special
   case: [unforced] is never forced, so its thunk (and the large computation
   it captures) is eliminated as dead pure initialization, while [forced]
   must keep its thunk and produce 28 at runtime. *)

let[@inline never] rec heavy n = if n = 0 then 0 else heavy (n - 1) + n

let unforced = lazy (heavy 1_000_000)

let forced = lazy (heavy 7)

let () =
  Printf.printf "%d %s %c %.1f\n" 42 "auto" 'x' 1.5;
  Format.printf "%d@." (Lazy.force forced);
  print_string (Printf.sprintf "[%03d|%-4s|%x]\n" 5 "ab" 255)
