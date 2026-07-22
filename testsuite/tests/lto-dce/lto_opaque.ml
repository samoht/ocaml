(* TEST
 flambda;
 readonly_files = "lib_opaque_lto.ml";
 setup-ocamlopt.byte-build-env;

 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -opaque -nopervasives -c lib_opaque_lto.ml";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -warn-error +a -nopervasives -a -o lib_opaque_lto.cmxa lib_opaque_lto.cmx";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -warn-error +a -use-lto -nopervasives -a -o lib_opaque_lto2.cmxa lib_opaque_lto.cmx";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -opaque -nopervasives -c lto_opaque.ml";
 script;
 script = "${ocamlrun} ${ocamlopt_byte} -nostdlib -I ${ocamlsrcdir}/stdlib -use-lto -nopervasives -output-obj -o lto_opaque.o lib_opaque_lto.cmx lto_opaque.cmx";
 script;
*)

(* Dune deliberately passes [-opaque] when compiling modules generated at
   link time, including [Build_info_data].  An LTO-enabled compiler must retain
   the whole-program body while keeping ordinary cross-module information
   opaque, so the subsequent LTO link can consume both [.cmx] files.  The
   archive steps run under -warn-error +a: packing an -opaque member into a
   .cmxa must stay silent, with and without -use-lto. *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external raise : exn -> 'a = "%raise"
external opaque : int -> int = "%opaque"

exception Check

let () = if Lib_opaque_lto.succ (opaque 0) = 0 then raise Check
