(* The pair of closures escapes through this unit's lifted toplevel
   binding: the one call site of [Lib_gen_meta.meta] in the program. *)
type info = { name : string }

let (meta_of_info : info -> Lib_gen_meta.meta),
    (info_of_meta : Lib_gen_meta.meta -> info option) =
  Lib_gen_meta.meta ()
