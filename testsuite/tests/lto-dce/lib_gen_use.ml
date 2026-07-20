(* Compiled per-unit, the application inlines [inj]'s body: this unit's
   cmx carries a [Project_var] naming [Lib_gen_meta]'s original closure
   ids, fixed before the -use-lto link re-optimizes the producer. *)
let tag name = Lib_gen_bind.meta_of_info { Lib_gen_bind.name }
