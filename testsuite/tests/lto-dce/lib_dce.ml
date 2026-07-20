(* [used] is reachable from the main module; [dead1] calls [dead2], so the
   pair must be eliminated together, which exercises the transitive cleanup
   (removing [dead1] is what makes [dead2] removable).  Inlining is disabled
   so the functions survive to the link as distinct closures whatever the
   per-unit optimizer does.  Compiled with -nopervasives, hence no operators
   from Stdlib. *)

let[@inline never] used x = match x with 0 -> 1 | n -> n

let[@inline never] dead2 x = (x, x)

let[@inline never] dead1 x = dead2 x
