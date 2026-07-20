(* Kept byte-for-byte identical to lib_dup2.ml: identical units number their
   static exceptions identically, so once both toplevel initializers are
   concatenated into the single startup function of a -use-lto link, the
   lowering must relabel the catches or emit two Cmm handlers with the same
   continuation number.  [%opaque] hides the scrutinees: a compiler-visible
   value would let the link-time partial evaluator execute these matches
   (catches and all) and replace the fields with their results. *)

external opaque : int -> int = "%opaque"

let[@inline never] g x = (x, x)

let r =
  match opaque 3 with
  | 1 | 3 | 5 -> g 1
  | 2 | 4 -> g 2
  | _ -> g 3

let s =
  match opaque 7 with
  | 6 | 8 | 10 -> g 4
  | 7 | 9 -> g 5
  | _ -> g 6
