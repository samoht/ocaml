(* Kept byte-for-byte identical to lib_dup2.ml: identical units number their
   static exceptions identically, so once both toplevel initializers are
   concatenated into the single startup function of a -use-lto link, the
   lowering must relabel the catches or emit two Cmm handlers with the same
   continuation number.  [pick] hides the scrutinee so the matches and their
   shared or-pattern handlers survive to the link. *)

let[@inline never] pick n = n

let[@inline never] g x = (x, x)

let r =
  match pick 3 with
  | 1 | 3 | 5 -> g 1
  | 2 | 4 -> g 2
  | _ -> g 3

let s =
  match pick 7 with
  | 6 | 8 | 10 -> g 4
  | 7 | 9 -> g 5
  | _ -> g 6
