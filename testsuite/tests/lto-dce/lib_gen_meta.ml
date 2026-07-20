(* A generative injector: [meta] declares a set of closures and returns
   its closures.  The single-use collapse must leave [meta]'s body where
   it is declared: inlining it at its one call site (in [Lib_gen_bind]'s
   initializer) would duplicate the declaration under fresh closure ids,
   while [Lib_gen_use] was compiled against the original ids. *)
type meta = ..

let meta (type t) () =
  let module M = struct type meta += V : t -> meta end in
  let inj x = M.V x in
  let proj = function M.V v -> Some v | _ -> None in
  (inj, proj)
