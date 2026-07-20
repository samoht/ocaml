(* Exceptions and early exits, another source of static catch/raise handlers. *)

let find_first p a =
  let exception Found of int in
  try
    Array.iteri (fun i x -> if p x then raise (Found i)) a;
    -1
  with Found i -> i

let safe_div a b = try Some (a / b) with Division_by_zero -> None

let first_line s =
  match String.index_opt s '\n' with
  | Some i -> String.sub s 0 i
  | None -> s

let run () =
  Printf.sprintf "find=%d div=%s line=%S"
    (find_first (fun x -> x > 100) [| 3; 50; 200; 1 |])
    (match safe_div 10 0 with Some n -> string_of_int n | None -> "none")
    (first_line "hello\nworld")
