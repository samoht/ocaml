(* for/while loops compile to static exceptions; combine them with matches so a
   single function carries several catch/exit handlers. *)

let checksum s =
  let acc = ref 0 in
  for i = 0 to String.length s - 1 do
    (match s.[i] with
     | 'a' .. 'z' -> acc := !acc + Char.code s.[i]
     | 'A' .. 'Z' -> acc := !acc - Char.code s.[i]
     | _ -> incr acc)
  done;
  !acc

let count_until_stop a =
  let i = ref 0 and stop = ref false in
  while not !stop && !i < Array.length a do
    (match a.(!i) with 0 -> stop := true | _ -> incr i)
  done;
  !i

let triangular n =
  let s = ref 0 in
  for i = 1 to n do s := !s + i done;
  !s

let run () =
  Printf.sprintf "checksum=%d until=%d tri=%d"
    (checksum "AbCdEf 123")
    (count_until_stop [| 3; 7; 0; 9 |])
    (triangular 10)
