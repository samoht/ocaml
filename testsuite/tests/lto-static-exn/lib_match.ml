(* Pattern matches that lower to static exceptions: or-patterns joining to a
   shared handler, guards, character ranges, and a switch with a computed
   default.  Under -use-lto these are re-lowered from concatenated cmx. *)

let classify c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> "vowel"
  | 'a' .. 'z' -> "lower"
  | 'A' .. 'Z' -> "upper"
  | '0' .. '9' -> "digit"
  | ' ' | '\t' | '\n' | '\r' -> "space"
  | '!' | '?' | '.' | ',' | ';' | ':' -> "punct"
  | c -> Printf.sprintf "other(%d)" (Char.code c)

let sign_guard n =
  match n with
  | n when n < 0 -> -1
  | 0 -> 0
  | _ -> 1

(* Nested matches with a shared exit. *)
let describe (a, b) =
  match a with
  | None -> "none"
  | Some x ->
    (match b with
     | [] -> Printf.sprintf "just(%d)" x
     | [ y ] -> Printf.sprintf "pair(%d,%d)" x y
     | y :: _ -> Printf.sprintf "many(%d,%d,..)" x y)

let run () =
  let b = Buffer.create 64 in
  String.iter
    (fun c -> Buffer.add_string b (classify c); Buffer.add_char b ' ')
    "Ok! 42\tz.";
  List.iter
    (fun n -> Buffer.add_string b (string_of_int (sign_guard n)); Buffer.add_char b ' ')
    [ -5; 0; 9 ];
  List.iter
    (fun p -> Buffer.add_string b (describe p); Buffer.add_char b ' ')
    [ (None, []); (Some 1, []); (Some 2, [ 3 ]); (Some 4, [ 5; 6 ]) ];
  Buffer.contents b
