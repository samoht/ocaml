(* [String.escaped]/[Bytes.escaped] are [Stdlib__Bytes.unsafe_escape], the
   function whose two internal loops originally collided on continuation labels
   under -use-lto.  Exercise the whole escaped family plus other stdlib matches. *)

let run () =
  let esc = String.escaped "a\n\t\"\\b\r\012\255z" in
  let besc = Bytes.to_string (Bytes.escaped (Bytes.of_string "x\ny\tz")) in
  let cesc = String.concat "" (List.map Char.escaped [ 'a'; '\n'; '\t'; '\255' ]) in
  let parts = String.split_on_char ',' "a,,b,c," in
  Printf.sprintf "esc=%s besc=%s cesc=%s split=[%s]" esc besc cesc
    (String.concat "|" parts)
