external ( + ) : int -> int -> int = "%addint"
external ( * ) : int -> int -> int = "%mulint"

let transform x =
  let x = (x * 3) + 1 in
  let x = (x * 5) + 2 in
  let x = (x * 7) + 3 in
  let x = (x * 11) + 4 in
  (x * 13) + 5
