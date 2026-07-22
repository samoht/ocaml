external ( + ) : int -> int -> int = "%addint"
external ( * ) : int -> int -> int = "%mulint"

let transform x =
  let x = (x * 3) + 1 in
  let x = (x * 5) + 2 in
  let x = (x * 7) + 3 in
  let x = (x * 11) + 4 in
  let x = (x * 13) + 5 in
  let x = (x * 17) + 6 in
  let x = (x * 19) + 7 in
  let x = (x * 23) + 8 in
  (x * 29) + 9
