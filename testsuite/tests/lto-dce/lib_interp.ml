external ( + ) : int -> int -> int = "%addint"
external ( * ) : int -> int -> int = "%mulint"
external ( - ) : int -> int -> int = "%subint"

type op = Add of int | Mul of int | Neg

let rec run ops x =
  match ops with
  | [] -> x
  | Add n :: rest -> run rest (x + n)
  | Mul n :: rest -> run rest (x * n)
  | Neg :: rest -> run rest (0 - x)

let prog = [ Add 1; Mul 3; Neg; Add 7 ]
