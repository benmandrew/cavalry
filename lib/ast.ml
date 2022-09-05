
type ast =
  | Int of int
  | Plus of (ast * ast)
  | Mul of (ast * ast)

let rec exec = function
  | Int a -> a
  | Plus (a, b) -> (exec a) + (exec b)
  | Mul (a, b) -> (exec a) * (exec b)

