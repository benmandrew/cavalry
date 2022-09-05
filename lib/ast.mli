
type var_map

type ast =
| Int of int
| Var of string
| Let of (string * ast * ast)
| Plus of (ast * ast)
| Mul of (ast * ast)

val exec : ast -> int
