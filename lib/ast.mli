
type ast =
| Int of int
| Plus of (ast * ast)
| Mul of (ast * ast)

val exec : ast -> int
