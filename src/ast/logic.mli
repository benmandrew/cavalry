module T = Why3.Term

(* Untyped AST to play nice with the Menhir parser generator *)
type arith_expr =
  | Int of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Sub of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
[@@deriving sexp_of, show]

type logic_expr =
  | Bool of bool
  | Not of logic_expr
  | And of logic_expr * logic_expr
  | Or of logic_expr * logic_expr
  | Impl of logic_expr * logic_expr
  | Eq of arith_expr * arith_expr
  | Neq of arith_expr * arith_expr
  | Lt of arith_expr * arith_expr
  | Leq of arith_expr * arith_expr
  | Gt of arith_expr * arith_expr
  | Geq of arith_expr * arith_expr
[@@deriving sexp_of, show]

type expr = logic_expr [@@deriving sexp_of, show]

val translate_term : Vars.t -> expr -> T.term
val print_expr : expr -> unit
val print_term : T.term -> unit
