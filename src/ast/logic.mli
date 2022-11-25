module T = Why3.Term

type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Impl of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Leq of expr * expr
  | Gt of expr * expr
  | Geq of expr * expr
  | Plus of expr * expr
  | Mul of expr * expr

val translate_term : Vars.t -> expr -> T.term
