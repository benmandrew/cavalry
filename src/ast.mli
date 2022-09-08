type var_map

type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value
[@@deriving sexp_of]

type _ expr =
  | Value : 'a value -> 'a expr
  | Seq : 'a expr * 'a expr -> 'a expr
  | Assgn : string * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
[@@deriving sexp_of]

(* Untyped AST to play nice with the Menhir parser generator *)
type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | USeq of ut_expr * ut_expr
  | UAssgn of string * ut_expr
  | UIf of ut_expr * ut_expr * ut_expr
  | UEq of ut_expr * ut_expr
  | UPlus of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr

val type_expr : ut_expr -> int expr
val exec : int expr -> int
