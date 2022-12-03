type var_map

type _ value =
  (* | Unit : unit -> unit value *)
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value
[@@deriving sexp_of]

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
[@@deriving sexp_of]

type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | Assgn of string * int expr
  | If of bool expr * cmd * cmd
  | While of Logic.expr * bool expr * cmd
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

type ut_program = { p : Logic.expr; u : ut_expr; q : Logic.expr }
type program = { p : Logic.expr; c : cmd; q : Logic.expr }

val translate_cmd : ut_expr -> cmd
val collect_vars : cmd -> Vars.t
val exec : cmd -> int
