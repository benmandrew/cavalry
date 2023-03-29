open Core

type _ value =
  | Unit : unit -> unit value
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value
[@@deriving sexp_of]

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Neq : int expr * int expr -> bool expr
  | Lt : int expr * int expr -> bool expr
  | Leq : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | Geq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
[@@deriving sexp_of]

type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | EAssgn of string * int expr
  | PAssgn of string * string * int expr list
  | If of bool expr * cmd * cmd
  | While of Logic.expr * bool expr * cmd
  | Print of int expr
[@@deriving sexp_of]

type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | USeq of ut_expr * ut_expr
  | UEAssgn of string * ut_expr
  | UPAssgn of string * string * ut_expr list
  | UIf of ut_expr * ut_expr * ut_expr
  | UWhile of Logic.expr * ut_expr * ut_expr
  | UEq of ut_expr * ut_expr
  | UNeq of ut_expr * ut_expr
  | ULt of ut_expr * ut_expr
  | ULeq of ut_expr * ut_expr
  | UGt of ut_expr * ut_expr
  | UGeq of ut_expr * ut_expr
  | UPlus of ut_expr * ut_expr
  | USub of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr
[@@deriving sexp_of, show]

exception TypeError of string

let rec t_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | UPlus (a, b) -> Plus (t_int_expr a, t_int_expr b)
  | USub (a, b) -> Sub (t_int_expr a, t_int_expr b)
  | UMul (a, b) -> Mul (t_int_expr a, t_int_expr b)
  | e -> raise (TypeError (show_ut_expr e))

and t_bool_expr = function
  | UBool v -> Value (Bool v)
  | UEq (a, b) -> Eq (t_int_expr a, t_int_expr b)
  | UNeq (a, b) -> Neq (t_int_expr a, t_int_expr b)
  | ULt (a, b) -> Lt (t_int_expr a, t_int_expr b)
  | ULeq (a, b) -> Leq (t_int_expr a, t_int_expr b)
  | UGt (a, b) -> Gt (t_int_expr a, t_int_expr b)
  | UGeq (a, b) -> Geq (t_int_expr a, t_int_expr b)
  | e -> raise (TypeError (show_ut_expr e))

and expr_to_cmd = function
  | UInt v -> IntExpr (t_int_expr (UInt v))
  | UVar v -> IntExpr (t_int_expr (UVar v))
  | UPlus (a, b) -> IntExpr (t_int_expr (UPlus (a, b)))
  | UMul (a, b) -> IntExpr (t_int_expr (UMul (a, b)))
  | e -> raise (TypeError (show_ut_expr e))

and t_cmd = function
  | USeq (c, c') -> Seq (t_cmd c, t_cmd c')
  | UEAssgn (s, e) -> EAssgn (s, t_int_expr e)
  | UPAssgn (s, f, ps) -> PAssgn (s, f, List.map ps ~f:t_int_expr)
  | UIf (e, c, c') -> If (t_bool_expr e, t_cmd c, t_cmd c')
  | UWhile (inv, e, c) -> While (inv, t_bool_expr e, t_cmd c)
  | v -> expr_to_cmd v

let translate_cmd = t_cmd
