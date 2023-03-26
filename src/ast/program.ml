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
  | App : string * int expr list -> int expr
[@@deriving sexp_of]

type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | Assgn of string * int expr
  | If of bool expr * cmd * cmd
  | While of Logic.expr * bool expr * cmd
  | Print of int expr
  | Func of string * string list * cmd
[@@deriving sexp_of]

type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | USeq of ut_expr * ut_expr
  | UEAssgn of string * ut_expr
  | UFAssgn of string * string * ut_expr list
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
  | UFunc of string * string list * ut_expr
[@@deriving sexp_of, show]

exception TypeError of string

let rec translate_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | UPlus (a, b) -> Plus (translate_int_expr a, translate_int_expr b)
  | USub (a, b) -> Sub (translate_int_expr a, translate_int_expr b)
  | UMul (a, b) -> Mul (translate_int_expr a, translate_int_expr b)
  | e -> raise (TypeError (show_ut_expr e))

and translate_bool_expr = function
  | UBool v -> Value (Bool v)
  | UEq (a, b) -> Eq (translate_int_expr a, translate_int_expr b)
  | UNeq (a, b) -> Neq (translate_int_expr a, translate_int_expr b)
  | ULt (a, b) -> Lt (translate_int_expr a, translate_int_expr b)
  | ULeq (a, b) -> Leq (translate_int_expr a, translate_int_expr b)
  | UGt (a, b) -> Gt (translate_int_expr a, translate_int_expr b)
  | UGeq (a, b) -> Geq (translate_int_expr a, translate_int_expr b)
  | e -> raise (TypeError (show_ut_expr e))

and expr_to_cmd = function
  | UInt v -> IntExpr (translate_int_expr (UInt v))
  | UVar v -> IntExpr (translate_int_expr (UVar v))
  | UPlus (a, b) -> IntExpr (translate_int_expr (UPlus (a, b)))
  | UMul (a, b) -> IntExpr (translate_int_expr (UMul (a, b)))
  | e -> raise (TypeError (show_ut_expr e))

and translate_cmd = function
  | USeq (c, c') -> Seq (translate_cmd c, translate_cmd c')
  | UEAssgn (s, e) -> Assgn (s, translate_int_expr e)
  | UFAssgn (s, f, ps) -> Assgn (s, App (f, List.map ps ~f:translate_int_expr))
  | UIf (e, c, c') ->
      If (translate_bool_expr e, translate_cmd c, translate_cmd c')
  | UWhile (inv, e, c) -> While (inv, translate_bool_expr e, translate_cmd c)
  | UFunc (f, ps, c) -> Func (f, ps, translate_cmd c)
  | v -> expr_to_cmd v
