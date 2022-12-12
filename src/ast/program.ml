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

exception TypeError

let rec translate_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | UPlus (a, b) -> Plus (translate_int_expr a, translate_int_expr b)
  | UMul (a, b) -> Mul (translate_int_expr a, translate_int_expr b)
  | _ -> raise TypeError

and translate_bool_expr = function
  | UBool v -> Value (Bool v)
  | UEq (a, b) -> Eq (translate_int_expr a, translate_int_expr b)
  | _ -> raise TypeError

and expr_to_cmd = function
  | UInt v -> IntExpr (translate_int_expr (UInt v))
  | UVar v -> IntExpr (translate_int_expr (UVar v))
  | UPlus (a, b) -> IntExpr (translate_int_expr (UPlus (a, b)))
  | UMul (a, b) -> IntExpr (translate_int_expr (UMul (a, b)))
  | _ -> raise TypeError

and translate_cmd = function
  | USeq (e, e') -> Seq (translate_cmd e, translate_cmd e')
  | UAssgn (s, e) -> Assgn (s, translate_int_expr e)
  | UIf (e, e', e'') ->
      If (translate_bool_expr e, translate_cmd e', translate_cmd e'')
  | v -> expr_to_cmd v
