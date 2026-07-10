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
  | Div : int expr * int expr -> int expr
  | Mod : int expr * int expr -> int expr
  | Get : string * int expr -> int expr (* array element a[i] *)
  | Len : string -> int expr (* array length len(a) *)
[@@deriving sexp_of]

type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | Assgn of string * int expr
  | Let of string * int expr
  | Proc of string * int expr list
  | If of bool expr * cmd * cmd
  | While of Logic.expr * bool expr * cmd
  | Print of int expr
  | ArrMake of string * int expr (* a <- array(n) *)
  | ArrAssgn of string * int expr * int expr (* a[i] <- e *)
[@@deriving sexp_of]

type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | USeq of ut_expr * ut_expr
  | UAssgn of string * ut_expr
  | ULet of string * ut_expr
  | UProc of string * ut_expr list
  | UIf of ut_expr * ut_expr * ut_expr
  | UWhile of Logic.expr * ut_expr * ut_expr
  | UPrint of ut_expr
  | UEq of ut_expr * ut_expr
  | UNeq of ut_expr * ut_expr
  | ULt of ut_expr * ut_expr
  | ULeq of ut_expr * ut_expr
  | UGt of ut_expr * ut_expr
  | UGeq of ut_expr * ut_expr
  | UPlus of ut_expr * ut_expr
  | USub of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr
  | UDiv of ut_expr * ut_expr
  | UMod of ut_expr * ut_expr
  | UGet of string * ut_expr
  | ULen of string
  | UArrMake of string * ut_expr
  | UArrAssgn of string * ut_expr * ut_expr
[@@deriving sexp_of, show]

exception TypeError of string

let rec t_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | UPlus (a, b) -> Plus (t_int_expr a, t_int_expr b)
  | USub (a, b) -> Sub (t_int_expr a, t_int_expr b)
  | UMul (a, b) -> Mul (t_int_expr a, t_int_expr b)
  | UDiv (a, b) -> Div (t_int_expr a, t_int_expr b)
  | UMod (a, b) -> Mod (t_int_expr a, t_int_expr b)
  | UGet (a, i) -> Get (a, t_int_expr i)
  | ULen a -> Len a
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
  | UAssgn (s, e) -> Assgn (s, t_int_expr e)
  | ULet (s, e) -> Let (s, t_int_expr e)
  | UProc (f, ps) -> Proc (f, List.map ps ~f:t_int_expr)
  | UIf (e, c, c') -> If (t_bool_expr e, t_cmd c, t_cmd c')
  | UWhile (inv, e, c) -> While (inv, t_bool_expr e, t_cmd c)
  | UPrint e -> Print (t_int_expr e)
  | UArrMake (a, n) -> ArrMake (a, t_int_expr n)
  | UArrAssgn (a, i, e) -> ArrAssgn (a, t_int_expr i, t_int_expr e)
  | v -> expr_to_cmd v

let translate_cmd = t_cmd
