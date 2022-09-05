open Core
module VarMap = Map.Make (String)

type var_map = int VarMap.t

type _ value =
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value

type _ expr =
  | Value : 'a value -> 'a expr
  | Let : string * int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr

(* Untyped AST to play nice with the Menhir parser generator *)
type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | ULet of string * ut_expr * ut_expr
  | UIf of ut_expr * ut_expr * ut_expr
  | UEq of ut_expr * ut_expr
  | UPlus of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr

exception TypeError

let rec type_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | ULet (s, e, e') -> Let (s, type_int_expr e, type_int_expr e')
  | UIf (e, e', e'') ->
      If (type_bool_expr e, type_int_expr e', type_int_expr e'')
  | UPlus (a, b) -> Plus (type_int_expr a, type_int_expr b)
  | UMul (a, b) -> Mul (type_int_expr a, type_int_expr b)
  | _ -> raise TypeError

and type_bool_expr = function
  | UBool v -> Value (Bool v)
  | UEq (a, b) -> Eq (type_int_expr a, type_int_expr b)
  | _ -> raise TypeError

let type_expr = type_int_expr
let add_var vars x v = VarMap.add_exn vars ~key:x ~data:v

let exec_value (vars : int VarMap.t) (type a) (v : a value) : a =
  match v with Int i -> i | Bool b -> b | VarInst x -> VarMap.find_exn vars x

let rec exec_aux : type a. int VarMap.t -> a expr -> a =
 fun vars v ->
  match v with
  | Value v -> exec_value vars v
  | Let (x, e, e') ->
      let v = exec_aux vars e in
      let new_vars = add_var vars x v in
      exec_aux new_vars e'
  | If (e, e', e'') ->
      let b = exec_aux vars e in
      if b then exec_aux vars e' else exec_aux vars e''
  | Plus (a, b) -> exec_aux vars a + exec_aux vars b
  | Mul (a, b) -> exec_aux vars a * exec_aux vars b
  | Eq (a, b) -> exec_aux vars a = exec_aux vars b

let exec t = exec_aux VarMap.empty t
