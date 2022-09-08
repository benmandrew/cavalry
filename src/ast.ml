open Core
module VarMap = Map.Make (String)

type var_map = int VarMap.t

type _ value =
  (* | Unit : unit -> unit value *)
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

exception TypeError

let rec type_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | USeq (e, e') -> Seq (type_int_expr e, type_int_expr e')
  | UAssgn (s, e) -> Assgn (s, type_int_expr e)
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

exception UnboundVarError of string

let exec_value (vars : int VarMap.t) (type a) (v : a value) : a =
  match v with
  | Int i -> i
  | Bool b -> b
  | VarInst x -> (
      match VarMap.find vars x with
      | None -> raise (UnboundVarError x)
      | Some v -> v)

let rec exec_aux : type a. int VarMap.t -> a expr -> a * int VarMap.t =
 fun vars v ->
  match v with
  | Value v -> (exec_value vars v, vars)
  | Seq (e, e') ->
      let _, vars' = exec_aux vars e in
      exec_aux vars' e'
  | Assgn (x, e) ->
      let v, vars' = exec_aux vars e in
      let vars'' = add_var vars' x v in
      (v, vars'')
  | If (e, e', e'') ->
      let b, vars' = exec_aux vars e in
      if b then exec_aux vars' e' else exec_aux vars' e''
  | Plus (a, b) ->
      let v1, vars' = exec_aux vars a in
      let v2, vars'' = exec_aux vars' b in
      (v1 + v2, vars'')
  | Mul (a, b) ->
      let v1, vars' = exec_aux vars a in
      let v2, vars'' = exec_aux vars' b in
      (v1 * v2, vars'')
  | Eq (a, b) ->
      let v1, vars' = exec_aux vars a in
      let v2, vars'' = exec_aux vars' b in
      (v1 = v2, vars'')

let exec t =
  let v, _ = exec_aux VarMap.empty t in
  v
