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

type ut_program = { p : Logic.expr; u : ut_expr; q : Logic.expr }
type program = { p : Logic.expr; c : cmd; q : Logic.expr }

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

let add_var vars x v = VarMap.add_exn vars ~key:x ~data:v

module StrSet = Set.Make (String)

let collect_vars c =
  let collect_value : type a. a value -> StrSet.t =
   fun v ->
    match v with
    | Int _ -> StrSet.empty
    | Bool _ -> StrSet.empty
    | VarInst str -> StrSet.singleton str
  in
  let rec collect_expr : type a. a expr -> StrSet.t =
   fun e ->
    match e with
    | Value v -> collect_value v
    | Eq (e, e') | Plus (e, e') | Mul (e, e') ->
        StrSet.union (collect_expr e) (collect_expr e')
  in
  let rec collect_cmd = function
    | IntExpr e -> collect_expr e
    | Seq (c, c') -> StrSet.union (collect_cmd c) (collect_cmd c')
    | Assgn (_, e) -> collect_expr e
    | If (b, e, e') ->
        StrSet.union (collect_expr b)
          (StrSet.union (collect_cmd e) (collect_cmd e'))
    | While (_, b, c) -> StrSet.union (collect_expr b) (collect_cmd c)
  in
  let vars = collect_cmd c in
  List.fold_left (StrSet.elements vars) ~init:Vars.empty ~f:(fun vm x ->
      let symbol = Why3.(Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int) in
      Vars.add x symbol vm)

exception UnboundVarError of string

let exec_value (vars : int VarMap.t) (type a) (v : a value) : a =
  match v with
  | Int i -> i
  | Bool b -> b
  | VarInst x -> (
      match VarMap.find vars x with
      | None -> raise (UnboundVarError x)
      | Some v -> v)

let rec exec_expr : type a. int VarMap.t -> a expr -> a * int VarMap.t =
 fun vars v ->
  match v with
  | Value v -> (exec_value vars v, vars)
  | Plus (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 + v2, vars'')
  | Mul (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 * v2, vars'')
  | Eq (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 = v2, vars'')

let rec exec_cmd vars = function
  | Seq (c, c') ->
      let _, vars' = exec_cmd vars c in
      exec_cmd vars' c'
  | Assgn (x, e) ->
      let v, vars' = exec_expr vars e in
      let vars'' = add_var vars' x v in
      (v, vars'')
  | If (e, c, c') ->
      let b, vars' = exec_expr vars e in
      if b then exec_cmd vars' c else exec_cmd vars' c'
  | While (inv, b, c) ->
      ignore inv;
      ignore b;
      ignore c;
      (0, vars)
  | IntExpr v -> exec_expr vars v

let exec t =
  let v, _ = exec_cmd VarMap.empty t in
  v
