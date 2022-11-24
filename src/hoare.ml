open Why3
open Ast.Program
module T = Term
module VarMap = Map.Make (String)

type var_map = T.vsymbol VarMap.t

let val_to_term : type a. var_map -> a value -> T.term =
 fun vars v ->
  match v with
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_bool_true else T.t_bool_false
  | VarInst x ->
      let symbol = VarMap.find x vars in
      T.t_var symbol

let rec expr_to_term : type a. var_map -> a expr -> T.term =
 fun vars e ->
  let f = expr_to_term vars in
  match e with
  | Value v -> val_to_term vars v
  | Eq (e, e') -> T.t_equ (f e) (f e')
  | Plus (e, e') -> Ast.Arith.plus (f e) (f e')
  | Mul (e, e') -> Ast.Arith.mul (f e) (f e')

module StrSet = Set.Make (String)

let collect_variables c =
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
  in
  let vars = collect_cmd c in
  List.fold_left
    (fun vm x ->
      let symbol = T.create_vsymbol (Ident.id_fresh x) Ty.ty_int in
      VarMap.add x symbol vm)
    VarMap.empty (StrSet.elements vars)

(* https://en.wikipedia.org/wiki/Predicate_transformer_semantics *)
let rec wlp vars c q =
  match c with
  | IntExpr _ -> q
  | Seq (c, c') -> wlp vars c (wlp vars c' q)
  | Assgn (x, e) ->
      (* q[ x <- e ] *)
      let t = expr_to_term vars e in
      let symbol = VarMap.find x vars in
      T.t_subst_single symbol t q
  | If (b, c, c') ->
      (* ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
      let t = expr_to_term vars b in
      let wp = wlp vars c q in
      let wp' = wlp vars c' q in
      T.(t_and (t_implies t wp) (t_implies (t_not t) wp'))

let list_of_var_map (vm : var_map) =
  VarMap.bindings vm |> List.map (fun (_, x) -> x)

let verify vars c p q =
  let p_gen = wlp vars c q in
  let vars = list_of_var_map vars in
  Smt.Prover.prove_implies Ast.Arith.int_task vars p p_gen

let get_var (vm : var_map) x = VarMap.find x vm
