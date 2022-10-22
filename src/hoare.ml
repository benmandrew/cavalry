open Why3
open Ast
module VarMap = Map.Make (String)

type var_map = Term.vsymbol VarMap.t

let val_to_term : type a. var_map -> a value -> Term.term =
 fun vars v ->
  match v with
  | Int v -> Term.t_nat_const v
  | Bool b -> if b then Term.t_bool_true else Term.t_bool_false
  | VarInst x ->
      let symbol = VarMap.find x vars in
      Term.t_var symbol

let rec expr_to_term : type a. var_map -> a expr -> Term.term =
 fun vars e ->
  match e with
  | Value v -> val_to_term vars v
  | Eq (e, e') -> Term.t_equ (expr_to_term vars e) (expr_to_term vars e')
  | Plus (e, e') -> Arith.plus (expr_to_term vars e) (expr_to_term vars e')
  | Mul (e, e') -> Arith.mul (expr_to_term vars e) (expr_to_term vars e')

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
      let symbol = Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int in
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
      Term.t_subst_single symbol t q
  | If (b, c, c') ->
      (* ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
      let t = expr_to_term vars b in
      let wp = wlp vars c q in
      let wp' = wlp vars c' q in
      Term.(t_and (t_implies t wp) (t_implies (t_not t) wp'))

let list_of_vars (vm : var_map) =
  VarMap.bindings vm |> List.map (fun (_, x) -> x)

let verify vars c q =
  let p = wlp vars c q in
  let vars = list_of_vars vars in
  if Prover.prove_implies Arith.int_task vars p q then Some p else None

let get_var (vm : var_map) x = VarMap.find x vm
