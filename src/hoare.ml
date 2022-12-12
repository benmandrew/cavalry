open Ast.Program
module T = Why3.Term
module Ty = Why3.Ty

let unit_term = T.t_app (T.fs_tuple 0) [] (Some (Ty.ty_app (Ty.ts_tuple 0) []))

let val_to_term : type a. Ast.Vars.t -> a value -> T.term =
 fun vars v ->
  match v with
  | Unit _ -> unit_term
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_bool_true else T.t_bool_false
  | VarInst x ->
      let symbol = Ast.Vars.find x vars in
      T.t_var symbol

let rec expr_to_term : type a. Ast.Vars.t -> a expr -> T.term =
 fun vars e ->
  let f = expr_to_term vars in
  let open Ast.Arith in
  match e with
  | Value v -> val_to_term vars v
  | Eq (e, e') -> eq (f e) (f e')
  | Neq (e, e') -> neq (f e) (f e')
  | Lt (e, e') -> lt (f e) (f e')
  | Leq (e, e') -> leq (f e) (f e')
  | Gt (e, e') -> gt (f e) (f e')
  | Geq (e, e') -> geq (f e) (f e')
  | Plus (e, e') -> plus (f e) (f e')
  | Mul (e, e') -> mul (f e) (f e')

(** [forall y_i. t\[x_i <- y_i\]] *)
let forall_over_term t =
  let term_vars = T.t_v_fold (fun l x -> x :: l) [] t in
  let quant_vars = List.map (fun _ -> Ast.Vars.create_fresh "y") term_vars in
  let m =
    List.fold_left2
      (fun m y x -> T.Mvs.add x (T.t_var y) m)
      T.Mvs.empty quant_vars term_vars
  in
  T.t_subst m t |> T.t_forall_close quant_vars []

(* https://en.wikipedia.org/wiki/Predicate_transformer_semantics *)
let rec wlp vars c q =
  let wlp = wlp vars in
  match c with
  | IntExpr _ | Print _ -> q
  | Seq (c, c') -> wlp c (wlp c' q)
  | Assgn (x, e) ->
      (* forall y. y = e -> q[ x <- y ] *)
      let e_t = expr_to_term vars e in
      let x = Ast.Vars.find x vars in
      let y = Ast.Vars.create_fresh "y" in
      let y_t = T.t_var y in
      let q_sub = T.t_subst_single x y_t q in
      T.(t_forall_close [ y ] [] (t_implies (t_equ y_t e_t) q_sub))
  | If (b, c, c') ->
      (* ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
      let t = expr_to_term vars b in
      let wp = wlp c q in
      let wp' = wlp c' q in
      T.(t_and (t_implies t wp) (t_implies (t_not t) wp'))
  | While (inv, b, c) ->
      (* inv
         /\ forall y_i.
             ((b /\ inv) -> wlp(c, inv)
             /\ (~b /\ inv) -> q)[x_i <- y_i] *)
      let guard = expr_to_term vars b in
      let inv = Ast.Logic.translate_term vars inv in
      let s = wlp c inv in
      let iterate = T.(t_implies (t_and guard inv) s) in
      let postcond = T.(t_implies (t_and (t_not guard) inv) q) in
      let quant = forall_over_term (T.t_and iterate postcond) in
      T.(t_and inv quant)

let list_of_var_map (vm : Ast.Vars.t) = Ast.Vars.bindings vm |> List.map snd

let verify vars Ast.Triple.{ p; c; q } =
  let p = Ast.Logic.translate_term vars p in
  let q = Ast.Logic.translate_term vars q in
  let p_gen = wlp vars c q in
  let vars = list_of_var_map vars in
  Smt.Prover.prove_implies Ast.Arith.int_task vars p p_gen

let get_var (vm : Ast.Vars.t) x = Ast.Vars.find x vm
