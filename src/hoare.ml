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
  | Sub (e, e') -> sub (f e) (f e')
  | Mul (e, e') -> mul (f e) (f e')
  | App (_f, _ps) -> unit_term

module FuncMap = Map.Make (String)

(* https://en.wikipedia.org/wiki/Predicate_transformer_semantics *)
let rec wlp_int_expr funcs vars e q =
  match e with
  | Value (Int _ | VarInst _) -> q
  | Plus (a, b) | Sub (a, b) | Mul (a, b) ->
      let q' = wlp_int_expr funcs vars a q in
      wlp_int_expr funcs vars b q'
  | App (f, vs) ->
      (* ASSUMING f(x) = {p_f}c{q_f} HOLDS *)
      (* wlp (v_1, wlp(v_2, ... wlp(v_n, p_f) ... )) /\ q_f -> q *)
      let f_triple = FuncMap.find f funcs in
      let p =
        let p_f = Ast.(Logic.translate_term vars f_triple.Triple.p) in
        let vs_rev = List.rev vs in
        List.fold_left (fun t v -> wlp_int_expr funcs vars v t) p_f vs_rev
      in
      let q_f_impl_q =
        let q_f = Ast.(Logic.translate_term vars f_triple.Triple.q) in
        T.t_implies q_f q
      in
      T.t_and p q_f_impl_q

(* and wlp_bool_expr funcs vars e q =
   match e with
   | Value (Bool _) -> q
   | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
       let q' = wlp_int_expr funcs vars a q in
       wlp_int_expr funcs vars b q' *)

and wlp_cmd funcs vars c q =
  let wlp_cmd = wlp_cmd funcs vars in
  match c with
  | IntExpr e -> wlp_int_expr funcs vars e q
  | Print _ -> q
  | Seq (c, c') -> wlp_cmd c (wlp_cmd c' q)
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
      let wp = wlp_cmd c q in
      let wp' = wlp_cmd c' q in
      T.(t_and (t_implies t wp) (t_implies (t_not t) wp'))
  | While (inv, b, c) ->
      (* inv
         /\ forall y_i.
             (((b /\ inv) -> wlp(c, inv))
             /\ ((~b /\ inv) -> q))[x_i <- y_i] *)
      let guard = expr_to_term vars b in
      let inv = Ast.Logic.translate_term vars inv in
      let s = wlp_cmd c inv in
      let iterate = T.(t_implies (t_and guard inv) s) in
      let postcond = T.(t_implies (t_and (t_not guard) inv) q) in
      T.(t_and inv (T.t_and iterate postcond))
  | Func (_f, ps, c) ->
      let quant_vars = List.map (fun x -> Ast.Vars.create_fresh x) ps in
      T.t_forall_close quant_vars [] (wlp_cmd c q)

let wlp = wlp_cmd
let list_of_var_map (vm : Ast.Vars.t) = Ast.Vars.bindings vm |> List.map snd

let verify ?timeout vars Ast.Triple.{ p; c; q } =
  let p = Ast.Logic.translate_term vars p in
  let q = Ast.Logic.translate_term vars q in
  let p_gen = wlp_cmd FuncMap.empty vars c q in
  let vars = list_of_var_map vars in
  (* Printf.printf "\n\n";
     Ast.Logic.print_term (T.t_forall_close vars [] (T.t_implies p p_gen));
     Printf.printf "\n\n";
     flush stdout; *)
  Smt.Prover.prove_implies timeout Ast.Arith.base_task vars p p_gen
