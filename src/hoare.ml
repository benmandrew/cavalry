open Ast
open Ast.Program
module T = Why3.Term
module Ty = Why3.Ty

let list_of_var_map (vm : Vars.t) = Vars.bindings vm |> List.map snd
let unit_term = T.t_app (T.fs_tuple 0) [] (Some (Ty.ty_app (Ty.ts_tuple 0) []))

let val_to_term : type a. Vars.t -> a value -> T.term =
 fun vars v ->
  match v with
  | Unit _ -> unit_term
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_bool_true else T.t_bool_false
  | VarInst x ->
      let symbol = Vars.find x vars in
      T.t_var symbol

let rec expr_to_term : type a. Vars.t -> a expr -> T.term =
 fun vars e ->
  let f = expr_to_term vars in
  let open Arith in
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

module Proc_map = Map.Make (String)

(* https://en.wikipedia.org/wiki/Predicate_transformer_semantics *)
(* This does nothing as expressions are pure, but keep around anyway *)
let rec wlp_int_expr procs vars e q =
  ignore procs;
  ignore vars;
  ignore e;
  q
(* match e with
   | Value (Int _ | VarInst _) -> q
   | Plus (a, b) | Sub (a, b) | Mul (a, b) ->
       let q' = wlp_int_expr procs vars a q in
       wlp_int_expr procs vars b q' *)

(* and wlp_bool_expr procs vars e q =
   match e with
   | Value (Bool _) -> q
   | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
       let q' = wlp_int_expr procs vars a q in
       wlp_int_expr procs vars b q' *)

and wlp_cmd procs vars c q =
  let wlp_cmd = wlp_cmd procs vars in
  match c with
  | IntExpr e -> wlp_int_expr procs vars e q
  | Print _ -> q
  | Seq (c, c') -> wlp_cmd c (wlp_cmd c' q)
  | Assgn (x, e) | Let (x, e) ->
      (* forall y. y = e -> q[ x <- y ] *)
      let e_t = expr_to_term vars e in
      let x = Vars.find x vars in
      let y = Vars.create_fresh "y" in
      let y_t = T.t_var y in
      let q_sub = T.t_subst_single x y_t q in
      T.(t_forall_close [ y ] [] (t_implies (t_equ y_t e_t) q_sub))
  | Proc (f, ps) ->
      (* p_f[x_i <- e_i] /\ forall y. (q_f[x_i <- e_i][w_i <- y_i]['_w'_i <- w_i] -> q[w_i <- y_i]) *)
      let { Triple.p = p_f; q = q_f; ws; f = _f; ps = fps; c = _c }, p_vars =
        Proc_map.find f procs
      in
      ignore ws;
      let all_vars = Vars.union p_vars vars in
      let p_f = Logic.translate_term all_vars p_f in
      let q_f = Logic.translate_term all_vars q_f in
      let substitute_params p =
        let params = List.map (expr_to_term vars) ps in
        let fn q fp p =
          let fp = Vars.find fp p_vars in
          T.t_subst_single fp p q
        in
        List.fold_left2 fn p fps params
      in
      (* q_f[x_i <- e_i] *)
      let post = substitute_params q_f in
      let y = Vars.create_fresh "y" in
      (* let y_t = T.t_var y in *)
      (* (q_f[x_i <- e_i] -> q)[x <- y] *)
      (* TODO: WRITTEN VARIABLES *)
      let q_sub = T.t_implies post q in
      (* let q_sub = T.(t_subst_single x y_t (t_implies post q)) in *)
      (* p_f[x_i <- e_i] /\ forall y. (q_f[x_i <- e_i] -> q)[x <- y] *)
      let pre = substitute_params p_f in
      let tm = T.(t_and pre (t_forall_close [ y ] [] q_sub)) in
      let ghost_vars = Vars.filter_ghost_vars p_vars |> list_of_var_map in
      T.t_forall_close ghost_vars [] tm
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
      let inv = Logic.translate_term vars inv in
      let s = wlp_cmd c inv in
      let iterate = T.(t_implies (t_and guard inv) s) in
      let postcond = T.(t_implies (t_and (t_not guard) inv) q) in
      T.(t_and inv (T.t_and iterate postcond))

let merge_in vm x =
  match Vars.find_opt x vm with
  | None -> Vars.(add x (create_fresh x)) vm
  | Some _ -> vm

let verify_procedure ?timeout procs proc =
  match proc with
  | { Triple.p; q; ws; f = _f; ps; c }, vars ->
      ignore ws;
      let p = Logic.translate_term vars p in
      let q = Logic.translate_term vars q in
      let p_gen = wlp_cmd procs vars c q in
      let vars = List.fold_left merge_in vars ps |> list_of_var_map in
      (* Printf.printf "1\n";
         Logic.print_term p;
         Printf.printf "\n";
         flush stdout;
         Printf.printf "2\n";
         Logic.print_term q;
         Printf.printf "\n";
         flush stdout;
         Printf.printf "3\n";
         Logic.print_term T.(t_forall_close vars [] (t_implies p p_gen));
         Printf.printf "\n";
         flush stdout; *)
      Smt.Prover.prove timeout Arith.base_task vars (T.t_implies p p_gen)

exception Invalid_proc

let verify ?timeout program =
  let f procs proc =
    match proc with
    | { Triple.f; _ }, _ -> (
        match verify_procedure ?timeout procs proc with
        | Valid -> Proc_map.add f proc procs
        | Invalid | Failed _ -> raise Invalid_proc)
  in
  try
    let (_ : (Triple.t * Vars.t) Proc_map.t) =
      List.fold_left f Proc_map.empty program
    in
    Smt.Prover.Valid
  with Invalid_proc -> Smt.Prover.Invalid
