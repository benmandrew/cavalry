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
module Wlp = struct
  let sub_old_vars vars ws p =
    let map =
      List.filter_map
        (fun w ->
          let x = Vars.find_opt ("_" ^ w) vars in
          Option.map (fun x -> (x, T.t_var (Vars.find w vars))) x)
        ws
    in
    T.(t_subst (Mvs.of_list map) p)

  let proc vars q p_vars ps (t : Triple.t) =
    let all_vars = Vars.union p_vars vars in
    let p_f = Logic.translate_term all_vars t.p in
    let q_f = Logic.translate_term all_vars t.q in
    let sub_params p =
      let map =
        List.map2
          (fun fp e -> (Vars.find fp p_vars, expr_to_term vars e))
          t.ps ps
      in
      T.(t_subst (Mvs.of_list map) p)
    in
    (* q_f[x_i <- e_i] *)
    let post = sub_params q_f in
    (* p_f[x_i <- e_i] *)
    let pre = sub_params p_f in
    let sub_written_vars p =
      let ys = List.map (fun _ -> Vars.create_fresh "y") t.ws in
      let map = List.map2 (fun w y -> (Vars.find w vars, T.t_var y)) t.ws ys in
      T.(t_subst (Mvs.of_list map) p |> t_forall_close ys [])
    in
    (* forall y. ((q_f[x_i <- e_i] -> q)[x <- y])[x_i@old <- x_i] *)
    let q_sub =
      T.t_implies post q |> sub_written_vars
      |> sub_old_vars vars t.ws
    in
    (* p_f[x_i <- e_i] /\ forall y. (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i] -> q[x_i <- y_i]) *)
    T.(t_and pre q_sub)

  let rec cmd procs vars c q =
    let cmd = cmd procs vars in
    match c with
    | IntExpr _ -> q
    | Print _ -> q
    | Seq (c, c') -> cmd c (cmd c' q)
    | Assgn (x, e) | Let (x, e) ->
        (* forall y. y = e -> q[ x <- y ] *)
        let e_t = expr_to_term vars e in
        let x = Vars.find x vars in
        let y = Vars.create_fresh "y" in
        let y_t = T.t_var y in
        let q_sub = T.t_subst_single x y_t q in
        T.(t_forall_close [ y ] [] (t_implies (t_equ y_t e_t) q_sub))
    | Proc (f, ps) ->
        (* p_f[x_i <- e_i]
            /\ forall y.
              (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i]
              -> q[x_i <- y_i]) *)
        let triple, p_vars = Proc_map.find f procs in
        proc vars q p_vars ps triple
    | If (b, c, c') ->
        (* ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
        let t = expr_to_term vars b in
        let wp = cmd c q in
        let wp' = cmd c' q in
        T.(t_and (t_implies t wp) (t_implies (t_not t) wp'))
    | While (inv, b, c) ->
        (* inv
            /\ forall y_i.
                (((b /\ inv) -> wlp(c, inv))
                /\ ((~b /\ inv) -> q))[x_i <- y_i] *)
        let guard = expr_to_term vars b in
        let inv = Logic.translate_term vars inv in
        let s = cmd c inv in
        let iterate = T.(t_implies (t_and guard inv) s) in
        let postcond = T.(t_implies (t_and (t_not guard) inv) q) in
        T.(t_and inv (T.t_and iterate postcond))
end

let merge_in vm x =
  match Vars.find_opt x vm with
  | None -> Vars.(add x (create_fresh x)) vm
  | Some _ -> vm

let verify_procedure ?timeout procs ((t : Triple.t), vars) =
  let p = Logic.translate_term vars t.p in
  let q = Logic.translate_term vars t.q in
  let p_gen = Wlp.(sub_old_vars vars t.ws @@ cmd procs vars t.c q) in
  let vars = List.fold_left merge_in vars t.ps |> list_of_var_map in
  Printf.printf "1\n";
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
  flush stdout;
  Smt.Prover.prove timeout Arith.base_task vars (T.t_implies p p_gen)

exception Proc_invalid

let verify ?timeout program =
  let f procs proc =
    let (t : Triple.t) = fst proc in
    match verify_procedure ?timeout procs proc with
    | Valid -> Proc_map.add t.f proc procs
    | Invalid | Failed _ -> raise Proc_invalid
  in
  try
    let (_ : (Triple.t * Vars.t) Proc_map.t) =
      List.fold_left f Proc_map.empty program
    in
    Smt.Prover.Valid
  with Proc_invalid -> Smt.Prover.Invalid
