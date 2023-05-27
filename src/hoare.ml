open Ast
open Ast.Program
module T = Why3.Term
module Ty = Why3.Ty

(* Split a list [l @ \[m\]] into the tuple [(l, m)] *)
let split_last l =
  let rec aux acc = function
    | [] -> failwith "Can't take the last element of an empty list"
    | [ x ] -> (List.rev acc, x)
    | x :: l -> aux (x :: acc) l
  in
  aux [] l

let debug = false

let debug_print f vars p q p_gen =
  let open Printf in
  if debug then (
    printf "===== Procedure '%s'\n" f;
    printf "===== p           =====\n";
    Logic.print_term p;
    printf "\n===== q           =====\n";
    Logic.print_term q;
    printf "\n===== p -> wlp(q) =====\n";
    Logic.print_term T.(t_forall_close vars [] (t_implies p p_gen));
    printf "\n\n";
    flush stdout)

let list_of_var_map (vm : Vars.t) = Vars.bindings vm |> List.map snd
let unit_term = T.t_app (T.fs_tuple 0) [] (Some (Ty.ty_app (Ty.ts_tuple 0) []))

let val_to_term : type a. g_vars:Vars.t -> ?l_vars:Vars.t -> a value -> T.term =
 fun ~g_vars ?l_vars v ->
  match v with
  | Unit _ -> unit_term
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_bool_true else T.t_bool_false
  | VarInst x -> (
      match l_vars with
      | Some l_vars -> T.t_var @@ Vars.find_fallback x l_vars g_vars
      | None -> T.t_var @@ Vars.find x g_vars)

let rec expr_to_term :
    type a. g_vars:Vars.t -> ?l_vars:Vars.t -> a expr -> T.term =
 fun ~g_vars ?l_vars e ->
  let f = expr_to_term ~g_vars ?l_vars in
  let open Arith in
  match e with
  | Value v -> val_to_term ~g_vars ?l_vars v
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
  let sub_old_vars ~g_vars ~l_vars ws p =
    let map =
      List.filter_map
        (fun w ->
          let x = Vars.find_opt ("_" ^ w) l_vars in
          Option.map (fun x -> (x, T.t_var (Vars.find w g_vars))) x)
        ws
    in
    T.(t_subst (Mvs.of_list map) p)

  let proc g_vars q l_vars ps (t : Triple.t) =
    let p_f = Logic.translate_term ~g_vars ~l_vars t.p in
    let q_f = Logic.translate_term ~g_vars ~l_vars t.q in
    let sub_params p =
      let map =
        List.map2
          (fun fp e -> (Vars.find fp l_vars, expr_to_term ~g_vars ~l_vars e))
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
      let map =
        List.map2 (fun w y -> (Vars.find w g_vars, T.t_var y)) t.ws ys
      in
      T.(t_subst (Mvs.of_list map) p |> t_forall_close ys [])
    in
    (* forall y. ((q_f[x_i <- e_i] -> q)[x <- y])[x_i@old <- x_i] *)
    let q_sub =
      T.t_implies post q |> sub_written_vars
      |> sub_old_vars ~g_vars ~l_vars t.ws
    in
    (* p_f[x_i <- e_i] /\ forall y. (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i] -> q[x_i <- y_i]) *)
    T.(t_and pre q_sub)

  let rec cmd procs ~g_vars ~l_vars c q =
    let cmd = cmd procs ~g_vars ~l_vars in
    match c with
    | IntExpr _ -> q
    | Print _ -> q
    | Seq (c, c') -> cmd c (cmd c' q)
    | Assgn (x, e) | Let (x, e) ->
        (* forall y. y = e -> q[ x <- y ] *)
        let e_t = expr_to_term ~g_vars ~l_vars e in
        let x = Vars.find_fallback x l_vars g_vars in
        let y = Vars.create_fresh "y" in
        let y_t = T.t_var y in
        let q_sub = T.t_subst_single x y_t q in
        T.(t_forall_close [ y ] [] (t_implies (t_equ y_t e_t) q_sub))
    | Proc (f, ps) ->
        (* p_f[x_i <- e_i]
            /\ forall y.
              (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i]
              -> q[x_i <- y_i]) *)
        let triple, l_vars = Proc_map.find f procs in
        proc g_vars q l_vars ps triple
    | If (b, c, c') ->
        (* ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
        let t = expr_to_term ~g_vars b in
        let wp = cmd c q in
        let wp' = cmd c' q in
        T.(t_and (t_implies t wp) (t_implies (t_not t) wp'))
    | While (inv, b, c) ->
        (* inv
            /\ forall y_i.
                (((b /\ inv) -> wlp(c, inv))
                /\ ((~b /\ inv) -> q))[x_i <- y_i] *)
        let guard = expr_to_term ~g_vars b in
        let inv = Logic.translate_term ~g_vars inv in
        let s = cmd c inv in
        let iterate = T.(t_implies (t_and guard inv) s) in
        let postcond = T.(t_implies (t_and (t_not guard) inv) q) in
        T.(t_and inv (T.t_and iterate postcond))
end

let merge_in vm x =
  match Vars.find_opt x vm with
  | None -> Vars.(add x (create_fresh x)) vm
  | Some _ -> vm

let verify_procedure ?timeout ~is_main g_vars procs ((t : Triple.t), l_vars) =
  let p, q =
    if is_main then
      (Logic.translate_term ~g_vars t.p, Logic.translate_term ~g_vars t.q)
    else
      ( Logic.translate_term ~g_vars ~l_vars t.p,
        Logic.translate_term ~g_vars ~l_vars t.q )
  in
  let p_gen =
    Wlp.(sub_old_vars ~g_vars ~l_vars t.ws @@ cmd procs ~g_vars ~l_vars t.c q)
  in
  let merged_vars =
    List.fold_left merge_in (Vars.union l_vars g_vars) t.ps |> list_of_var_map
  in
  debug_print t.f merged_vars p q p_gen;
  Smt.Prover.prove timeout Arith.base_task merged_vars (T.t_implies p p_gen)

exception Proc_invalid of string

let verify ?timeout program =
  let procs, (main, globals) = split_last program in
  let f ~is_main procs proc =
    let (t : Triple.t) = fst proc in
    match verify_procedure ?timeout ~is_main globals procs proc with
    | Valid -> Proc_map.add t.f proc procs
    | Invalid | Failed _ ->
        let f = (fst proc).f in
        raise (Proc_invalid f)
  in
  try
    let proc_map = List.fold_left (f ~is_main:false) Proc_map.empty procs in
    let (_ : (Triple.t * Vars.t) Proc_map.t) =
      f ~is_main:true proc_map (main, globals)
    in
    Smt.Prover.Valid
  with Proc_invalid s ->
    if debug then Printf.printf "Procedure '%s' cannot be verified\n" s;
    Smt.Prover.Invalid
