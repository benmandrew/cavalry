open Why3
open Cavalry.Ast

module VarSet = Set.Make (struct
  type t = string * Term.lsymbol

  let compare (a, _) (b, _) = String.compare a b
end)

let val_to_term : type a. VarSet.t -> a value -> Term.term * VarSet.t =
 fun vars v ->
  match v with
  | Int v -> (Term.t_nat_const v, vars)
  | Bool b -> if b then (Term.t_bool_true, vars) else (Term.t_bool_false, vars)
  | VarInst str ->
      let symbol = Term.create_psymbol (Ident.id_fresh str) [] in
      (Term.ps_app symbol [], VarSet.add (str, symbol) vars)

let rec expr_to_term : type a. VarSet.t -> a expr -> Term.term * VarSet.t =
 fun vars e ->
  match e with
  | Value v -> val_to_term vars v
  | Eq (a, b) ->
      let a', vars' = expr_to_term vars a in
      let b', vars'' = expr_to_term vars' b in
      Term.t_equ a' b', vars''
  | Plus (a, b) ->
      let a', vars' = expr_to_term vars a in
      let b', vars'' = expr_to_term vars' b in
      Term.t_equ a' b', vars''
  | Mul (a, b) ->
      let a', vars' = expr_to_term vars a in
      let b', vars'' = expr_to_term vars' b in
      Term.t_equ a' b', vars''

let rec wlp : type a. a expr -> Term.term -> Term.term =
 fun v q ->
  match v with
  | Value _ -> q
  | Seq (e, e') -> wlp e (wlp e' q)
  | Assgn (x, e) -> q (* Requires expression->term transformation *)
  | If (e, e', e'') ->
      q
      (* Term.(t_and (t_implies e (wlp e' q)) (t_implies e (wlp e'' q))) *)
      (* Req^^ *)
  | Plus (e, e') | Mul (e, e') | Eq (e, e') -> q

(* let verify : type a. Term.term -> Term.term -> a expr -> bool =
   fun p q v ->
    match v with
    | Value _ -> true
    | Seq (e, e') -> true
    | Assgn (x, e) -> true
    | If (e, e', e'') -> true
    | Plus (e, e') -> true
    | Mul (e, e') -> true
    | Eq (e, e') -> true *)
