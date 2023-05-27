open Core
module T = Why3.Term

type arith_expr =
  | Int of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Sub of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
[@@deriving sexp_of, show]

type logic_expr =
  | Bool of bool
  | Not of logic_expr
  | And of logic_expr * logic_expr
  | Or of logic_expr * logic_expr
  | Impl of logic_expr * logic_expr
  | Eq of arith_expr * arith_expr
  | Neq of arith_expr * arith_expr
  | Lt of arith_expr * arith_expr
  | Leq of arith_expr * arith_expr
  | Gt of arith_expr * arith_expr
  | Geq of arith_expr * arith_expr
[@@deriving sexp_of, show]

type expr = logic_expr [@@deriving sexp_of, show]

let rec translate_arith_term ~g_vars ?l_vars e =
  let f = translate_arith_term ~g_vars ?l_vars in
  e |> function
  | Int v -> T.t_nat_const v
  | Var x -> (
      match l_vars with
      | Some l_vars -> (
          match Vars.find_opt x g_vars with
          | Some symbol -> T.t_var symbol
          | None -> T.t_var @@ Vars.find x l_vars)
      | None -> T.t_var @@ Vars.find x g_vars)
  | Plus (e0, e1) -> Arith.plus (f e0) (f e1)
  | Sub (e0, e1) -> Arith.sub (f e0) (f e1)
  | Mul (e0, e1) -> Arith.mul (f e0) (f e1)

let rec translate_term ~g_vars ?l_vars e =
  let f_t = translate_term ~g_vars ?l_vars in
  let f_a = translate_arith_term ~g_vars ?l_vars in
  e |> function
  | Bool b -> if b then T.t_true else T.t_false
  | Not e -> T.t_not (f_t e)
  | And (e0, e1) -> T.t_and (f_t e0) (f_t e1)
  | Or (e0, e1) -> T.t_or (f_t e0) (f_t e1)
  | Impl (e0, e1) -> T.t_implies (f_t e0) (f_t e1)
  | Eq (e0, e1) -> Arith.eq (f_a e0) (f_a e1)
  | Neq (e0, e1) -> Arith.neq (f_a e0) (f_a e1)
  | Lt (e0, e1) -> Arith.lt (f_a e0) (f_a e1)
  | Leq (e0, e1) -> Arith.leq (f_a e0) (f_a e1)
  | Gt (e0, e1) -> Arith.gt (f_a e0) (f_a e1)
  | Geq (e0, e1) -> Arith.geq (f_a e0) (f_a e1)

let fmt = Format.formatter_of_out_channel stdout

let print_expr p =
  Core.Sexp.pp_hum fmt (sexp_of_logic_expr p);
  Format.pp_print_flush fmt ()

let print_term t =
  Why3.Pretty.print_term fmt t;
  Format.pp_print_flush fmt ()
