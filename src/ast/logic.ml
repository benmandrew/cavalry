open Core
module T = Why3.Term

type arith_expr =
  | Int of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
[@@deriving sexp_of]

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
[@@deriving sexp_of]

type expr = logic_expr [@@deriving sexp_of]

let rec translate_arith_term vars e =
  let f = translate_arith_term vars in
  e |> function
  | Int v -> T.t_nat_const v
  | Var x ->
      let symbol = Vars.find x vars in
      T.t_var symbol
  | Plus (e0, e1) -> Arith.plus (f e0) (f e1)
  | Mul (e0, e1) -> Arith.mul (f e0) (f e1)

let rec translate_term vars e =
  let f_t = translate_term vars in
  let f_a = translate_arith_term vars in
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

let print_expr p =
  let formatter = Format.formatter_of_out_channel stdout in
  Core.Sexp.pp_hum formatter (sexp_of_logic_expr p);
  Format.pp_print_flush formatter ()

let print_term = Why3.Pretty.print_term (Format.formatter_of_out_channel stdout)
