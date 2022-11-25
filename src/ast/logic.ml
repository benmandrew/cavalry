open Why3
module T = Term

(* Untyped AST to play nice with the Menhir parser generator *)
type expr =
  | Int of int
  | Bool of bool
  | Var of string
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Impl of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Leq of expr * expr
  | Gt of expr * expr
  | Geq of expr * expr
  | Plus of expr * expr
  | Mul of expr * expr

let rec translate_term vars e =
  let f = translate_term vars in
  e |> function
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_true else T.t_false
  | Var x ->
      let symbol = Vars.find x vars in
      T.t_var symbol
  | Not e -> T.t_not (f e)
  | And (e0, e1) -> T.t_and (f e0) (f e1)
  | Or (e0, e1) -> T.t_or (f e0) (f e1)
  | Impl (e0, e1) -> T.t_implies (f e0) (f e1)
  | Eq (e0, e1) -> T.t_equ (f e0) (f e1)
  | Neq _ -> failwith "Not Implemented"
  | Lt (e0, e1) -> Arith.lt (f e0) (f e1)
  | Leq _ | Gt _ | Geq _ -> failwith "Not Implemented"
  | Plus (e0, e1) -> Arith.plus (f e0) (f e1)
  | Mul (e0, e1) -> Arith.mul (f e0) (f e1)
