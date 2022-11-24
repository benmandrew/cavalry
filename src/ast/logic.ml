open Why3
module T = Term
module VarMap = Map.Make (String)

type var_map = T.vsymbol VarMap.t

(* Untyped AST to play nice with the Menhir parser generator *)
type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | UNot of ut_expr
  | UAnd of ut_expr * ut_expr
  | UOr of ut_expr * ut_expr
  | UImpl of ut_expr * ut_expr
  | UEq of ut_expr * ut_expr
  | UNeq of ut_expr * ut_expr
  | ULt of ut_expr * ut_expr
  | ULeq of ut_expr * ut_expr
  | UGt of ut_expr * ut_expr
  | UGeq of ut_expr * ut_expr
  | UPlus of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr

let rec translate_term vars =
  let f = translate_term vars in
  function
  | UInt v -> T.t_nat_const v
  | UBool b -> if b then T.t_bool_true else T.t_bool_false
  | UVar x ->
      let symbol = VarMap.find x vars in
      T.t_var symbol
  | UNot e -> T.t_not (f e)
  | UAnd (e0, e1) -> T.t_and (f e0) (f e1)
  | UOr (e0, e1) -> T.t_or (f e0) (f e1)
  | UImpl (e0, e1) -> T.t_implies (f e0) (f e1)
  | UEq (e0, e1) -> T.t_equ (f e0) (f e1)
  | UNeq _ -> failwith "Not Implemented"
  | ULt (e0, e1) -> Arith.lt (f e0) (f e1)
  | ULeq _ | UGt _ | UGeq _ -> failwith "Not Implemented"
  | UPlus (e0, e1) -> Arith.plus (f e0) (f e1)
  | UMul (e0, e1) -> Arith.mul (f e0) (f e1)
