module T = Why3.Term

module VarMap : sig
  type 'a t
end

type var_map = T.vsymbol VarMap.t

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

val translate_term : T.vsymbol VarMap.t -> ut_expr -> T.term
