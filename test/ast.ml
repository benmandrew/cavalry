open Core
open Cavalry.Ast

let rec equal_expr : type a b. a expr -> b expr -> bool =
 fun e0 e1 ->
  match (e0, e1) with
  | Value (Int v0), Value (Int v1) -> v0 = v1
  | Value (Bool v0), Value (Bool v1) -> Bool.equal v0 v1
  | Value (VarInst v0), Value (VarInst v1) -> String.equal v0 v1
  | Let (s0, e0, e0'), Let (s1, e1, e1') ->
      String.equal s0 s1 && equal_expr e0 e1 && equal_expr e0' e1'
  | If (e0, e0', e0''), If (e1, e1', e1'') ->
      equal_expr e0 e1 && equal_expr e0' e1' && equal_expr e0'' e1''
  | Eq (e0, e0'), Eq (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | Plus (e0, e0'), Plus (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | Mul (e0, e0'), Mul (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | _ -> false

exception E of string * Sexp.t [@@deriving sexp]

let format_exn_sexp expected actual =
  Sexp.List
    [
      Sexp.List [ Sexp.Atom "Expected:"; sexp_of_expr sexp_of_int expected ];
      Sexp.List [ Sexp.Atom "Actual:"; sexp_of_expr sexp_of_int actual ];
    ]

let test_ast_eq expected actual =
  if not (equal_expr expected actual) then
    raise (E ("AST not equal", format_exn_sexp expected actual))

let%test_unit "Ast.type_expr" =
  let t =
    Let
      ( "x",
        Plus (Value (Int 2), Value (Int 2)),
        Mul (Value (VarInst "x"), Value (Int 5)) )
  in
  let ut = ULet ("x", UPlus (UInt 1, UInt 2), UMul (UVar "x", UInt 5)) in
  let t' = type_expr ut in
  (* [%test_result: int expr] ~equal:equal_expr t' ~expect *)
  test_ast_eq t t'
