open Core
open Cavalry.Main
open Ast.Program
open Ast.Runtime

let rec equal_expr : type a. a expr -> a expr -> bool =
 fun e0 e1 ->
  match (e0, e1) with
  | Value (Int v0), Value (Int v1) -> v0 = v1
  | Value (Bool v0), Value (Bool v1) -> Bool.equal v0 v1
  | Value (VarInst v0), Value (VarInst v1) -> String.equal v0 v1
  | Eq (e0, e0'), Eq (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | Plus (e0, e0'), Plus (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | Mul (e0, e0'), Mul (e1, e1') -> equal_expr e0 e1 && equal_expr e0' e1'
  | _, _ -> false

let rec equal_cmd : cmd -> cmd -> bool =
 fun e0 e1 ->
  match (e0, e1) with
  | IntExpr e0, IntExpr e1 -> equal_expr e0 e1
  | Seq (e0, e0'), Seq (e1, e1') -> equal_cmd e0 e1 && equal_cmd e0' e1'
  | EAssgn (s0, e0), EAssgn (s1, e1) -> String.equal s0 s1 && equal_expr e0 e1
  | If (e0, e0', e0''), If (e1, e1', e1'') ->
      equal_expr e0 e1 && equal_cmd e0' e1' && equal_cmd e0'' e1''
  | _, _ -> false

exception E of string * Sexp.t [@@deriving sexp]

let format_exn_sexp ~expected actual =
  Sexp.(
    List
      [
        List [ Atom "Expected:"; sexp_of_cmd expected ];
        List [ Atom "Actual:"; sexp_of_cmd actual ];
      ])

let test_ast_eq ~expected actual =
  if not (equal_cmd expected actual) then
    raise (E ("AST not equal", format_exn_sexp ~expected actual))

let%test_unit "Ast.Program.type_expr" =
  let expected =
    Seq
      ( EAssgn ("x", Plus (Value (Int 1), Value (Int 2))),
        If
          ( Eq (Value (Int 1), Value (Int 2)),
            IntExpr (Value (Int 5)),
            IntExpr (Mul (Value (VarInst "x"), Value (Int 5))) ) )
  in
  let ut =
    USeq
      ( UEAssgn ("x", UPlus (UInt 1, UInt 2)),
        UIf (UEq (UInt 1, UInt 2), UInt 5, UMul (UVar "x", UInt 5)) )
  in
  let result = translate_cmd ut in
  test_ast_eq ~expected result

let%test_unit "Ast.Runtime.exec - assgn" =
  let c =
    Seq
      ( EAssgn ("x", Plus (Value (Int 1), Value (Int 2))),
        IntExpr (Mul (Value (VarInst "x"), Value (Int 5))) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:15

let%test_unit "Ast.Runtime.exec - if" =
  let c =
    If
      ( Eq (Value (Int 1), Value (Int 2)),
        IntExpr (Value (Int 5)),
        IntExpr (Value (Int 7)) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:7

let%test_unit "Ast.Runtime.exec - var-var-assgn" =
  let c =
    Seq
      ( EAssgn ("x", Value (Int 1)),
        Seq
          ( EAssgn ("y", Plus (Value (VarInst "x"), Value (Int 2))),
            IntExpr (Value (VarInst "y")) ) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:3

let%test_unit "Ast.Runtime.exec - unbound" =
  let c = IntExpr (Plus (Value (VarInst "x"), Value (Int 2))) in
  let main = { f = ""; ps = []; c } in
  let result = Exn.does_raise (fun () -> exec [ main ]) in
  [%test_result: bool] result ~expect:true

let%test_unit "Ast.Runtime.exec - while" =
  let dummy_invariant = Ast.Logic.(Eq (Int 1, Int 2)) in
  let c =
    Seq
      ( EAssgn ("x", Value (Int 0)),
        Seq
          ( EAssgn ("i", Value (Int 0)),
            Seq
              ( While
                  ( dummy_invariant,
                    Lt (Value (VarInst "i"), Value (Int 10)),
                    Seq
                      ( EAssgn
                          ("x", Plus (Value (VarInst "x"), Value (VarInst "i"))),
                        EAssgn ("i", Plus (Value (VarInst "i"), Value (Int 1)))
                      ) ),
                IntExpr (Value (VarInst "x")) ) ) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:45

let%test_unit "Ast.Runtime.exec - function" =
  let fn : Ast.Triple.ut_t =
    {
      f = "f";
      ps = [ "x" ];
      u = UPlus (UVar "x", UInt 1);
      p = Ast.Logic.(Leq (Int 0, Int 0));
      q = Ast.Logic.(Leq (Int 0, Int 0));
    }
  in
  let main : Ast.Triple.ut_t =
    {
      f = "";
      ps = [];
      u =
        USeq
          ( UEAssgn ("x", UInt 2),
            USeq (UPAssgn ("x", "f", [ UVar "x" ]), UVar "x") );
      p = Ast.Logic.(Leq (Int 0, Int 0));
      q = Ast.Logic.(Leq (Int 0, Int 0));
    }
  in
  let program =
    List.map [ fn; main ] ~f:(fun ut ->
        Ast.Triple.translate ut |> Ast.Runtime.to_proc_t)
  in
  let result = exec program in
  [%test_result: int] result ~expect:3
