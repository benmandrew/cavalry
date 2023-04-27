open Core
open Cavalry.Main
open Ast.Program
open Ast.Runtime
open Ast

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
  | Assgn (s0, e0), Assgn (s1, e1) -> String.equal s0 s1 && equal_expr e0 e1
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
  (* x := 1 + 2; if 1 = 2 then 5 else x * 5 end *)
  let expected =
    Seq
      ( Assgn ("x", Plus (Value (Int 1), Value (Int 2))),
        If
          ( Eq (Value (Int 1), Value (Int 2)),
            IntExpr (Value (Int 5)),
            IntExpr (Mul (Value (VarInst "x"), Value (Int 5))) ) )
  in
  let ut =
    USeq
      ( UAssgn ("x", UPlus (UInt 1, UInt 2)),
        UIf (UEq (UInt 1, UInt 2), UInt 5, UMul (UVar "x", UInt 5)) )
  in
  let result = translate_cmd ut in
  test_ast_eq ~expected result

let%test_unit "Ast.Runtime.exec - assgn" =
  (* x := 1 + 2; x * 5 *)
  let c =
    Seq
      ( Assgn ("x", Plus (Value (Int 1), Value (Int 2))),
        IntExpr (Mul (Value (VarInst "x"), Value (Int 5))) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:15

let%test_unit "Ast.Runtime.exec - if" =
  (* if 1 = 2 then 5 else 7 end *)
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
  (* x := 1; y := x + 2; y *)
  let c =
    Seq
      ( Assgn ("x", Value (Int 1)),
        Seq
          ( Assgn ("y", Plus (Value (VarInst "x"), Value (Int 2))),
            IntExpr (Value (VarInst "y")) ) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:3

let%test_unit "Ast.Runtime.exec - unbound" =
  (* x + 2 *)
  let c = IntExpr (Plus (Value (VarInst "x"), Value (Int 2))) in
  let main = { f = ""; ps = []; c } in
  let result = Exn.does_raise (fun () -> exec [ main ]) in
  [%test_result: bool] result ~expect:true

let%test_unit "Ast.Runtime.exec - while" =
  let dummy_invariant = Logic.(Eq (Int 1, Int 2)) in
  (* x := 0; i := 0; while i < 10 do x := x + i; i := i + 1 end; x *)
  let c =
    Seq
      ( Assgn ("x", Value (Int 0)),
        Seq
          ( Assgn ("i", Value (Int 0)),
            Seq
              ( While
                  ( dummy_invariant,
                    Lt (Value (VarInst "i"), Value (Int 10)),
                    Seq
                      ( Assgn
                          ("x", Plus (Value (VarInst "x"), Value (VarInst "i"))),
                        Assgn ("i", Plus (Value (VarInst "i"), Value (Int 1)))
                      ) ),
                IntExpr (Value (VarInst "x")) ) ) )
  in
  let main = { f = ""; ps = []; c } in
  let result = exec [ main ] in
  [%test_result: int] result ~expect:45

let%test_unit "Ast.Runtime.exec - function" =
  (* f(z) = let y = z + 1; x := x + y *)
  let fn : Triple.ut_t =
    {
      f = "f";
      ps = [ "z" ];
      u =
        USeq
          ( ULet ("y", UPlus (UVar "z", UInt 1)),
            UAssgn ("x", UPlus (UVar "x", UVar "y")) );
      p = Logic.(Leq (Int 0, Int 0));
      q = Logic.(Leq (Int 0, Int 0));
      ws = [];
    }
  in
  (* x := 2; y := 3; f(5); [ret] *)
  let main ret : Triple.ut_t =
    {
      f = "";
      ps = [];
      u =
        USeq
          ( UAssgn ("x", UInt 2),
            USeq (UAssgn ("y", UInt 3), USeq (UProc ("f", [ UInt 5 ]), UVar ret))
          );
      p = Logic.(Leq (Int 0, Int 0));
      q = Logic.(Leq (Int 0, Int 0));
      ws = [];
    }
  in
  let program ret =
    List.map [ fn; main ret ] ~f:(fun ut -> Triple.translate ut |> to_proc_t)
  in
  let result = exec (program "x") in
  [%test_result: int] result ~expect:8;
  let result = exec (program "y") in
  [%test_result: int] result ~expect:3
