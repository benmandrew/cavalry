open Core
open Ast
open Ast.Program

(* Build a [(Triple.t * Vars.t) list] the way [Main.get_ast] would, from a
   hand-written [main] body, and emit OCaml for it. *)
let emit_main (c : cmd) : string =
  let main : Triple.t =
    {
      p = Logic.Bool true;
      q = Logic.Bool true;
      ws = [];
      f = "main";
      ps = [];
      c;
    }
  in
  Var_collection.collect [ main ] |> Cavalry.Compile.emit

let contains ~substring s = String.is_substring s ~substring

let%test_unit "Compile.emit - straight-line arithmetic" =
  (* x <- 3; y <- x + 4; print(y); x *)
  let body =
    Seq
      ( Assgn ("x", Value (Int 3)),
        Seq
          ( Assgn ("y", Plus (Value (VarInst "x"), Value (Int 4))),
            Seq (Print (Value (VarInst "y")), IntExpr (Value (VarInst "x"))) )
      )
  in
  let out = emit_main body in
  let want =
    [
      (* assignment targets become mutable refs *)
      "let x = ref Z.zero";
      "let y = ref Z.zero";
      (* unbounded arithmetic, not native + *)
      "Z.add";
      "(Z.of_int (3))";
      "(Z.of_int (4))";
      (* reads of a mutable are dereferenced *)
      "!x";
      (* print keeps the interpreter's decimal-per-line format *)
      "print_string (Z.to_string";
    ]
  in
  List.iter want ~f:(fun substring ->
      if not (contains ~substring out) then
        raise_s
          [%message
            "emitted OCaml missing expected fragment"
              (substring : string)
              (out : string)])

let%test_unit "Compile.emit - result epilogue prints main's value" =
  let body = Seq (Assgn ("x", Value (Int 1)), IntExpr (Value (VarInst "x"))) in
  let out = emit_main body in
  [%test_pred: string] (contains ~substring:"let _result =") out

let%test_unit "Compile.emit - if compiles both branches" =
  (* if x < 5 then x else 0 end *)
  let body =
    If
      ( Lt (Value (VarInst "x"), Value (Int 5)),
        IntExpr (Value (VarInst "x")),
        IntExpr (Value (Int 0)) )
  in
  let out = emit_main body in
  List.iter [ "(if "; "Z.lt"; "then"; "else" ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - while emits a Zarith-guarded loop" =
  (* while i < 10 do invariant { true } i <- i + 1 end *)
  let body =
    While
      ( Logic.Bool true,
        Lt (Value (VarInst "i"), Value (Int 10)),
        Assgn ("i", Plus (Value (VarInst "i"), Value (Int 1))) )
  in
  let out = emit_main body in
  List.iter [ "while "; "Z.lt"; "do"; "done"; "i := " ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - procedures are rejected until milestone 3" =
  let body = Proc ("f", []) in
  match emit_main body with
  | _ -> assert false
  | exception Cavalry.Compile.Unsupported _ -> ()
