open Core
open Ast
open Ast.Program

(* Build a [(Triple.t * Vars.t) list] the way [Main.get_ast] would, from a
   hand-written [main] body, and emit OCaml for it. *)
let triple ?(f = "main") ?(ps = []) ?(ws = []) (c : cmd) : Triple.t =
  { p = Logic.Bool true; q = Logic.Bool true; ws; f; ps; c }

let emit_main (c : cmd) : string =
  Var_collection.collect [ triple c ] |> Cavalry.Compile.emit

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
      (* assignment targets become prefixed global refs *)
      "let g_x = ref Z.zero";
      "let g_y = ref Z.zero";
      (* unbounded arithmetic, not native + *)
      "Z.add";
      "(Z.of_int (3))";
      "(Z.of_int (4))";
      (* reads of a global are dereferenced *)
      "!g_x";
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
  List.iter [ "while "; "Z.lt"; "do"; "done"; "g_i := " ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - procedure: formals are locals, Assgn hits global"
    =
  (* procedure f (a) = ... writes { x } x <- x + a end
     { true } x <- 2; y <- 5; f(y); x { true } *)
  let proc =
    triple ~f:"f" ~ps:[ "a" ] ~ws:[ "x" ]
      (Assgn ("x", Plus (Value (VarInst "x"), Value (VarInst "a"))))
  in
  let main =
    triple
      (Seq
         ( Assgn ("x", Value (Int 2)),
           Seq
             ( Assgn ("y", Value (Int 5)),
               Seq
                 ( Proc ("f", [ Value (VarInst "y") ]),
                   IntExpr (Value (VarInst "x")) ) ) ))
  in
  let out = Var_collection.collect [ proc; main ] |> Cavalry.Compile.emit in
  List.iter
    [
      "let p_f l_a =";
      (* formal [a] read as a local *)
      "l_a";
      (* the [<-] target is the shared global, not the formal *)
      "g_x :=";
      (* call passes the actual through the caller's global [y] *)
      "(p_f (!g_y))";
    ] ~f:(fun substring ->
      if not (contains ~substring out) then
        raise_s
          [%message "missing fragment" (substring : string) (out : string)])

let%test_unit "Compile.emit - zero-arg procedure takes unit" =
  let proc = triple ~f:"f" (Assgn ("y", Value (Int 1))) in
  let main = triple (Proc ("f", [])) in
  let out = Var_collection.collect [ proc; main ] |> Cavalry.Compile.emit in
  List.iter [ "let p_f () ="; "(p_f ())" ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)
