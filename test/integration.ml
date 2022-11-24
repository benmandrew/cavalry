open Core
open Why3
open Cavalry

let%test_unit "Main.exec" =
  let result = Main.exec "test_exec.cvl" in
  [%test_result: int] result ~expect:182

let%test_unit "Main.verify true" =
  let ast = Main.get_ast "test_verify_true.cvl" in
  let vars = Hoare.collect_variables ast in
  let x = Term.t_var (Hoare.get_var vars "x") in
  let q = Ast.Arith.lt x (Term.t_nat_const 190) in
  let result = Main.verify vars ast Term.t_true q in
  [%test_result: bool] result ~expect:true

let%test_unit "Main.verify false" =
  let ast = Main.get_ast "test_verify_false.cvl" in
  let vars = Hoare.collect_variables ast in
  let x = Term.t_var (Hoare.get_var vars "x") in
  let q = Ast.Arith.lt x (Term.t_nat_const 80) in
  let result = Main.verify vars ast Term.t_true q in
  [%test_result: bool] result ~expect:false
