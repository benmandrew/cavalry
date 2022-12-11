open Core
open Cavalry

let%test_unit "Main.exec" =
  let result = Main.exec "test_exec.cvl" in
  [%test_result: int] result ~expect:182

let%test_unit "Main.verify true" =
  let ast = Main.get_ast "test_verify_true.cvl" in
  let vars = Ast.Var_collection.collect ast in
  let result = Main.verify vars ast in
  [%test_result: bool] result ~expect:true

let%test_unit "Main.verify false" =
  let ast = Main.get_ast "test_verify_false.cvl" in
  let vars = Ast.Var_collection.collect ast in
  let result = Main.verify vars ast in
  [%test_result: bool] result ~expect:false
