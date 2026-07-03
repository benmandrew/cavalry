open Core
open Cavalry

let debug = false

let%test_unit "Main.exec if" =
  let result = Main.exec "exec_if.cav" in
  [%test_result: int] result ~expect:182

let%test_unit "Main.exec while" =
  let result = Main.exec "exec_while.cav" in
  [%test_result: int] result ~expect:45

let%test_unit "Main.verify false" =
  let program = Main.get_ast "verify_false.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Invalid

let%test_unit "Main.verify true if" =
  let program = Main.get_ast "verify_true_if.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true while" =
  let program = Main.get_ast "verify_true_while.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true succ" =
  let program = Main.get_ast "verify_true_succ.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true procedure empty" =
  let program = Main.get_ast "verify_true_proc_empty.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true add procedure" =
  let program = Main.get_ast "verify_true_add_proc.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true fib procedure" =
  let program = Main.get_ast "verify_true_fib_proc.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

let%test_unit "Main.verify true euclid" =
  let program = Main.get_ast "verify_true_euclid.cav" in
  let result = Main.verify ~debug ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid
