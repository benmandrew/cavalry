(* open Core *)
open Cavalry

(* let%test_unit "Main.exec if" =
        let result = Main.exec "test_exec_if.cvl" in
        [%test_result: int] result ~expect:182

      let%test_unit "Main.exec while" =
        let result = Main.exec "test_exec_while.cvl" in
        [%test_result: int] result ~expect:45

      let%test_unit "Main.verify false" =
        let program = Main.get_ast "test_verify_false.cvl" in
        let result = Main.verify ?timeout:(Some 5.) program in
        [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Invalid

      let%test_unit "Main.verify true if" =
        let program = Main.get_ast "test_verify_true_if.cvl" in
        let result = Main.verify ?timeout:(Some 5.) program in
        [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

      let%test_unit "Main.verify true while" =
        let program = Main.get_ast "test_verify_true_while.cvl" in
        let result = Main.verify ?timeout:(Some 5.) program in
        [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

      let%test_unit "Main.verify true succ" =
        let program = Main.get_ast "test_verify_true_succ.cvl" in
        let result = Main.verify ?timeout:(Some 5.) program in
        [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid

   let%test_unit "Main.verify true procedure empty" =
     let program = Main.get_ast "test_verify_true_proc_empty.cvl" in
     let result = Main.verify ?timeout:(Some 5.) program in
     [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid *)

let%test_unit "Main.verify true succ procedure" =
  let program = Main.get_ast "test_verify_true_succ_proc.cvl" in
  let result = Main.verify ?timeout:(Some 5.) program in
  [%test_result: Smt.Prover.result] result ~expect:Smt.Prover.Valid
