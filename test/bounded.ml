open Core
open Cavalry
open Smt.Prover

let verify ?(machine_int = false) path =
  Hoare.verify ~machine_int ?timeout:(Some 5.) (Main.get_ast path)

let is_valid = function Valid -> true | Invalid | Failed _ -> false
let is_invalid = function Invalid -> true | Valid | Failed _ -> false

(* The same program is provable over unbounded integers but rejected under
   63-bit machine integers, because [x <- x + 1] overflows when [x = max_int].
   This is the point of the feature, not a regression. *)
let%test_unit "machine-int: unbounded increment is rejected" =
  [%test_result: bool] ~expect:true (is_valid (verify "verify_true_succ.cav"));
  [%test_result: bool] ~expect:true
    (is_invalid (verify ~machine_int:true "verify_true_succ.cav"))

(* With the counter bounded by the precondition, [x + 1] provably stays in
   range, so the machine-integer proof goes through. *)
let%test_unit "machine-int: bounded increment still verifies" =
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_bounded_succ.cav"))

(* Loops: the invariant + the in_bounds-guarded havoc must let a bounded loop
   verify, while an unbounded counter is rejected because [i + 1] can overflow.
   Both verify over unbounded integers. *)
let%test_unit "machine-int: bounded loop verifies" =
  [%test_result: bool] ~expect:true
    (is_valid (verify "verify_bounded_loop.cav"));
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_bounded_loop.cav"))

let%test_unit "machine-int: unbounded loop is rejected" =
  [%test_result: bool] ~expect:true
    (is_valid (verify "verify_unbounded_loop.cav"));
  [%test_result: bool] ~expect:true
    (is_invalid (verify ~machine_int:true "verify_unbounded_loop.cav"))
