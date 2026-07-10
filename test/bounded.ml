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

(* Pin the exact overflow boundary: a result of exactly max_int (2^62-1) is in
   range, but max_int + 1 is not. This is what catches an off-by-one in the
   bounds -- every other machine-int fixture sits far from the boundary. *)
let%test_unit "machine-int: exact overflow boundary (max_int)" =
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_boundary_ok.cav"));
  [%test_result: bool] ~expect:true
    (is_invalid (verify ~machine_int:true "verify_boundary_overflow.cav"))

(* The symmetric lower boundary, exercising subtraction/underflow: a result of
   exactly min_int (-2^62) is in range, but min_int - 1 is not. *)
let%test_unit "machine-int: exact underflow boundary (min_int)" =
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_underflow_ok.cav"));
  [%test_result: bool] ~expect:true
    (is_invalid (verify ~machine_int:true "verify_underflow_reject.cav"))

(* A procedure whose body is bounded by its precondition verifies under machine
   integers, and so does a caller composing with it -- the accept direction of
   the procedure-call path (the triage only covers rejections). *)
let%test_unit "machine-int: bounded procedure call verifies" =
  [%test_result: bool] ~expect:true
    (is_valid (verify "verify_bounded_proc.cav"));
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_bounded_proc.cav"))

(* Arrays coexist with machine-int mode: the map-typed variables must be skipped
   by the [in_bounds] antecedent and the havoc guard (they are not integers),
   and in-range element writes still verify. Valid under both models. *)
let%test_unit "machine-int: arrays verify" =
  [%test_result: bool] ~expect:true
    (is_valid (verify "verify_true_array_machine_int.cav"));
  [%test_result: bool] ~expect:true
    (is_valid (verify ~machine_int:true "verify_true_array_machine_int.cav"))
