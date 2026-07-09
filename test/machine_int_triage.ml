open Core
open Cavalry
open Smt.Prover

let mverify path =
  Hoare.verify ~machine_int:true ?timeout:(Some 5.) (Main.get_ast path)

let is_valid = function Valid -> true | Invalid | Failed _ -> false

(* Classification of every verify_true_* fixture under 63-bit machine integers.
   A fixture verifies iff all its arithmetic is provably in range. The Invalid
   ones each contain genuinely unbounded arithmetic (an unconstrained [x + 1],
   [x + a], or a loop counter its invariant does not bound), so their rejection
   is correct behaviour, not incompleteness. The Valid ones are bounded by
   concrete precondition values, a guard, or the loop invariant. This locks the
   classification down as a regression check on both directions: breaking the
   range hypothesis would flip a bounded fixture to Invalid, and dropping a
   [safe] obligation would flip an unbounded one to Valid. *)
let classification =
  [
    (* provably bounded -> still Valid under machine integers *)
    ("verify_true_bigmul.cav", true);
    ("verify_true_variable.cav", true);
    ("verify_true_if.cav", true);
    ("verify_true_disjunction.cav", true);
    ("verify_true_implication.cav", true);
    ("verify_true_negative.cav", true);
    ("verify_true_while.cav", true);
    (* unconstrained arithmetic -> now correctly rejected *)
    ("verify_true_succ.cav", false);
    ("verify_true_neq.cav", false);
    ("verify_true_read_global.cav", false);
    ("verify_true_add_proc.cav", false);
    ("verify_true_frame.cav", false);
    ("verify_true_fib_proc.cav", false);
    ("verify_true_euclid.cav", false);
    ("verify_true_proc_empty.cav", false);
    ("verify_true_if_local_guard.cav", false);
    ("verify_true_nested_while.cav", false);
  ]

let%test_unit "machine-int classification of verify_true fixtures" =
  List.iter classification ~f:(fun (path, expect) ->
      [%test_result: bool] ~message:path ~expect (is_valid (mverify path)))
