open Core
open Cavalry

let debug = false

(* Verify a fixture through the full pipeline (parse -> type -> WLP -> SMT). *)
let verify path = Main.verify ~debug ?timeout:(Some 5.) (Main.get_ast path)

let check_verify path expect =
  [%test_result: Smt.Prover.result] (verify path) ~expect

(* ===== Interpreter (`cav run`) ===== *)

let%test_unit "Main.exec if" =
  [%test_result: int] (Main.exec "exec_if.cav") ~expect:182

let%test_unit "Main.exec while" =
  [%test_result: int] (Main.exec "exec_while.cav") ~expect:45

(* Procedure call: frame + return value. f(y) adds y (=5) to x (=2) -> 7. *)
let%test_unit "Main.exec procedure call" =
  [%test_result: int] (Main.exec "exec_proc.cav") ~expect:7

(* Two nested loops each running 3 times -> 9 increments. *)
let%test_unit "Main.exec nested while" =
  [%test_result: int] (Main.exec "exec_nested_while.cav") ~expect:9

(* Negatives only exist via `0 - n` (literals lex as naturals). *)
let%test_unit "Main.exec negative" =
  [%test_result: int] (Main.exec "exec_negative.cav") ~expect:(-5)

(* Reading an unbound variable at runtime raises (preconditions do not
   initialize variables; they are assertions only). *)
let%test_unit "Main.exec unbound raises" =
  let raised = Exn.does_raise (fun () -> Main.exec "exec_unbound.cav") in
  [%test_result: bool] raised ~expect:true

(* ===== verify: expected Valid ===== *)

let%test_unit "Main.verify true if" = check_verify "verify_true_if.cav" Valid

let%test_unit "Main.verify true while" =
  check_verify "verify_true_while.cav" Valid

let%test_unit "Main.verify true succ" =
  check_verify "verify_true_succ.cav" Valid

let%test_unit "Main.verify true procedure empty" =
  check_verify "verify_true_proc_empty.cav" Valid

let%test_unit "Main.verify true add procedure" =
  check_verify "verify_true_add_proc.cav" Valid

let%test_unit "Main.verify true fib procedure" =
  check_verify "verify_true_fib_proc.cav" Valid

let%test_unit "Main.verify true euclid" =
  check_verify "verify_true_euclid.cav" Valid

(* Multi-variable dataflow: classic three-step swap. *)
let%test_unit "Main.verify true variable" =
  check_verify "verify_true_variable.cav" Valid

(* Framing: a call that `writes { x }` must leave y untouched. *)
let%test_unit "Main.verify true frame" =
  check_verify "verify_true_frame.cav" Valid

(* A procedure may read a global it does not declare in `writes`. *)
let%test_unit "Main.verify true read global" =
  check_verify "verify_true_read_global.cav" Valid

(* Assertion operators absent from other fixtures: || , -> , != . *)
let%test_unit "Main.verify true disjunction" =
  check_verify "verify_true_disjunction.cav" Valid

let%test_unit "Main.verify true implication" =
  check_verify "verify_true_implication.cav" Valid

let%test_unit "Main.verify true neq" = check_verify "verify_true_neq.cav" Valid

(* Negative results reasoned about via `0 - n`. *)
let%test_unit "Main.verify true negative" =
  check_verify "verify_true_negative.cav" Valid

(* Integers are unbounded (Why3 `int`): a product that overflows 63-bit
   machine ints still verifies. *)
let%test_unit "Main.verify true bigmul" =
  check_verify "verify_true_bigmul.cav" Valid

(* Nested loops each carrying their own invariant. *)
let%test_unit "Main.verify true nested while" =
  check_verify "verify_true_nested_while.cav" Valid

(* ===== verify: expected Invalid ===== *)

let%test_unit "Main.verify false" = check_verify "verify_false.cav" Invalid

(* Loop invariant that does not hold on entry. *)
let%test_unit "Main.verify false invariant on entry" =
  check_verify "verify_false_inv_entry.cav" Invalid

(* Procedure body violates its own `ensures` (x <- x + 2 vs x = _x + 1). *)
let%test_unit "Main.verify false procedure ensures" =
  check_verify "verify_false_proc_ensures.cav" Invalid

(* Caller does not establish the callee's `requires` (needs x >= 10). *)
let%test_unit "Main.verify false procedure requires" =
  check_verify "verify_false_proc_requires.cav" Invalid

(* Prover incompleteness: a *true* nonlinear fact (x,y >= 2 => x*y > x)
   that Alt-Ergo cannot discharge, so verify reports Invalid, not Valid. *)
let%test_unit "Main.verify false nonlinear incompleteness" =
  check_verify "verify_false_nonlinear.cav" Invalid

(* ===== Type errors: get_ast raises during translation ===== *)

(* `if` guard must be boolean; `x + 1` is an int expr. *)
let%test_unit "Main.get_ast type error if guard" =
  let raised =
    Exn.does_raise (fun () -> Main.get_ast "type_error_if_guard.cav")
  in
  [%test_result: bool] raised ~expect:true

(* Assignment RHS must be an int expr; `1 < 2` is boolean. *)
let%test_unit "Main.get_ast type error assign bool" =
  let raised =
    Exn.does_raise (fun () -> Main.get_ast "type_error_assign_bool.cav")
  in
  [%test_result: bool] raised ~expect:true

(* ===== Prover answer -> result mapping (stubbed, no real solver call) ===== *)

let%test_unit "Prover.result_of_answer mapping" =
  let open Smt.Prover in
  let f a = result_of_answer ~output:"boom" a in
  [%test_result: result] (f Why3.Call_provers.Valid) ~expect:Valid;
  [%test_result: result] (f Why3.Call_provers.Invalid) ~expect:Invalid;
  [%test_result: result] (f (Why3.Call_provers.Unknown "")) ~expect:Invalid;
  (* Timeout / OOM / step-limit / failure all collapse to Failed, carrying the
     prover's output. This is the branch a real timeout would exercise. *)
  [%test_result: result] (f Why3.Call_provers.Timeout) ~expect:(Failed "boom");
  [%test_result: result]
    (f Why3.Call_provers.OutOfMemory)
    ~expect:(Failed "boom")

(* ===== KNOWN UNSOUNDNESS (regression markers) =====
   These fixtures verify as Valid but are semantically wrong. They pin the
   current (buggy) behavior; when the underlying bug is fixed, the expectation
   should flip to Invalid. *)

(* BUG 1 (framing): `writes { }` is not checked against the variables a
   procedure actually assigns. `f` mutates the global `y` without declaring
   it, so the caller's WLP never havocs y and "proves" y = 1, yet the
   interpreter yields 99. See Wlp.proc / sub_written_vars in src/hoare.ml. *)
let%test_unit "KNOWN UNSOUND: writes clause not enforced (verify)" =
  check_verify "unsound_writes.cav" Valid

let%test_unit "KNOWN UNSOUND: writes clause not enforced (run diverges)" =
  [%test_result: int] (Main.exec "unsound_writes.cav") ~expect:99

(* BUG 2 (loop rule): the While case in src/hoare.ml never universally
   quantifies the loop-modified variables (the promised `forall y_i. (..)[x_i
   <- y_i]` is missing), so invariant preservation and the `!guard` exit are
   only checked at the loop's *entry* state. Any postcondition "verifies" as
   long as the guard is initially true; here x = 999 passes though x ends at
   10. *)
let%test_unit "KNOWN UNSOUND: loop rule not quantified (verify)" =
  check_verify "unsound_loop.cav" Valid

let%test_unit "KNOWN UNSOUND: loop rule not quantified (run diverges)" =
  [%test_result: int] (Main.exec "unsound_loop.cav") ~expect:10
