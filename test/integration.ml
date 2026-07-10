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

(* Truncated division and remainder: 20 / 6 = 3, 20 % 6 = 2 -> 3*10 + 2. *)
let%test_unit "Main.exec div and mod" =
  [%test_result: int] (Main.exec "exec_div.cav") ~expect:32

(* Arrays: create, write two cells, read them back: 42 + 8. *)
let%test_unit "Main.exec array" =
  [%test_result: int] (Main.exec "exec_array.cav") ~expect:50

(* [len] as a program expression: len(a) + a[6] = 7 + 3. *)
let%test_unit "Main.exec array len" =
  [%test_result: int] (Main.exec "exec_array_len.cav") ~expect:10

(* Procedure array write propagates to the caller (a[0] = 7) while the caller's
   own write (a[1] = 4) is preserved across the call: 7 + 4. *)
let%test_unit "Main.exec array via procedure" =
  [%test_result: int] (Main.exec "exec_array_proc.cav") ~expect:11

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

(* Division/remainder identity [x = q*y + r] holds for a non-zero divisor; the
   divisor-non-zero obligation is discharged from the precondition [y > 0]. *)
let%test_unit "Main.verify true div" = check_verify "verify_true_div.cav" Valid

(* Arrays: writes update the addressed cell and frame the rest ([a\[3\]]/[a\[5\]]
   independent), [len] is preserved, and every index is in bounds. *)
let%test_unit "Main.verify true array write" =
  check_verify "verify_true_array_write.cav" Valid

(* [array(n)] yields a length-[n] all-zeros array: a universally quantified
   postcondition over every in-bounds index. *)
let%test_unit "Main.verify true array init" =
  check_verify "verify_true_array_init.cav" Valid

(* Flagship: a loop filling an array, carrying a quantified invariant over the
   prefix written so far, establishes a quantified postcondition. Exercises
   array havoc, the quantifier, and the index-bounds obligation together. *)
let%test_unit "Main.verify true array fill" =
  check_verify "verify_true_array_fill.cav" Valid

(* Existential in a postcondition: some in-bounds index holds 5 (witness k = 1,
   which the prover can find -- unlike a bare arithmetic existential). *)
let%test_unit "Main.verify true exists" =
  check_verify "verify_true_exists.cav" Valid

(* A procedure that [writes { a }] an array global: the caller havocs the array
   and recovers the written cell from the procedure's [ensures]. Exercises the
   array path through [Wlp.proc]. *)
let%test_unit "Main.verify true array procedure" =
  check_verify "verify_true_array_proc.cav" Valid

(* Two arrays are independent maps: writing [a] does not disturb [b]. *)
let%test_unit "Main.verify true array framing" =
  check_verify "verify_true_array_frame.cav" Valid

(* Nested quantifiers: every pair of in-bounds elements of a freshly created
   (all-zeros) array is equal. *)
let%test_unit "Main.verify true nested forall" =
  check_verify "verify_true_nested_forall.cav" Valid

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

(* An `if` guard inside a procedure may reference a procedure-local (here the
   parameter `a`); the guard must resolve through l_vars, not globals only. *)
let%test_unit "Main.verify true if local guard" =
  check_verify "verify_true_if_local_guard.cav" Valid

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

(* Well-definedness: [q <- x / y] with an unconstrained divisor cannot discharge
   the [y <> 0] obligation, so verification fails even though the postcondition
   is [true]. (Like the nonlinear case, the prover cannot close the goal within
   the timeout, which the pipeline reports as Invalid.) *)
let%test_unit "Main.verify false div by zero" =
  check_verify "verify_false_div_by_zero.cav" Invalid

(* Array bounds: an out-of-range element write ([a\[20\]] into a length-10
   array) fails its [0 <= i < len(a)] obligation. *)
let%test_unit "Main.verify false array write out of bounds" =
  check_verify "verify_false_array_write_oob.cav" Invalid

(* Array bounds: an out-of-range element *read* fails the same obligation,
   imposed on reads via [defined]. *)
let%test_unit "Main.verify false array read out of bounds" =
  check_verify "verify_false_array_read_oob.cav" Invalid

(* [array(n)] with an unconstrained [n] cannot discharge its [0 <= n]
   well-definedness obligation, so it is rejected. *)
let%test_unit "Main.verify false array negative length" =
  check_verify "verify_false_array_negative_length.cav" Invalid

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

(* ===== Soundness regressions (previously unsound, now fixed) ===== *)

(* Framing: a procedure that assigns the global `y` without listing it in
   `writes { }` must be rejected -- otherwise a caller's WLP would not havoc y
   and could "prove" y = 1 while the interpreter yields 99 (see below). *)
let%test_unit "Main.verify false undeclared write" =
  check_verify "verify_false_writes_undeclared.cav" Invalid

let%test_unit "Main.exec undeclared write mutates global" =
  [%test_result: int]
    (Main.exec "verify_false_writes_undeclared.cav")
    ~expect:99

(* Loop rule: with the invariant `true` the loop cannot establish x = 999, so
   verification must fail. Before the loop-modified variables were havoc'd,
   the obligations were only checked at the entry state and this "verified"
   despite the loop ending at x = 10. *)
let%test_unit "Main.verify false loop cannot prove postcondition" =
  check_verify "verify_false_loop_unquantified.cav" Invalid

let%test_unit "Main.exec loop reaches its natural bound" =
  [%test_result: int]
    (Main.exec "verify_false_loop_unquantified.cav")
    ~expect:10
