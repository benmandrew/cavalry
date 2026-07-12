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

(* One-armed [if]: the first guard is taken, the second is skipped (its absent
   [else] is a no-op), so x = 5. *)
let%test_unit "Main.exec one-armed if" =
  [%test_result: int] (Main.exec "exec_if_noelse.cav") ~expect:5

(* [&&]/[||] short-circuit at runtime, so the out-of-bounds read guarded by a
   decided condition is never evaluated and the run does not raise: 2 + 30. *)
let%test_unit "Main.exec short-circuit guards" =
  [%test_result: int] (Main.exec "exec_short_circuit.cav") ~expect:32

(* [//] line comments are stripped by the lexer, so a program peppered with
   them evaluates identically to exec_if.cav: 2 + 60 * 3 = 182. *)
let%test_unit "Main.exec line comments" =
  [%test_result: int] (Main.exec "exec_comment.cav") ~expect:182

let%test_unit "Main.exec while" =
  [%test_result: int] (Main.exec "exec_while.cav") ~expect:45

(* Procedure call: frame + return value. f(y) adds y (=5) to x (=2) -> 7. *)
let%test_unit "Main.exec procedure call" =
  [%test_result: int] (Main.exec "exec_proc.cav") ~expect:7

(* A loop [variant] is verification-only: the interpreter ignores it and the
   loop runs normally, summing 0..4 = 10. *)
let%test_unit "Main.exec variant is ignored at runtime" =
  [%test_result: int] (Main.exec "exec_variant.cav") ~expect:10

(* A recursive procedure runs and terminates: sum_to(5) = 0+1+...+5 = 15. *)
let%test_unit "Main.exec recursion" =
  [%test_result: int] (Main.exec "verify_true_recursion.cav") ~expect:15

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

(* One-armed [if]: the implicit (empty) else preserves the state, so the
   postcondition holds on the guard-false path. *)
let%test_unit "Main.verify true one-armed if" =
  check_verify "verify_true_if_noelse.cav" Valid

(* Compound guard with a short-circuiting [&&]: [a[i]] need only be in bounds
   when [i < n] holds, which is exactly what the short-circuit definedness
   obligation grants. The headline case for compound boolean guards. *)
let%test_unit "Main.verify true linear search" =
  check_verify "verify_true_search.cav" Valid

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

(* A procedure may `writes` a global that never appears in `main`: it is still
   classified as a global (not a procedure-local), so the caller's havoc
   resolves it. Previously this crashed with [Var_not_found]. *)
let%test_unit "Main.verify true writes global absent from main" =
  check_verify "verify_true_writes_global_not_in_main.cav" Valid

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

(* A true nonlinear fact (x,y >= 2 => x*y > x && x*y > y). Z3 discharges the
   nonlinear reasoning that the former Alt-Ergo backend could not. *)
let%test_unit "Main.verify true nonlinear" =
  check_verify "verify_true_nonlinear.cav" Valid

(* Nested loops each carrying their own invariant. *)
let%test_unit "Main.verify true nested while" =
  check_verify "verify_true_nested_while.cav" Valid

(* An `if` guard inside a procedure may reference a procedure-local (here the
   parameter `a`); the guard must resolve through l_vars, not globals only. *)
let%test_unit "Main.verify true if local guard" =
  check_verify "verify_true_if_local_guard.cav" Valid

(* Total correctness: a [variant] measure that is non-negative and strictly
   decreases each iteration proves the loop terminates. *)
let%test_unit "Main.verify true variant (total correctness)" =
  check_verify "verify_true_variant.cav" Valid

(* Partial correctness is the default (no variant): a non-terminating loop makes
   any postcondition -- here [1 = 2] -- vacuously provable, because the loop
   never reaches the exit. This is exactly what a variant rules out. *)
let%test_unit "Main.verify true non-terminating loop (partial, vacuous)" =
  check_verify "verify_true_nonterminating_partial.cav" Valid

(* A variant inside a procedure body: the measure resolves through the
   procedure's locals, and its termination obligation is discharged there. *)
let%test_unit "Main.verify true variant in procedure" =
  check_verify "verify_true_variant_proc.cav" Valid

(* Total-correctness recursion: [sum_to] calls itself with a decreasing variant
   [n], verified inductively (its own contract is the hypothesis for the
   recursive call). Lifts the old bottom-up ordering restriction. *)
let%test_unit "Main.verify true recursion (total)" =
  check_verify "verify_true_recursion.cav" Valid

(* Partial correctness allows recursion with no variant: a non-terminating
   recursive procedure makes [ensures { false }] -- and hence the whole triple --
   vacuously provable, just like a non-terminating loop. *)
let%test_unit "Main.verify true recursion (partial, vacuous)" =
  check_verify "verify_true_recursion_partial.cav" Valid

(* Two recursive calls in one body ([fib(n-1)] and [fib(n-2)]): each call site
   gets its own decrease obligation, both discharged in the [n >= 2] branch. *)
let%test_unit "Main.verify true recursion (multiple calls)" =
  check_verify "verify_true_recursion_multi.cav" Valid

(* Recursion over an array: [fill] writes [a\[i\]] and recurses with a
   [len(a) - i] measure, exercising the array bounds obligation and the array
   havoc across a recursive call together. *)
let%test_unit "Main.verify true recursion over an array" =
  check_verify "verify_true_recursion_array.cav" Valid

(* A variant whose measure is array-based ([len(a) - i]): variants compose with
   arrays and the [len]/element machinery. *)
let%test_unit "Main.verify true variant with array measure" =
  check_verify "verify_true_variant_array.cav" Valid

(* ===== verify: expected Invalid ===== *)

let%test_unit "Main.verify false" = check_verify "verify_false.cav" Invalid

(* A rejected program reports which procedure failed to verify (main included,
   under the name "main"). *)
let check_failing_proc path expect =
  [%test_result: string option]
    (Main.verify_report ~debug ?timeout:(Some 5.) (Main.get_ast path))
      .failing_proc ~expect

let%test_unit "Main.verify_report names the failing procedure" =
  check_failing_proc "verify_false_proc_ensures.cav" (Some "f")

let%test_unit "Main.verify_report names an undeclared-writes procedure" =
  check_failing_proc "verify_false_writes_undeclared.cav" (Some "f")

let%test_unit "Main.verify_report names main when main fails" =
  check_failing_proc "verify_false.cav" (Some "main")

let%test_unit "Main.verify_report reports no procedure on success" =
  check_failing_proc "verify_true_if.cav" None

(* A rejected program is classified by *why* the obligation failed. Each fixture
   below is engineered to fail exactly one kind of obligation, so the reported
   reason pins down the fault. *)
let check_reason ?(machine_int = false) path expect =
  [%test_result: Hoare.reason option]
    (Main.verify_report ~debug ~machine_int ?timeout:(Some 5.)
       (Main.get_ast path))
      .reason
    ~expect:(Some expect)

let%test_unit "reason: postcondition" =
  check_reason "verify_false.cav" Postcondition

let%test_unit "reason: procedure violates its own ensures" =
  check_reason "verify_false_proc_ensures.cav" Postcondition

let%test_unit "reason: callee precondition" =
  check_reason "verify_false_proc_requires.cav" Call_precondition

let%test_unit "reason: divisor may be zero" =
  check_reason "verify_false_div_by_zero.cav" Nonzero_divisor

let%test_unit "reason: array index out of bounds (write)" =
  check_reason "verify_false_array_write_oob.cav" Array_bounds

let%test_unit "reason: array index out of bounds (read)" =
  check_reason "verify_false_array_read_oob.cav" Array_bounds

let%test_unit "reason: array length negative" =
  check_reason "verify_false_array_negative_length.cav" Array_length_nonneg

let%test_unit "reason: loop invariant on entry" =
  check_reason "verify_false_inv_entry.cav" Loop_invariant_init

let%test_unit "reason: loop variant does not decrease" =
  check_reason "verify_false_variant.cav" Loop_variant

let%test_unit "reason: recursive variant does not decrease" =
  check_reason "verify_false_recursion.cav" Recursive_variant

let%test_unit "reason: undeclared write" =
  check_reason "verify_false_writes_undeclared.cav" Undeclared_write

(* Overflow-freedom is only an obligation under machine-integer verification. *)
let%test_unit "reason: arithmetic may overflow (machine int)" =
  check_reason ~machine_int:true "verify_boundary_overflow.cav" No_overflow

(* A construct-level failure points at the source line of the offending command;
   a whole-procedure obligation (a plain postcondition) carries no location. *)
let loc_line path =
  match
    (Main.verify_report ~debug ?timeout:(Some 5.) (Main.get_ast path)).loc
  with
  | Some (l : Main.Ast.Loc.t) -> Some l.line
  | None -> None

let check_loc_line path expect =
  [%test_result: int option] (loc_line path) ~expect

let%test_unit "location: divide-by-zero points at the division's line" =
  check_loc_line "verify_false_div_by_zero.cav" (Some 2)

let%test_unit "location: out-of-bounds write points at the write's line" =
  check_loc_line "verify_false_array_write_oob.cav" (Some 3)

let%test_unit "location: recursive variant points at the call's line" =
  check_loc_line "verify_false_recursion.cav" (Some 7)

let%test_unit "location: a plain postcondition has no location" =
  check_loc_line "verify_false.cav" None

(* The counterexample is a concrete failing state, projected back onto source
   variables. Values are forced by the fixture, so they are stable: the divisor
   must be 0 to fail, and the precondition pins x,y. *)
let ce_value path name =
  List.Assoc.find ~equal:String.equal
    (Main.verify_report ~debug ?timeout:(Some 5.) (Main.get_ast path))
      .counterexample name

let%test_unit "counterexample: the divisor is zero" =
  [%test_result: string option]
    (ce_value "verify_false_div_by_zero.cav" "y")
    ~expect:(Some "0")

let%test_unit "counterexample: the precondition values that break the goal" =
  [%test_result: string option]
    (ce_value "verify_false.cav" "y")
    ~expect:(Some "60")

(* Internal WLP symbols (havoc copies, the frozen-variant var) are not surfaced:
   the loop-variant obligation reports only the source variable. *)
let%test_unit "counterexample: hides internal variables" =
  [%test_result: string option]
    (ce_value "verify_false_variant.cav" "variant")
    ~expect:None

(* An array witness is expanded into a concrete list of its (model) length, not
   left as Why3 map syntax. The fixture pins [len(a) = 3] on entry and writes out
   of bounds, so the array is a three-element list; its entries are unconstrained
   and Z3 fills them with the map default 0. *)
let%test_unit "counterexample: an array is a concrete list" =
  [%test_result: string option]
    (ce_value "verify_false_array_oob_len.cav" "a")
    ~expect:(Some "[0, 0, 0]")

(* An out-of-bounds access on an array reassigned inside the body leaves the
   entry-state array empty (length 0): a clean [[]], not [[|_ => 0|]]. *)
let%test_unit "counterexample: an empty array renders as []" =
  [%test_result: string option]
    (ce_value "verify_false_array_read_oob.cav" "a")
    ~expect:(Some "[]")

(* An [Invalid] verdict records its confidence. Z3 answers [unknown "sat"] on
   these quantified WLP goals, so a genuine failure is reported as a candidate
   rather than a confirmed disproof; a successful verification carries no
   status. *)
let status_of path =
  (Main.verify_report ~debug ?timeout:(Some 5.) (Main.get_ast path)).status

let%test_unit "status: a failed obligation is a candidate" =
  [%test_result: Smt.Prover.status option]
    (status_of "verify_false_div_by_zero.cav")
    ~expect:(Some Smt.Prover.Candidate)

(* A static writes-clause rejection never reaches the prover, so it is a
   confirmed disproof rather than a candidate. *)
let%test_unit "status: a static rejection is disproved" =
  [%test_result: Smt.Prover.status option]
    (status_of "verify_false_writes_undeclared.cav")
    ~expect:(Some Smt.Prover.Disproved)

let%test_unit "status: a valid program has none" =
  [%test_result: Smt.Prover.status option]
    (status_of "verify_true_nonlinear.cav")
    ~expect:None

(* A provided variant that does not decrease (here the measure [i] increases)
   is rejected even though the loop does terminate: the given measure fails to
   prove it. *)
let%test_unit "Main.verify false variant does not decrease" =
  check_verify "verify_false_variant.cav" Invalid

(* The same non-terminating loop as the partial-correctness fixture, but with a
   variant: total correctness now demands termination, which cannot be proved,
   so the (vacuous) triple is rejected. *)
let%test_unit "Main.verify false non-terminating loop (total)" =
  check_verify "verify_false_nonterminating_total.cav" Invalid

(* Exercises the *bounded-below* half of the termination obligation (distinct
   from [verify_false_variant], which fails the strict-decrease half): the
   measure [0 - i] strictly decreases every iteration but is unbounded below, so
   the loop need not terminate and [0 <= V] cannot be proved. *)
let%test_unit "Main.verify false variant unbounded below" =
  check_verify "verify_false_variant_unbounded.cav" Invalid

(* Recursion with a variant that does not decrease across the recursive call
   ([loops(n)] recurses on the same [n]) is rejected: [n < n] is false. *)
let%test_unit "Main.verify false recursion (variant does not decrease)" =
  check_verify "verify_false_recursion.cav" Invalid

(* The same non-terminating recursion as the partial fixture, now with a variant:
   total correctness demands termination, which cannot be proved. *)
let%test_unit "Main.verify false recursion (total)" =
  check_verify "verify_false_recursion_total.cav" Invalid

(* Bounded-below half of the recursion decrease obligation: [dive] recurses on
   [n - 1] (which decreases) but with no lower bound on [n], so [0 <= n - 1]
   cannot be proved -- the recursion need not terminate. *)
let%test_unit "Main.verify false recursion (unbounded below)" =
  check_verify "verify_false_recursion_unbounded.cav" Invalid

(* Loop invariant that does not hold on entry. *)
let%test_unit "Main.verify false invariant on entry" =
  check_verify "verify_false_inv_entry.cav" Invalid

(* Procedure body violates its own `ensures` (x := x + 2 vs x = _x + 1). *)
let%test_unit "Main.verify false procedure ensures" =
  check_verify "verify_false_proc_ensures.cav" Invalid

(* Caller does not establish the callee's `requires` (needs x >= 10). *)
let%test_unit "Main.verify false procedure requires" =
  check_verify "verify_false_proc_requires.cav" Invalid

(* Well-definedness: [q := x / y] with an unconstrained divisor cannot discharge
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

(* Only a hard [Invalid] answer is a confirmed disproof; every other answer that
   still yields an [Invalid] result (Z3's [Unknown "sat"] on a quantified goal)
   is a candidate whose model may be spurious. *)
let%test_unit "Prover.status_of_answer mapping" =
  let open Smt.Prover in
  [%test_result: status]
    (status_of_answer Why3.Call_provers.Invalid)
    ~expect:Disproved;
  [%test_result: status]
    (status_of_answer (Why3.Call_provers.Unknown "sat"))
    ~expect:Candidate;
  [%test_result: status]
    (status_of_answer (Why3.Call_provers.Unknown ""))
    ~expect:Candidate

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
