# PLAN: QuickCheck-based soundness harness

## Goal

Automatically find **soundness** bugs in `Hoare.verify` — cases where the
verifier certifies a Hoare triple `{P} C {Q}` as `Valid` even though the
program can, starting from a state satisfying `P`, terminate in a state
violating `Q`.

The oracle is the interpreter (`Ast.Runtime.exec`): it is an independent
implementation of the language semantics and is the ground truth for concrete
runs. This makes soundness testing a **differential test** between
`Hoare.verify` (symbolic) and `Runtime.exec` (concrete).

Both bugs fixed on this branch (unquantified loop rule; unenforced `writes`
clause) have this exact signature: `verify = Valid` while `exec` reaches a
state contradicting `Q`. The harness is designed to catch that whole class.

## Key semantic facts to respect

- **Partial correctness.** WLP is the weakest *liberal* precondition: if the
  program terminates, `Q` holds. A soundness counterexample must therefore be a
  **terminating** run. Bound the interpreter with fuel and *discard* (do not
  fail) runs that exceed it — non-termination is not a counterexample.
- **Preconditions are assertions, not initializers.** The interpreter needs
  variables bound before use, and reading an unbound variable raises
  (`Runtime.UnboundError`). The generator must produce a concrete initial
  environment separately from `P`.
- **Incompleteness is expected and is not a soundness bug.** A *true* triple
  can verify as `Invalid` (e.g. nonlinear arithmetic Alt-Ergo cannot close).
  Execution cannot witness `∀`-facts, so the harness targets soundness
  (`Valid`-but-false), not completeness.

## Primary strategy: constructed false triples

Do not generate random `P`/`Q` and hope to hit the `Valid` branch (you would
almost always get `Invalid`). Instead construct triples that are *definitely
false* and assert the verifier never accepts them:

1. Generate a random well-typed `Program.cmd` `C` and a concrete initial
   environment `s0` (a binding for every variable `C` reads).
2. Execute `C` on `s0` with fuel. If it diverges or raises, **skip**.
   Otherwise obtain the final state `s1`.
3. Build `P` = a formula pinning `s0` exactly (`x = 3 && y = 5 && ...`), so the
   triple ranges over the single state `s0`.
4. Build `Q` = a formula **false on `s1`** (negate an atom that holds at `s1`,
   or assert `v = s1(v) + 1` for some variable `v`).
5. Property: `verify {P} C {Q}` **must** be `Invalid`. A `Valid` result is a
   minimized soundness bug.

### Dual property (regression / incompleteness signal)

With the same `C`, `s0`, `s1`, build `Q'` = a formula **true on `s1`**. Then
`verify {P} C {Q'}` **should** be `Valid`. Failures here are not unsoundness
but catch regressions and gross incompleteness on fully-concrete triples (all
linear, so the solver should handle them). Keep this property but treat its
failures as lower severity / allow an ignore-list.

## Generators

Use `qcheck` (structured, well-typed generation + shrinking) rather than
AFL/`crowbar` byte fuzzing — we need programs that type-check and shrinking that
minimizes counterexamples to a few lines.

- **`cmd` generator**, recursive with a depth/size budget:
  - leaves: `Assgn (x, int_expr)`, trailing `IntExpr`
  - nodes: `Seq`, `If (bool_expr, c, c')`, small bounded `While (inv, b, c)`,
    `Proc` calls into a small generated procedure pool
  - bias toward `Assgn`/`Seq`; keep `While` shallow and rare so runs terminate
    within fuel.
- **Variable pool**: a small fixed set of names (e.g. `x y z i j`) so aliasing
  and collisions actually occur.
- **`int_expr` / `bool_expr` generators** over the pool, ints from a small
  range (e.g. `-8..8`) so nonlinear terms do not dominate and the concrete runs
  stay small. Respect the GADT int/bool split (guards are `bool expr`,
  assignment RHS is `int expr`).
- **Invariant generator** for `While`: generate `Logic.expr` over the pool. It
  does not need to be a true invariant — the constructed-false-triple property
  holds regardless.
- **Procedures**: generate a handful with random `writes` clauses (including
  deliberately incomplete ones) to exercise framing.

Seed the qcheck corpus with the existing `.cav` fixtures where practical.

## Interpreter changes required

- Add a **fuel-bounded** exec entry point (max loop iterations / steps) that
  returns `Terminated of state | OutOfFuel | Raised`. Reuse `Runtime.exec_cmd`;
  thread a decreasing counter through the `While` case.
- Expose the **final environment** (all variable bindings), not just the return
  value, so `s1` can be read for building `Q`. Today `exec` returns only the
  top-level `int`.

## Shrinking

Rely on qcheck's shrinkers for `cmd`, the environment, and the chosen `Q`.
Target: a counterexample shrinks to a minimal program + state + postcondition,
suitable to drop straight into `test/` as a `verify_false_*.cav` regression
fixture.

## Wiring

- New test executable/library, e.g. `test/fuzz/` with `qcheck-core` (+
  `qcheck-alcotest` or the inline-test integration).
- Add `qcheck` to `cavalry.opam` dev/test deps and the test `dune`.
- Run a **small** iteration count under `dune runtest` (fast CI), with an env
  knob (e.g. `CAVALRY_FUZZ_COUNT`) to crank it up for longer local/nightly
  sessions.
- On failure, print the generating program (`show_ut_expr` / a `.cav`
  pretty-print), `s0`, `s1`, `Q`, and the `verify` result.

## Later / complementary (out of scope for v1)

- **Counterexample-guided**: when `verify = Invalid`, extract a solver model
  (add **Z3 via Why3**) and replay it through the interpreter to confirm; also
  gives a second SMT backend for differential VC checking.
- **Runtime contract checking** (`cav check`): execute while dynamically
  asserting pre / invariants / post. Independent re-implementation of the
  oracle; a bug would have to exist in both it and the WLP code to hide.
- **Cheap targeted invariant**: have the interpreter record globals it actually
  writes and assert ⊆ declared `writes` — catches the framing bug class with no
  SMT at all.

## Milestones

1. [DONE] Fuel-bounded interpreter returning the final environment.
   (`Runtime.exec_env` in `src/ast/runtime.ml`; overflow-checked arithmetic so
   native 63-bit wraparound cannot masquerade as a counterexample; unit tests
   in `test/program.ml`.)
2. [DONE] `cmd` / expr / logic generators (no procedures), constructed-false-
   triple property, wired into `dune runtest` at low count.
   (`test/fuzz/fuzz.ml`; QCheck2 for integrated shrinking; `CAVALRY_FUZZ_COUNT`
   env knob, default 20.)
3. [DONE] Add the dual (true-`Q`) property. (Loop-free generator so the exact
   postcondition is always discharged by the solver; `Failed`/timeout treated
   as a skip, `Invalid` as a regression.)
4. [TODO] Add procedure generation with random `writes` clauses. To give this
   teeth against the framing bug class, generate procedures with `ensures
   { true }` and *incomplete* `writes`, and build `Q` asserting the pre-state
   is preserved: a correct `writes_are_declared` check rejects them (Invalid),
   but removing the check lets the caller's WLP skip the havoc and "prove" the
   false triple (Valid).
5. [TODO] Shrinker tuning + a helper to emit a failing case as a `.cav`
   fixture. (Failures currently print a readable sexp of the `cmd` plus `s0`,
   `s1`, and `Q`; a `.cav` pretty-printer would let counterexamples drop
   straight into `test/`.)

## Validation

The harness was smoke-tested by reintroducing the pre-fix unsound loop rule
(dropping the havoc in `Hoare.Wlp.cmd`'s `While` case): the primary property
fails and shrinks to `while c > 0 do t := k; c := c - 1 end` with invariant
`true` and a false postcondition -- essentially `verify_false_loop_unquantified.cav`.
With the correct rule restored it passes across seeds. The trivial invariant
`true` is deliberately over-represented in the invariant generator because it
is the shape that exposes this whole class.
