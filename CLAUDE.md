# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Cavalry is a toy imperative programming language (OCaml) with Hoare-logic
verification: programs carry pre/postcondition annotations and loop
invariants, and can be proven correct via SMT (Z3, through Why3)
without executing them. See `example.cav` for the surface syntax and the
article linked from `README.md` for the theory.

## Commands

```bash
opam install --deps-only --with-test --with-doc .   # install deps (why3, odoc, ...); Z3 4.16.0 is a system binary (e.g. brew install z3)
why3 config detect                       # required once, so why3 can find z3

dune build                               # build everything
dune build @fmt                          # check formatting (ocamlformat)
dune build @fmt --auto-promote           # apply formatting fixes
dune runtest                             # run the inline-test suite (test/)
dune build @doc                          # generate HTML API docs to _build/default/_doc/_html/

dune exec -- cav run <file.cav>            # interpret a program
dune exec -- cav verify <file.cav>         # verify pre/post conditions
dune exec -- cav verify -d <file.cav>      # verify with debug output (prints WLP terms)
```

There's no per-test filter wired up; `dune runtest` runs the whole
`test/integration.ml` + `test/program.ml` suite together against the `.cav`
fixtures in `test/`. CI (`.github/workflows/`) provisions Z3 4.16.0, then runs
`why3 config detect`, `dune build @fmt`, `dune build`, `dune build @runtest`,
and `dune build @doc`, plus a step that `cav verify`s every README example, and
a `snippets` job. Note Z3 diverges on nonlinear integer div/mod goals (e.g.
deriving `q = x / y` from a `x = q*y + r` loop invariant); prefer the
multiplicative postcondition. `cav verify --timeout` (default 10s) bounds each
obligation so such a goal reports a failure instead of hanging.

### README code snippets (regeneration flow)

The README embeds syntax-highlighted **SVGs** of the example programs, rendered
from `assets/readme-snippets/snippets/<slug>.cav` into
`assets/snippet-<slug>-{light,dark}.svg`. After editing any snippet `.cav` (or the
grammar / `.vscode/settings.json`), regenerate or CI's `snippets` job and the
pre-commit hook fail:

```bash
cd assets/readme-snippets && npm install && npm run build   # re-render SVGs + README blocks
npm run check                                             # what CI runs (exit 1 if stale)
```

See `assets/readme-snippets/README.md` for the full mechanism.

## Architecture

Pipeline for both `run` and `verify`, driven from `bin/main.ml` (cmdliner CLI)
through `src/main.ml` (`get_ast`, `verify`, `exec`):

1. **Parse**: `src/ast/parse/{lexer.mll,tokens.mly,program.mly,logic.mly}`
   (ocamllex + Menhir, `logic.mly` merged into `program`'s parser via
   `--external-tokens`) produce an *untyped* AST: `Triple.ut_t` wrapping
   `Program.ut_expr`.
2. **Type/translate**: `Program.translate_cmd` converts the untyped AST into
   a GADT-typed AST (`Program.expr`/`Program.value`, indexed by `int expr`
   vs `bool expr`), raising `TypeError` on malformed input. A whole source
   file is a list of `Triple.t` — one Hoare triple `{p} c {q}` per
   `procedure`, plus a final one for `main` (untitled, no params/writes).
3. **Variable resolution**: `Var_collection.collect` walks the triples and
   assigns each program variable a Why3 `Term.vsymbol` (via `Vars`),
   splitting them into per-procedure locals vs `main`'s globals — this
   distinction matters because procedures only see globals they declare in
   their `writes { ... }` clause.
4. Two consumers of the same typed AST:
   - **`Ast.Runtime.exec`**: a plain tree-walking interpreter over
     `Program.cmd`/`proc_t`, used by `cav run`.
   - **`Hoare.verify`** (`src/hoare.ml`): computes the *weakest liberal
     precondition* (WLP, Dijkstra predicate transformer semantics) of each
     procedure body bottom-up (procedures must verify before anything that
     calls them, enforced by folding over `split_last program` and raising
     `Proc_invalid` on failure), builds the term `p -> wlp(c, q)` as a Why3
     `Term.term`, and discharges it via `Smt.Prover.prove`.
     Procedure calls are handled by substituting actuals for formals and
     havoc'ing written variables (fresh `y` vars + `t_forall_close`), not by
     inlining — see `Wlp.proc`/`sub_old_vars`/`sub_written_vars`.
5. **SMT backend**: `src/smt/prover.ml` wraps Why3/Z3 — loads the Z3 4.16.0
   driver once at module init (fails fast with `exit 1` if Z3 isn't detected
   by `why3 config detect`), and the `Arith` module (`src/ast/arith.ml`,
   `plus`/`sub`/`mul`/... plus `base_task`) builds the arithmetic theory terms
   that `Hoare` and `Logic.translate_term` compile expressions into.

Key modules, by directory:
- `src/ast/` (`arith`, `logic`, `program`, `runtime`, `triple`, `vars`,
  `var_collection`) — AST types, the interpreter, and variable bookkeeping.
- `src/ast/parse/` — lexer/grammar only; produces `ut_expr`/`ut_t`.
- `src/hoare.ml` — the WLP calculus and top-level `verify`.
- `src/smt/` — Why3/Z3 plumbing, independent of the AST.
- `bin/main.ml` — CLI wiring only (`run` / `verify` subcommands).

`Vars.find_fallback` and the global/local split recur throughout `hoare.ml`
and `var_collection.ml`; when adding a language feature that introduces new
bindings, check both places for how a name resolves to a `vsymbol`.
