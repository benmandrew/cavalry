# Interpreter vs compiled benchmark

This directory measures how much faster a compiled Cavalry program runs than the
same program under the *tree-walking interpreter* (`cav run`). Each workload is
timed three ways: the interpreter, the default *Zarith* compile (arbitrary-precision
integers, matching the verified model), and the `--native-int` compile (wrapping
63-bit integers). Results are statistically summarised and rendered to an HTML
report.

## Prerequisites

- The Cavalry toolchain built in the repo root: `dune build` produces
  `_build/default/bin/main.exe`. Building needs OCaml, `dune`, `ocamlfind`,
  `ocamlopt`, and the `zarith` opam package.
- [`hyperfine`](https://github.com/sharkdp/hyperfine) for timing
  (`brew install hyperfine` or `apt install hyperfine`).
- `python3` (standard library only) for the analysis.

Z3 is *not* required. The benchmark compiles with `--no-verify`, and since the
prover is now initialised lazily, `cav run` never touches it either.

## Running

```bash
bash bench/run.sh            # build, generate, compile, time, analyse
```

One command reproduces everything: it regenerates the programs, compiles both
backends, times all three variants with hyperfine, and writes the analysis and
report. Knobs are environment variables:

```bash
WARMUP=5 MIN_RUNS=40 bash bench/run.sh   # more runs for tighter intervals
```

`run.sh` checks its tools up front and fails with a clear message if one is
missing.

## Hosting alongside the API docs

The server that builds and hosts the odoc API docs can host this report too and
link it from them. odoc renders `doc/index.mld` to
`_build/default/_doc/_html/cavalry/index.html`, which already carries a
*Performance* link to `benchmark.html` in the same directory. After building the
docs, run the benchmark and drop the standalone report next to that page:

```bash
dune build @doc                                    # render the odoc HTML tree
bash bench/run.sh                                  # regenerate results/report_local.html
cp bench/results/report_local.html \
   _build/default/_doc/_html/cavalry/benchmark.html
```

Then host `_build/default/_doc/_html/` as usual. The report must land in the
`cavalry/` directory: the *Performance* link is relative to it, and the page
pulls its fonts from `../odoc.support/fonts/` so it matches the docs' Gruvbox
theme. Opened from anywhere else those fonts 404 and fall back to the system
serif/sans/mono, with the palette still intact. The server needs `hyperfine` on
`PATH` on top of the OCaml toolchain.

One caveat, stated plainly: a pulling build server is usually a shared virtual
machine, a noisy place to benchmark. The *ratios* between variants stay
meaningful, but the absolute millisecond figures and the tight confidence
intervals wobble more than on a quiet machine. If that matters, run the
benchmark on dedicated hardware and copy the resulting `report_local.html` into
the deploy instead.

## What each file is

| File | Role |
|------|------|
| `gen.py` | Generates the five `.cav` workloads into `programs/`. Sizes are tuned near the top. |
| `run.sh` | Builds, compiles both backends, times with hyperfine, then calls `analyze.py`. |
| `analyze.py` | Turns the raw hyperfine JSON into statistics and the HTML report. |
| `report_template.html` | The report page with `@@TOKEN@@` placeholders that `analyze.py` fills. |
| `programs/` | Generated source and compiled binaries (`_z` = Zarith, `_n` = native). |
| `results/` | Outputs (see below). |

Outputs in `results/`: `meta.txt` (machine and run parameters), one `<prog>.json`
per workload with every per-run sample, `summary.md` and `summary.json` (the
canonical machine-readable results), and `report.html` / `report_local.html` (the
same page body-only for publishing and standalone for opening locally).

## The workloads

Each stresses a different part of execution, which is why the speedup spans
roughly 10x on the loop-bound programs to ~50x on deep recursion:

| Program | Shape | What it exercises |
|---------|-------|-------------------|
| `sum_loop` | Tight arithmetic loop | Per-operation dispatch and scalar stores |
| `collatz` | Branchy loop | Data-dependent control flow, `/` and `%` |
| `primes_trial` | Nested loops | Trial division, no arrays |
| `sieve` | Array-heavy | Array indexing and bounds, in-place element writes |
| `ackermann` | Recursion | Procedure-call overhead |

## Statistical method

`analyze.py` reports, per workload, the mean plus or minus standard deviation over
the runs, a 95% *confidence interval* on each speedup by nonparametric bootstrap,
and a Welch t-test establishing that the interpreter is genuinely slower rather
than noise. The `trivial` program (an empty body) measures each variant's fixed
startup so the interpreter is not charged for process and parse overhead. No
third-party statistics packages are used; the t-test p-value uses a normal
approximation, valid because the run counts are in the dozens and the effect
sizes are large.

## Reproducibility notes

Absolute millisecond figures depend on the machine, so nothing under `results/`
is committed — it is all regenerated locally by `run.sh` (see `.gitignore`). The
*ratios* are the portable result, and the report regenerates from whatever run
produced the JSON. The workload sizes in
`gen.py` were chosen so the interpreter runs for roughly 0.5 to 3 seconds on an
Apple M1; on much slower or faster hardware, edit `SIZES` to keep the timings in
that band. The one figure that will not reproduce is the interpreter's old ~100 ms
startup — that cost was removed by deferring the prover initialisation, and the
report records it only as the before half of a before-and-after.
