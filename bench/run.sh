#!/usr/bin/env bash
# Benchmark the Cavalry interpreter against the two compiled backends.
#
# For each program in bench/programs/ we time three variants of the *same*
# computation, using hyperfine (warmups + repeated runs + mean/stddev/outlier
# detection), and export the raw per-run samples to JSON for statistical
# analysis by analyze.py:
#
#   interp  : cav run <prog>.cav          -- tree-walking interpreter (native 63-bit int)
#   zarith  : ./<prog>_z                  -- compiled, arbitrary-precision Zarith (the `cav compile` default)
#   native  : ./<prog>_n                  -- compiled, native 63-bit int (`cav compile --native-int`)
#
# A `trivial` program (empty body) is also timed to measure each variant's fixed
# startup cost, so analyze.py can separate startup from compute.
set -euo pipefail

cd "$(dirname "$0")/.."
ROOT=$(pwd)
BIN="$ROOT/_build/default/bin/main.exe"
PROGDIR="$ROOT/bench/programs"
OUTDIR="$ROOT/bench/results"
mkdir -p "$OUTDIR"

WARMUP=${WARMUP:-3}
MIN_RUNS=${MIN_RUNS:-20}

# Prerequisite check up front, so a missing tool fails with a clear message
# rather than a confusing mid-run error.
missing=""
for tool in hyperfine python3 ocamlfind ocamlopt dune; do
  command -v "$tool" >/dev/null 2>&1 || missing="$missing $tool"
done
if [ -n "$missing" ]; then
  echo "error: missing required tool(s):$missing" >&2
  echo "  hyperfine: https://github.com/sharkdp/hyperfine  (brew/apt install hyperfine)" >&2
  echo "  ocamlfind/ocamlopt/dune + the 'zarith' opam package build the Cavalry toolchain." >&2
  echo "  Note: Z3 is NOT needed -- the benchmark compiles with --no-verify." >&2
  exit 1
fi

echo "Building..."
dune build

# (re)generate and compile every program in both backends.
python3 "$ROOT/bench/gen.py"
printf '{ true }\n0\n{ true }\n' > "$PROGDIR/trivial.cav"

PROGRAMS=(trivial sum_loop collatz primes_trial sieve ackermann)
for prog in "${PROGRAMS[@]}"; do
  "$BIN" compile --no-verify              -o "$PROGDIR/${prog}_z" "$PROGDIR/$prog.cav" >/dev/null
  "$BIN" compile --no-verify --native-int -o "$PROGDIR/${prog}_n" "$PROGDIR/$prog.cav" >/dev/null
done

# Record machine metadata alongside the results (portable across macOS/Linux).
if [ "$(uname -s)" = "Darwin" ]; then
  CPU=$(sysctl -n machdep.cpu.brand_string 2>/dev/null || echo unknown)
  CORES=$(sysctl -n hw.ncpu 2>/dev/null || echo unknown)
  OS="$(sw_vers -productName 2>/dev/null || echo macOS) $(sw_vers -productVersion 2>/dev/null || true)"
else
  CPU=$(awk -F: '/model name/{print $2; exit}' /proc/cpuinfo 2>/dev/null | sed 's/^ *//' || echo unknown)
  CORES=$(nproc 2>/dev/null || echo unknown)
  OS=$( (. /etc/os-release 2>/dev/null && echo "$PRETTY_NAME") || uname -sr)
fi
{
  echo "date: $(date -u +%Y-%m-%dT%H:%M:%SZ)"
  echo "cpu: ${CPU:-unknown}"
  echo "cores: ${CORES:-unknown}"
  echo "os: ${OS:-unknown}"
  echo "hyperfine: $(hyperfine --version)"
  echo "warmup: $WARMUP"
  echo "min_runs: $MIN_RUNS"
} > "$OUTDIR/meta.txt"
cat "$OUTDIR/meta.txt"

for prog in "${PROGRAMS[@]}"; do
  echo
  echo "=== $prog ==="
  hyperfine \
    --warmup "$WARMUP" --min-runs "$MIN_RUNS" \
    --command-name "interp" "$BIN run $PROGDIR/$prog.cav" \
    --command-name "zarith" "$PROGDIR/${prog}_z" \
    --command-name "native" "$PROGDIR/${prog}_n" \
    --export-json "$OUTDIR/$prog.json"
done

echo
echo "Analysing..."
python3 "$ROOT/bench/analyze.py"
echo
echo "Done. Raw JSON + summary.{md,json} + report_local.html in $OUTDIR/."
