#!/usr/bin/env python3
"""Generate the Cavalry benchmark programs.

Each program is a self-contained `.cav` file exercising a different, realistic
workload shape. Loop `invariant { true }` clauses are placeholders: the benchmark
compiles with `--no-verify`, so the programs need not be provably correct, only
representative of the *execution* the interpreter and the compiled binary each
perform. Sizes are tuned so the interpreter runs for roughly 0.5-3 s (long
enough that its fixed startup is a small, separately-measured fraction).

Every program prints a single checksum, letting the harness confirm the
interpreter, the Zarith binary, and the native-int binary all compute the same
thing before their timings are compared.
"""

import pathlib

HERE = pathlib.Path(__file__).parent
OUT = HERE / "programs"

# Per-program workload size. Tuned on the dev machine for ~0.5-3 s interpreter
# time.
SIZES = {
    "sum_loop": 5_000_000,
    "collatz": 120_000,
    "primes_trial": 200_000,
    "sieve": 2_000_000,
    "ackermann_n": 8,  # ack(3, N)
}

PROGRAMS = {}

# 1. Tight arithmetic loop: a modular checksum. No arrays, no calls -- pure
#    scalar arithmetic and loop dispatch. The `%` keeps the accumulator
#    bounded so native 63-bit and unbounded Zarith agree (no overflow).
PROGRAMS["sum_loop"] = lambda n: f"""// Arithmetic-bound: a modular running checksum over a tight loop.
{{ true }}
s := 0;
i := 0;
while i < {n} do
  invariant {{ true }}
  s := (s + i * i - i) % 1000000007;
  i := i + 1
end;
print(s);
s
{{ true }}
"""

# 2. Branchy loop with data-dependent iteration counts and div/mod: sum of the
#    Collatz stopping times for k = 1 .. N-1.
PROGRAMS["collatz"] = lambda n: f"""// Branch-bound: total Collatz stopping time for 1..N, with data-dependent
// control flow and integer div/mod.
{{ true }}
total := 0;
k := 1;
while k < {n} do
  invariant {{ true }}
  m := k;
  steps := 0;
  while m > 1 do
    invariant {{ true }}
    if m % 2 = 0 then
      m := m / 2
    else
      m := 3 * m + 1
    end;
    steps := steps + 1
  end;
  total := total + steps;
  k := k + 1
end;
print(total);
total
{{ true }}
"""

# 3. Nested loops, trial division: count the primes below N. Arithmetic- and
#    branch-bound, no arrays, scales cleanly.
PROGRAMS["primes_trial"] = lambda n: f"""// Nested-loop-bound: count primes below N by trial division.
{{ true }}
count := 0;
m := 2;
while m < {n} do
  invariant {{ true }}
  d := 2;
  isprime := 1;
  while d * d <= m do
    invariant {{ true }}
    if m % d = 0 then
      isprime := 0;
      d := m + 1
    else
      d := d + 1
    end
  end;
  count := count + isprime;
  m := m + 1
end;
print(count);
count
{{ true }}
"""

# 4. Array-heavy: Sieve of Eratosthenes. Every `sieve[j] := 0` is an element
#    write -- an in-place O(1) store in both the compiled binaries and the
#    interpreter. This isolates array indexing and bounds handling from the
#    scalar arithmetic the other workloads stress.
PROGRAMS["sieve"] = lambda n: f"""// Array-bound: Sieve of Eratosthenes below N, counting primes.
{{ true }}
sieve := array({n});
i := 2;
while i < {n} do
  invariant {{ true }}
  sieve[i] := 1;
  i := i + 1
end;
p := 2;
while p * p < {n} do
  invariant {{ true }}
  if sieve[p] = 1 then
    j := p * p;
    while j < {n} do
      invariant {{ true }}
      sieve[j] := 0;
      j := j + p
    end
  else
    p := p
  end;
  p := p + 1
end;
count := 0;
i := 2;
while i < {n} do
  invariant {{ true }}
  count := count + sieve[i];
  i := i + 1
end;
print(count);
count
{{ true }}
"""

# 5. Recursion / procedure-call-bound: the Ackermann function ack(3, N). Tiny
#    arithmetic per call but a huge number of deeply nested calls -- it measures
#    call overhead (interpreter builds a fresh local env + hashtable lookup per
#    call; the compiled binary makes a real OCaml call).
PROGRAMS["ackermann"] = lambda n: f"""// Call-bound: Ackermann ack(3, {n}), returning through the global `r`.
procedure ack (m, n) =
  requires {{ true }}
  ensures {{ true }}
  writes {{ r }}
  if m = 0 then
    r := n + 1
  else
    if n = 0 then
      ack(m - 1, 1)
    else
      ack(m, n - 1);
      ack(m - 1, r)
    end
  end
end

{{ true }}
ack(3, {n});
print(r);
r
{{ true }}
"""


def main():
    OUT.mkdir(parents=True, exist_ok=True)
    for name, tmpl in PROGRAMS.items():
        size = SIZES["ackermann_n"] if name == "ackermann" else SIZES[name]
        (OUT / f"{name}.cav").write_text(tmpl(size))
        print(f"wrote {name}.cav (size {size})")


if __name__ == "__main__":
    main()
