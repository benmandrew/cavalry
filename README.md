# Cavalry

![](docs/cavalry-banner.png)

Cavalry is a mini programming language of my own design where written programs can be "verified", i.e. the implementation of the program can be rigorously checked against its logical specification for correctness, without ever running the code.

Programs are built from procedures, loops, and bounded arrays, and their specifications quantify over program state with `forall` and `exists`. Verification covers not just partial correctness but termination too: loops and recursive procedures carry `variant` measures that prove they finish, giving total correctness. A verified program can then be compiled to a native executable.

Details about Cavalry and Hoare logic are in an article on my website [here](https://www.benmandrew.com/articles/cavalry).

## Requirements

The recommended way to get a working toolchain is [Nix](https://nixos.org/) with
[flakes enabled](https://nixos.wiki/wiki/Flakes): the bundled flake provisions
opam and the native libraries the project builds against. Setting the toolchain
up by hand instead needs:

- OCaml >= 4.14 and opam
- [Why3](https://www.why3.org/) with the [Z3](https://github.com/Z3Prover/z3) 4.16.0 SMT solver, used to discharge verification proof obligations (the Nix dev shell supplies Z3; without Nix, install it yourself, e.g. `brew install z3`)

## Getting started

```bash
git clone git@github.com:benmandrew/cavalry.git
cd cavalry
```

### With Nix (recommended)

```bash
nix develop
```

Entering the dev shell for the first time bootstraps a local opam switch,
installs the project dependencies, and runs `why3 config detect` so Why3 can
find Z3 (which the shell provides). A stamp file guards this so it only happens
once per clone. If
you use [direnv](https://direnv.net/), `direnv allow` enters the shell (and runs
the bootstrap) automatically.

### Without Nix

Provision the opam switch and prover yourself (install Z3 4.16.0 first, e.g.
`brew install z3`):

```bash
opam install --deps-only --with-test .
why3 config detect  # let Why3 find the Z3 prover
```

## Running

Once the environment is ready, build the project and exercise a program:

```bash
dune build
# Verify [example.cav]
dune exec -- cav verify example.cav
# Run [example.cav]
dune exec -- cav run example.cav
# Compile [example.cav] to a native executable (gated on verification)
dune exec -- cav compile example.cav
```

By default the prover reasons over unbounded (mathematical) integers. Pass
`--machine-int` to `verify` — or `--native-int` to `compile` — to reason over
OCaml's 63-bit machine integers instead, in which case every arithmetic
operation must additionally be proven not to overflow.

## Testing

```bash
dune runtest
```

## Example programs

### Computing triangle numbers

A loop's `invariant` holds on entry and after every iteration, while its
optional `variant` — a non-negative measure that strictly decreases each
iteration — proves termination, giving total correctness.

```
{ x = 0 && i = 0 && n >= 0 }
while i < n do
  invariant { 2 * x = i * (i - 1) && 0 <= i && i <= n }
  variant { n - i }
  x <- x + i;
  i <- i + 1
end;
x
{ 2 * x = n * (n - 1) }
```

### Euclidean division procedure

Division `/` and modulo `%` are part of the logic, so the postcondition can
specify the loop's result directly in terms of them.

```
procedure euclidean_div () =
  requires { x >= 0 && y > 0 }
  ensures { q = x / y && r = x % y }
  writes { q, r }
  q <- 0;
  r <- x;
  while r >= y do
    invariant { x = q * y + r && 0 <= r }
    variant { r }
    r <- r - y;
    q <- q + 1
  end
end

{ true }
x <- 42;
y <- 17;
q <- 0;
r <- 0;
euclidean_div();
print(q);
print(r)
{ q = 2 && r = 8 }
```

### Filling an array

Bounded arrays are created with `array(n)` (zero-initialised), indexed with
`a[i]`, and sized with `len(a)`. Specifications quantify over their contents
with `forall` and `exists`.

```
{ n >= 0 }
a <- array(n);
i <- 0;
while i < len(a) do
  invariant { 0 <= i && i <= len(a) && forall j . 0 <= j && j < i -> a[j] = 5 }
  variant { len(a) - i }
  a[i] <- 5;
  i <- i + 1
end
{ forall j . 0 <= j && j < len(a) -> a[j] = 5 }
```

### Recursive procedure

Procedures may call themselves. A `variant` on the procedure — decreasing
across each recursive call — proves the recursion terminates.

```
procedure sum_to (n) =
  requires { n >= 0 }
  ensures { s >= 0 }
  variant { n }
  writes { s }
  if n = 0 then
    s <- 0
  else
    sum_to(n - 1);
    s <- s + n
  end
end

{ true }
sum_to(5)
{ s >= 0 }
```
