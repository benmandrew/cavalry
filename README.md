# Cavalry

![](docs/cavalry-banner.png)

Cavalry is a mini programming language of my own design where written programs can be "verified", i.e. the implementation of the program can be rigorously checked against its logical specification for correctness, without ever running the code.

Details about Cavalry and Hoare logic are in an article on my website [here](https://www.benmandrew.com/articles/cavalry).

## Requirements

- OCaml >= 4.08 and opam
- [Why3](https://www.why3.org/) with the [Alt-Ergo](https://alt-ergo.ocamlpro.com/) 2.4.3 SMT solver, used to discharge verification proof obligations

## Running

```bash
git clone git@github.com:benmandrew/cavalry.git
cd cavalry
opam install --deps-only --with-test .
why3 config detect  # let Why3 find the Alt-Ergo prover
dune build
# Verify [example.cav]
dune exec -- cav verify example.cav
# Run [example.cav]
dune exec -- cav run example.cav
```

## Testing

```bash
dune runtest
```

## Example programs

### Computing triangle numbers
```
{ x = 0 && i = 0 }
while i < 10 do
  { 2 * x = (i * (i - 1)) }
  x <- x + i;
  i <- i + 1
end;
x
{ x = 45 }
```


### Euclidean division procedure
```
procedure euclidean_div () =
  requires { x >= 0 }
  ensures { x = q * y + r && 0 <= r && r < y }
  writes { q, r }
  q <- 0;
  r <- x;
  while r >= y do
    { x = q * y + r && 0 <= r }
    r <- r - y;
    q <- q + 1
  end
end

{ true }
x <- 42;
y <- 17;
q <- 0;
r <- 0;
euclidean_div()
{ q = 2 && r = 8 }
```
