# Cavalry

![](https://benmandrew.s3.eu-west-2.amazonaws.com/headers/cavalry-banner.png)

Cavalry is a mini programming language of my own design where written programs can be "verified", i.e.  the implementation of the program can be rigorously checked against its logical specification for correctness, without ever running the code!

Details about Cavalry and Hoare logic are in an article on my website [here](https://www.benmandrew.com/articles/cavalry).

## Running

```bash
git clone git@github.com:benmandrew/cavalry.git
cd cavalry
opam install --deps-only .
dune build
# Verify [example.cvl]
dune exec -- cavalry verify example.cvl
# Run [example.cvl]
dune exec -- cavalry run example.cvl
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
