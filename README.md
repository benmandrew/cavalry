# Cavalry

Cavalry is a mini-language of my own design where written programs can be "verified", i.e.  the implementation of the program can be rigorously checked against its logical specification for correctness, without ever running the code!

## 🚧 README in progress 🚧

## Grammar

## Example programs

Computing triangle numbers
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

Addition procedure
```
procedure f (a) =
  requires { true }
  ensures { x = _x + a }
  writes { x }
  x <- x + a
end

{ x = 2 }
x <- 2;
y <- 5;
f(y)
{ x = 7 }
```


## Limitations

- The assertion language is limited to propositional logic, having no support for first-order logic predicates. This makes it difficult or impossible to write many specifications, for example in the test `test_verify_true_fib_proc.cvl` we unroll the loop, as a loop invariant would have to use a Fibonacci predicate.

- A proof of correctness for a Cavalry program is just a proof according to the language's abstract semantics, it has not been proven that the abstract semantics is equivalent to the concrete implementation. Thus actually running the code on the provided interpreter could yield different results.

- Along similar lines, the prover itself is not verified and so claims of (in)correctness of a given program may not be trustworthy.