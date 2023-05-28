# Cavalry

Cavalry is a mini programming language of my own design where written programs can be "verified", i.e.  the implementation of the program can be rigorously checked against its logical specification for correctness, without ever running the code!

## 🚧 README in progress 🚧

## Hoare logic and predicate transformer semantics

We reason about the correctness of programs using [Hoare logic][1], a syntactic formal proof system which allows us to prove that a program satisfies an attached formal specification, written in an assertion logic. Our program and specification are represented by a Hoare triple $\{P\} \; C \; \{Q\}$, where $C$ is our program, $P$ is the precondition that must hold before $C$ is executed, and $Q$ is the postcondition that must hold after.

In saying that the above triple holds semantically, we mean that starting from any state satisfying $P$, executing $C$ will, if it terminates, result in a state satisfying $Q$.

While Hoare logic is a formal proof system, it does not provide an algorithm for verifying that a given triple holds. We instead turn to [predicate transformer semantics][2], a reformulation of Hoare logic that provides an algorithm to reduce the problem of proving the validity of a Hoare logic triple to that of proving a corresponding first-order logic formula. The proof of the first-order formula is then performed using the [Alt-Ergo SMT solver][3].

The first-order formula we reduce to is

$$P \implies wp(C,Q)$$

where $wp$ is a function computing the *weakest precondition* of the command $C$ and postcondition $Q$. This means that $wp(C,Q)$ is the weakest assertion such that the triple $\{wp(C,Q)\} \; C \; \{Q\}$ holds.

We then claim that if $P \implies wp(C,Q)$ holds (proved by Alt-Ergo) then the original $\{P\} C \{Q\}$ must hold.


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

Euclidean division procedure
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

## Limitations

- The assertion language is limited to propositional logic, having no support for first-order logic predicates. This makes it difficult or impossible to write many specifications, for example in the test located in `./test/verify_true_fib_proc.cvl` we unroll the loop, as a loop invariant would have to use a Fibonacci predicate.

- A proof of correctness for a Cavalry program is just a proof according to the language's abstract semantics, it has not been proven that the abstract semantics is equivalent to the concrete implementation. Thus actually running the code on the provided interpreter could yield different results.

- Along similar lines, the prover itself is not verified and so claims of (in)correctness of a given program may not be trustworthy.

## Sources
- The Hoare Logic and Model Checking course at the University of Cambridge (https://www.cl.cam.ac.uk/teaching/2223/HLog+ModC/).

- For verification of procedures, chapter 3 of the Proof of Program course at MPRI (https://www.lri.fr/~marche/MPRI-2-36-1/2012/). An updated version of the course is also linked (https://marche.gitlabpages.inria.fr/lecture-deductive-verif/).

[1]: https://en.wikipedia.org/wiki/Hoare_logic
[2]: https://en.wikipedia.org/wiki/Predicate_transformer_semantics
[3]: https://alt-ergo.ocamlpro.com
