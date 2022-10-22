open Why3

val env : Env.env

val prove_implies :
  Task.task -> Term.vsymbol list -> Term.term -> Term.term -> bool
