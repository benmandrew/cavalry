open Why3

val env : Env.env

type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

val prove :
  float option -> Task.task -> Term.vsymbol list -> Term.term -> result
