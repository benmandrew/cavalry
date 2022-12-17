open Why3

val env : Env.env

type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

val prove_implies :
  int option ->
  Task.task ->
  Term.vsymbol list ->
  Term.term ->
  Term.term ->
  result
