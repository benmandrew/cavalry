open Why3

val env : Env.env

type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

val result_of_answer : output:string -> Call_provers.prover_answer -> result
(** Maps a prover's raw answer onto {!result}. Exposed so the [Failed] branch
    can be exercised with a stubbed answer instead of a real timeout. *)

val prove :
  float option -> Task.task -> Term.vsymbol list -> Term.term -> result
