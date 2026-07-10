(** The SMT backend: a thin wrapper over Why3 driving Alt-Ergo. The prover
    (pinned to 2.4.3) and its driver are loaded once at module initialisation,
    failing fast with [exit 1] if [why3 config detect] has not found it. *)

open Why3

val env : Env.env
(** The Why3 environment (theory load path), shared so callers can build tasks
    that reference the standard library. *)

(** A goal's outcome: [Valid] (proven), [Invalid] (a counter-model or [unknown]
    -- not proven), or [Failed] (timeout, out-of-memory, or prover error),
    carrying the raw prover output. *)
type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

val result_of_answer : output:string -> Call_provers.prover_answer -> result
(** Maps a prover's raw answer onto {!result}. Exposed so the [Failed] branch
    can be exercised with a stubbed answer instead of a real timeout. *)

val prove :
  float option -> Task.task -> Term.vsymbol list -> Term.term -> result
(** [prove timeout task vars goal] universally closes [goal] over [vars], adds
    it to [task] as the goal, and runs Alt-Ergo. [timeout] is a per-goal time
    limit in seconds ([None] for unlimited). *)
