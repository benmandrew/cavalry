open Why3

val plus : Term.term -> Term.term -> Term.term
val sub : Term.term -> Term.term -> Term.term
val mul : Term.term -> Term.term -> Term.term
val eq : Term.term -> Term.term -> Term.term
val neq : Term.term -> Term.term -> Term.term
val lt : Term.term -> Term.term -> Term.term
val leq : Term.term -> Term.term -> Term.term
val gt : Term.term -> Term.term -> Term.term
val geq : Term.term -> Term.term -> Term.term

val min_int : Term.term
(** The smallest representable machine integer, [-2^62] (OCaml 63-bit [int]). *)

val max_int : Term.term
(** The largest representable machine integer, [2^62 - 1]. *)

val in_bounds : Term.term -> Term.term
(** [in_bounds t] is the proposition [min_int <= t <= max_int]: [t] fits in a
    machine integer. Used to build per-operation overflow-freedom obligations.
*)

val base_task : Task.task
