open Why3

val plus : Term.term -> Term.term -> Term.term
val sub : Term.term -> Term.term -> Term.term
val mul : Term.term -> Term.term -> Term.term

val div : Term.term -> Term.term -> Term.term
(** Truncated (round-towards-zero) integer division, matching OCaml's native
    [/]. Underspecified by a zero divisor -- callers must discharge a
    [divisor <> 0] obligation (see [Hoare]). *)

val modulo : Term.term -> Term.term -> Term.term
(** Remainder consistent with {!div} (sign of the dividend), matching OCaml's
    native [mod]. Underspecified by a zero divisor. *)

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

val task_for : Term.term -> Task.task
(** The proof task to discharge a goal against: the integer theory, plus Why3's
    [int.ComputerDivision] iff the goal uses {!div}/{!modulo}. Those axioms are
    withheld otherwise because Alt-Ergo can loop on them even when they are
    irrelevant. *)
