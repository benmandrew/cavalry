(** The Why3 encoding of Cavalry's integer and array theory: term constructors
    for every arithmetic and comparison operator, the machine-int bounds used
    for overflow reasoning, and the proof-task builders that pull in the right
    background theories for a given goal.

    Everything downstream ([Logic.translate_term], [Hoare]) compiles source
    expressions into Why3 [Term.term]s through these combinators. *)

open Why3

val plus : Term.term -> Term.term -> Term.term
val sub : Term.term -> Term.term -> Term.term

val mul : Term.term -> Term.term -> Term.term
(** [plus]/[sub]/[mul] build the integer [+]/[-]/[*] application terms over
    Why3's unbounded [int] theory. Overflow, when it matters, is a separate
    obligation ([Hoare.safe]) rather than a property of these terms. *)

val div : Term.term -> Term.term -> Term.term
(** Truncated (round-towards-zero) integer division, matching OCaml's native
    [/]. Underspecified by a zero divisor -- callers must discharge a
    [divisor <> 0] obligation (see [Hoare]). *)

val modulo : Term.term -> Term.term -> Term.term
(** Remainder consistent with {!div} (sign of the dividend), matching OCaml's
    native [mod]. Underspecified by a zero divisor. *)

val ty_int_map : Ty.ty Lazy.t
(** The Why3 type of an array's element store: [map int int]. Lazy so that
    binding it does not force the Why3 theory layer at startup (see the note on
    {!min_int}). *)

val aget : Term.term -> Term.term -> Term.term
(** [aget a i] is the element [a[i]] ([map.Map]'s [get]). *)

val aset : Term.term -> Term.term -> Term.term -> Term.term
(** [aset a i v] is the array [a] with index [i] updated to [v] ([map.Map]'s
    [set]); other indices and the separately-tracked length are unchanged. *)

val azero : Term.term Lazy.t
(** The all-zeros element store an [array(n)] begins from ([map.Const]'s
    [const 0]). Lazy for the same reason as {!ty_int_map}. *)

val eq : Term.term -> Term.term -> Term.term
val neq : Term.term -> Term.term -> Term.term
val lt : Term.term -> Term.term -> Term.term
val leq : Term.term -> Term.term -> Term.term
val gt : Term.term -> Term.term -> Term.term

val geq : Term.term -> Term.term -> Term.term
(** The six integer comparison predicates ([=], [<>], [<], [<=], [>], [>=]),
    each building a proposition term from two integer terms. *)

val min_int : Term.term Lazy.t
(** The smallest representable machine integer, [-2^62] (OCaml 63-bit [int]).
    Lazy so that a consumer binding it at its own top level does not re-force
    the deferred Why3 theory layer at process startup. *)

val max_int : Term.term Lazy.t
(** The largest representable machine integer, [2^62 - 1]. Lazy for the same
    reason as {!min_int}. *)

val in_bounds : Term.term -> Term.term
(** [in_bounds t] is the proposition [min_int <= t <= max_int]: [t] fits in a
    machine integer. Used to build per-operation overflow-freedom obligations.
*)

val task_for : Term.term -> Task.task
(** The proof task to discharge a goal against: the integer theory, plus Why3's
    [int.ComputerDivision] and/or map theories iff the goal uses their symbols.
    Those axioms are withheld otherwise because Alt-Ergo can loop on them even
    when they are irrelevant. *)

val uses_div : Term.term -> bool
(** Whether a term applies the [div]/[mod] symbols. *)

val uses_map : Term.term -> bool
(** Whether a term applies the array [get]/[set]/[const] symbols. *)

val task : div:bool -> map:bool -> Task.task
(** [task ~div ~map] builds the proof task including the [ComputerDivision]
    and/or map theories as requested. Use over {!task_for} when map support is
    needed for a [map]-typed variable that the term never applies a symbol to
    (the type still requires the theory to be present). *)
