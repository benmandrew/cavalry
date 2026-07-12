(** Well-typedness gate over the untyped AST, run before {!Triple.translate}
    elaborates it. Classifies every name (scalar vs array), checks each
    expression against its expected type, and validates procedure calls and
    optional parameter annotations.

    Accepts exactly the programs that verified before -- the only inhabited
    variable types are [int] and [int] arrays -- while turning previously silent
    mistakes (a name used as both a scalar and an array, a wrong-arity or
    undeclared call, a boolean where an integer is required) into located
    diagnostics. *)

exception Type_error of Loc.t option * string
(** The source location of the offending construct (when known) and a
    human-readable message. *)

val check : Triple.ut_t list -> unit
(** @raise Type_error if the program is not well-typed. *)
