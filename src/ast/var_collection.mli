(** Assigning each program variable its Why3 [vsymbol], the bridge between the
    parsed AST and everything that reasons over terms ({!Logic}, [Hoare]). *)

val collect : Triple.t list -> (Triple.t * Vars.t) list
(** Walk the program -- procedures followed by [main] -- and pair each triple
    with the {!Vars.t} environment its terms resolve against.

    A name is a {e global} if it occurs in [main] or in any procedure's [writes]
    clause; every other name in a procedure is that procedure's {e local}. This
    split matters because a procedure only sees the globals it declares in
    [writes]. A name used as an array (indexed, [len]-ed, created, or
    element-assigned) is given a [map int int] symbol plus a companion length
    variable (see {!Vars.len_key}); all other names are scalar integers. *)
