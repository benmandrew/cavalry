(** Render a Why3 {!Why3.Term.term} back to Cavalry surface syntax, inverting
    the encoding {!Arith} and {!Logic} build. Used to display the WLP assertions
    the verifier threads between statements (see {!Hoare.proof_outline}).

    The printer undoes the internal conventions the WLP introduces: [#len#a]
    prints as [len(a)], an [_x] snapshot as [old(x)], and the boolean coercions
    [if b then True else False] / [x = True] as the bare formula [b] / [x].
    Parenthesisation follows the operator precedence of `parse/tokens.mly`. *)

val to_string : Why3.Term.term -> string

val simplify : Why3.Term.term -> Why3.Term.term
(** Apply the one-point rule [forall y. y = t -> P  ==>  P[y := t]] (for [y] not
    free in [t]) throughout the term. The WLP encodes each scalar assignment as
    a quantifier of this shape, so simplifying recovers the substituted
    assertion a proof outline should show. Also drops vacuous quantifiers (a
    [forall]/[exists] binding no variable its body uses). Loop-havoc quantifiers
    do not match and are left intact. Logically equivalence-preserving; purely
    for readability. *)
