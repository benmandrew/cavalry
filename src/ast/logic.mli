(** The assertion language: the AST of the logical annotations a program carries
    -- preconditions, postconditions, loop invariants and variants -- together
    with its translation into Why3 terms for the prover.

    Unlike {!Program}'s expressions these are untyped (a flat [arith_expr] /
    [logic_expr] split rather than a GADT), since assertions are parsed
    separately and never interpreted, only translated. *)

module T = Why3.Term

(* Untyped AST to play nice with the Menhir parser generator *)

(** Integer-valued assertion terms: literals, program/quantifier variables,
    arithmetic, and the array projections [Get] ([a[i]]) and [Len] ([len(a)]).
*)
type arith_expr =
  | Int of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Sub of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
  | Div of arith_expr * arith_expr
  | Mod of arith_expr * arith_expr
  | Get of string * arith_expr
  | Len of string
[@@deriving sexp_of, show]

(** Proposition-valued assertion terms: a first-order logic over the
    {!arith_expr} comparisons, closed under the connectives and quantifiers. *)
type logic_expr =
  | Bool of bool
  | Not of logic_expr
  | And of logic_expr * logic_expr
  | Or of logic_expr * logic_expr
  | Impl of logic_expr * logic_expr
  | Eq of arith_expr * arith_expr
  | Neq of arith_expr * arith_expr
  | Lt of arith_expr * arith_expr
  | Leq of arith_expr * arith_expr
  | Gt of arith_expr * arith_expr
  | Geq of arith_expr * arith_expr
  | Forall of string * logic_expr
  | Exists of string * logic_expr
[@@deriving sexp_of, show]

type expr = logic_expr [@@deriving sexp_of, show]
(** The top-level assertion form -- a proposition. Alias for {!logic_expr}. *)

val translate_arith_term :
  g_vars:Vars.t -> ?l_vars:Vars.t -> ?bound:Vars.t -> arith_expr -> T.term
(** Translate an arithmetic assertion term (e.g. a loop variant's measure) to a
    Why3 term, resolving names the same way as {!translate_term}. *)

val translate_term :
  g_vars:Vars.t -> ?l_vars:Vars.t -> ?bound:Vars.t -> expr -> T.term
(** Translate a proposition to a Why3 term. Names resolve against [g_vars], then
    the optional procedure-locals [l_vars]; quantifier-bound names ([bound],
    extended as [Forall]/[Exists] are entered) shadow both. *)

val print_expr : expr -> unit
(** Print an assertion's AST as an s-expression to stdout. For [-d] debug
    output. *)

val print_term : T.term -> unit
(** Pretty-print a translated Why3 term to stdout. For [-d] debug output. *)
