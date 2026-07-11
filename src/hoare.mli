(** The verifier: the weakest-liberal-precondition calculus that turns each
    Hoare triple [{p} c {q}] into a Why3 goal [p -> wlp(c, q)] and discharges it
    through {!Smt.Prover}.

    Procedures are verified bottom-up (a callee before any caller) and calls are
    handled by substitution and havoc rather than inlining, so a procedure's
    body is proven once against its own contract. *)

module T = Why3.Term
open Ast

(** Procedures in scope during WLP, keyed by name, so a call site can look up
    its callee's contract. *)
module Proc_map : sig
  type 'a t
end

val safe : g_vars:Vars.t -> ?l_vars:Vars.t -> 'a Program.expr -> T.term
(** [safe e] is the overflow-freedom obligation for [e]: the conjunction of
    [Arith.in_bounds] over every arithmetic result [e] computes. It is [true]
    for expressions built only from literals, variables, and comparisons. *)

val verify :
  ?debug:bool ->
  ?timeout:float ->
  ?machine_int:bool ->
  (Triple.t * Vars.t) list ->
  Smt.Prover.result
(** With [machine_int] (default [false]), verify against OCaml's 63-bit machine
    integers: each arithmetic operation must be proven not to overflow (see
    [safe]/[Arith.in_bounds]), including inside loops and across procedure
    calls. The default reasons over unbounded integers. *)

(** Why a verification obligation failed. Recovered from the failing subgoal's
    explanation attribute; see {!expl_of_reason} for the human wording. *)
type reason =
  | Postcondition
  | Loop_invariant_init
  | Loop_invariant_preserved
  | Loop_variant
  | Recursive_variant
  | Call_precondition
  | Array_bounds
  | Array_length_nonneg
  | Nonzero_divisor
  | No_overflow
  | Undeclared_write
[@@deriving sexp_of, compare]

val expl_of_reason : reason -> string
(** A one-line, user-facing explanation of a {!type-reason}. *)

type report = {
  result : Smt.Prover.result;
  failing_proc : string option;
  reason : reason option;
}
(** A whole-program verification outcome. [failing_proc] names the first
    procedure ([main] included, under the name ["main"]) whose body failed to
    verify, and is [None] iff [result] is [Valid]. [reason] classifies the
    failure when the prover isolated it to one obligation ([None] if it could
    not, e.g. an internal [Failed]). *)

val verify_report :
  ?debug:bool ->
  ?timeout:float ->
  ?machine_int:bool ->
  (Triple.t * Vars.t) list ->
  report
(** As {!verify}, but also reports which procedure was rejected. {!verify} is
    [(verify_report ...).result]. *)
