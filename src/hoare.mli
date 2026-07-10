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
