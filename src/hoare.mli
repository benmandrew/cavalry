module T = Why3.Term
open Ast

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
    [safe]/[Arith.in_bounds]). The default reasons over unbounded integers.
    Loops are only fully handled once the [While] rule gains its overflow
    obligations (a later milestone). *)
