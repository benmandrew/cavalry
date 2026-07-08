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
  ?debug:bool -> ?timeout:float -> (Triple.t * Vars.t) list -> Smt.Prover.result
