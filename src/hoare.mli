module T = Why3.Term
open Ast

module Proc_map : sig
  type 'a t
end

val verify :
  ?debug:bool -> ?timeout:float -> (Triple.t * Vars.t) list -> Smt.Prover.result
