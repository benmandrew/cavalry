module Ast = Ast
open Ast

val get_ast : string -> (Triple.t * Vars.t) list

val verify :
  ?debug:bool -> ?timeout:float -> (Triple.t * Vars.t) list -> Smt.Prover.result

val exec : string -> int
