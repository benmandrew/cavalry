module Ast = Ast

val get_ast : string -> Ast.Triple.t list
val verify : ?timeout:int -> Ast.Vars.t -> Ast.Triple.t -> Smt.Prover.result
val exec : string -> int
