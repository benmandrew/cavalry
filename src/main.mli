module Ast = Ast

val get_ast : string -> Ast.Program.cmd

val verify :
  Hoare.var_map -> Ast.Program.cmd -> Why3.Term.term -> Why3.Term.term -> bool

val exec : string -> int
