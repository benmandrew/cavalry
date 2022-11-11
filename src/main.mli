val get_ast : string -> Ast.cmd

val verify :
  Hoare.var_map -> Ast.cmd -> Why3.Term.term -> Why3.Term.term -> unit

val exec : string -> int
