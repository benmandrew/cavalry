module T = Why3.Term

val collect_variables : Ast.Program.cmd -> Ast.Vars.t
val wlp : Ast.Vars.t -> Ast.Program.cmd -> T.term -> T.term
val verify : Ast.Vars.t -> Ast.Program.program -> bool
val get_var : Ast.Vars.t -> string -> T.vsymbol
