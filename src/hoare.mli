module T = Why3.Term

val wlp : Ast.Vars.t -> Ast.Program.cmd -> T.term -> T.term
val verify : Ast.Vars.t -> Ast.Triple.t -> bool
val get_var : Ast.Vars.t -> string -> T.vsymbol
