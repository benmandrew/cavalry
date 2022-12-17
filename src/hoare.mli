module T = Why3.Term

val wlp : Ast.Vars.t -> Ast.Program.cmd -> T.term -> T.term
val verify : ?timeout:int -> Ast.Vars.t -> Ast.Triple.t -> Smt.Prover.result
val get_var : Ast.Vars.t -> string -> T.vsymbol
