val get_ast : string -> Ast.Triple.t
val verify : Ast.Vars.t -> Ast.Triple.t -> bool
val exec : string -> int
