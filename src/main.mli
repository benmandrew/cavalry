val get_ast : string -> Ast.Program.program
val verify : Ast.Vars.t -> Ast.Program.program -> bool
val exec : string -> int
