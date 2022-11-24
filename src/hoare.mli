module VarMap : sig
  type 'a t
end

type var_map = Why3.Term.vsymbol VarMap.t

val collect_variables : Ast.Program.cmd -> Why3.Term.vsymbol VarMap.t
val wlp : var_map -> Ast.Program.cmd -> Why3.Term.term -> Why3.Term.term

val verify :
  var_map -> Ast.Program.cmd -> Why3.Term.term -> Why3.Term.term -> bool

val get_var : var_map -> string -> Why3.Term.vsymbol
