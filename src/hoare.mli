module VarMap : sig
  type 'a t
end

type var_map = Why3.Term.vsymbol VarMap.t

val collect_variables : Ast.cmd -> Why3.Term.vsymbol VarMap.t
val wlp : var_map -> Ast.cmd -> Why3.Term.term -> Why3.Term.term
val verify : var_map -> Ast.cmd -> Why3.Term.term -> Why3.Term.term option
val get_var : var_map -> string -> Why3.Term.vsymbol
