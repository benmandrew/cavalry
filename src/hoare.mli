module T = Why3.Term

module ProcMap : sig
  type 'a t
end

val wlp :
  Ast.Triple.t ProcMap.t -> Ast.Vars.t -> Ast.Program.cmd -> T.term -> T.term

val verify : ?timeout:float -> Ast.Vars.t -> Ast.Triple.t -> Smt.Prover.result
