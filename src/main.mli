module Ast = Ast
open Ast

val get_ast : string -> (Triple.t * Vars.t) list

val verify :
  ?debug:bool -> ?timeout:float -> (Triple.t * Vars.t) list -> Smt.Prover.result

val exec : string -> int

val compile : ?debug:bool -> output:string -> string -> unit
(** [compile ~output path] transpiles the program at [path] to OCaml and builds
    a native executable at [output]. With [debug], the generated OCaml is also
    written to stdout. May raise [Compile.Unsupported] or
    [Compile.Toolchain_error]. *)
