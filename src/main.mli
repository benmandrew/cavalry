module Ast = Ast
open Ast

val get_ast : string -> (Triple.t * Vars.t) list

val verify :
  ?debug:bool ->
  ?timeout:float ->
  ?machine_int:bool ->
  (Triple.t * Vars.t) list ->
  Smt.Prover.result

val exec : string -> int

exception Verification_failed of string
(** Raised by [compile] when its verification gate rejects the program. *)

val compile :
  ?debug:bool ->
  ?verify:bool ->
  ?native_int:bool ->
  output:string ->
  string ->
  unit
(** [compile ~output path] transpiles the program at [path] to OCaml and builds
    a native executable at [output]. With [debug], the generated OCaml is also
    written to stdout. Unless [verify] is [false] (default [true]), the program
    is verified first and nothing is emitted if it fails. With [native_int], the
    binary computes in wrapping 63-bit [int] rather than the default unbounded
    Zarith integers -- faster, but unsound on overflow. May raise
    [Verification_failed], [Compile.Unsupported], or [Compile.Toolchain_error].
*)
