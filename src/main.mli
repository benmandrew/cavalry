(** The library entry point wiring the pipeline together: parse a source file,
    then either verify it, run it, or compile it. The [cav] CLI in [bin/main.ml]
    is a thin cmdliner shell over these. *)

module Ast = Ast
open Ast

val get_ast : string -> (Triple.t * Vars.t) list
(** Parse the source file at the given path and run variable collection,
    producing the annotated triples that {!verify} and {!Hoare} consume. *)

val verify :
  ?debug:bool ->
  ?timeout:float ->
  ?machine_int:bool ->
  (Triple.t * Vars.t) list ->
  Smt.Prover.result
(** Verify a parsed program. See {!Hoare.verify} for [machine_int] and the other
    parameters. *)

val verify_report :
  ?debug:bool ->
  ?timeout:float ->
  ?machine_int:bool ->
  (Triple.t * Vars.t) list ->
  Hoare.report
(** As {!verify}, but also reports which procedure was rejected. Backs the
    [cav verify] CLI so it can name the failing procedure. *)

val exec : string -> int
(** Parse and interpret the source file at the given path, returning [main]'s
    result. Backs [cav run]. *)

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
    binary computes in 63-bit machine [int] rather than the default unbounded
    Zarith integers; the gate then verifies against 63-bit integers too, proving
    overflow-freedom, so the faster native-int binary is still sound (only
    [verify = false] forfeits that). May raise [Verification_failed],
    [Compile.Unsupported], or [Compile.Toolchain_error]. *)
