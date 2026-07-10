(** Ahead-of-time backend: transpile the typed AST to OCaml source and build it
    into a native executable. Behind [cav compile], driven by {!Main.compile}.

    Cavalry integers are Why3's {e unbounded} mathematical integers -- the model
    {!Hoare.verify} proves against -- so {!emit} defaults to arbitrary-precision
    Zarith arithmetic, keeping the compiled program's integer model identical to
    the verified one. [native_int] instead emits OCaml's 63-bit [int], which
    wraps; it is sound only when paired with the machine-integer verification
    gate that proves overflow cannot happen (see {!Main.compile}). *)

open Ast

exception Unsupported of string
(** Raised for a Cavalry feature the backend cannot yet compile. Kept distinct
    from {!Toolchain_error} so the CLI can phrase each differently. *)

exception Toolchain_error of string
(** Raised when the external OCaml toolchain is missing or rejects the emitted
    source -- e.g. [ocamlfind]/[ocamlopt] not found, or a non-zero exit. *)

val emit : ?native_int:bool -> (Triple.t * Vars.t) list -> string
(** Transpile a verified program -- procedures followed by [main] -- to OCaml
    source. With [native_int] the emitted code computes in native [int];
    otherwise (default) in Zarith. This choice must match the one {!to_native}
    is given, so the source and the link line agree. *)

val to_native : ?native_int:bool -> output:string -> string -> unit
(** [to_native ~output source] builds the OCaml [source] into a native
    executable at [output] by shelling out to [ocamlfind ocamlopt], linking
    Zarith unless [native_int]. The source and ocamlopt's intermediate artifacts
    go in a temp file that is removed afterwards; only [output] survives.

    @raise Toolchain_error if the toolchain is absent or compilation fails. *)
