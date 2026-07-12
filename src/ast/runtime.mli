(** The tree-walking interpreter behind [cav run]: it executes the typed
    {!Program.cmd} AST directly, ignoring the pre/postcondition annotations that
    [Hoare] consumes. Runs the same procedure list the verifier does, with the
    last one taken as [main]. *)

type proc_t = {
  f : string;
  ps : string list;
  c : Program.cmd;
  result : string option;
}
(** A runnable procedure: name [f], formal parameters [ps], body [c], and the
    name of its result binder [result] (if it returns a value). The
    interpreter's stripped-down view of a {!Triple.t} -- the logical annotations
    are dropped. *)

module Env : Map.S with type key = string
(** The final-state environment of a run, keyed by variable name. *)

val to_proc_t : Triple.t -> proc_t
(** Project a verified {!Triple.t} down to its runnable {!proc_t}, discarding
    the pre/postcondition, variant, and [writes] clause. *)

val exec : proc_t list -> int
(** Interpret a program -- the procedure list with [main] last -- and return the
    integer [main] evaluates to. Used by [cav run]. *)

(** Outcome of a fuel-bounded run; [Terminated] carries the final global
    environment. [OutOfFuel] also covers detected arithmetic overflow. *)
type exec_result = Terminated of int Env.t | OutOfFuel | Raised

val exec_env : ?fuel:int -> proc_t list -> exec_result
(** Fuel-bounded interpreter returning the final global environment rather than
    the entrypoint value. Non-terminating runs (loops exceeding [fuel]) and
    overflowing runs yield [OutOfFuel]; a differential harness discards these.
    [fuel] bounds total loop iterations (default 10000). *)
