type proc_t = { f : string; ps : string list; c : Program.cmd }

(* The final-state environment of a run, keyed by variable name. *)
module Env : Map.S with type key = string

val to_proc_t : Triple.t -> proc_t
val exec : proc_t list -> int

(* Outcome of a fuel-bounded run; [Terminated] carries the final global
   environment. [OutOfFuel] also covers detected arithmetic overflow. *)
type exec_result = Terminated of int Env.t | OutOfFuel | Raised

val exec_env : ?fuel:int -> proc_t list -> exec_result
(** Fuel-bounded interpreter returning the final global environment rather than
    the entrypoint value. Non-terminating runs (loops exceeding [fuel]) and
    overflowing runs yield [OutOfFuel]; a differential harness discards these.
    [fuel] bounds total loop iterations (default 10000). *)
