(** The SMT backend: a thin wrapper over Why3 driving Z3. The prover (pinned to
    4.16.0) and its driver are loaded {e lazily} on the first proof, failing
    fast with [exit 1] if [why3 config detect] has not found it. Deferring the
    load keeps [cav run], which never proves anything, off that cost -- OCaml
    otherwise runs a linked module's top-level bindings at process startup. *)

open Why3

val configure_browser :
  loadpath:string list -> driver_file:string -> ce_driver_file:string -> unit
(** Switch the SMT layer to the browser path: build {!env} and the print drivers
    from files embedded in the js_of_ocaml pseudo-filesystem instead of via Why3
    config detection (which needs a real filesystem and Z3 binary). Must be
    called -- with the embedded stdlib loadpath, an embedded plain [.drv], and
    the embedded counterexamples [.drv] -- before anything forces {!env}. A
    no-op's inverse: on native builds this is never called and Whyconf detection
    runs as before. *)

val env : Env.env Lazy.t
(** The Why3 environment (theory load path), shared so callers can build tasks
    that reference the standard library. Forced on first use (see the module
    note on lazy loading). *)

(** A goal's outcome: [Valid] (proven), [Invalid] (a counter-model or [unknown]
    -- not proven), or [Failed] (timeout, out-of-memory, or prover error),
    carrying the raw prover output. *)
type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

(** Confidence in an {!Invalid} verdict: [Disproved] (a hard [Invalid] answer --
    the goal is definitely false) or [Candidate] (an [unknown]/[sat] answer
    whose model may be spurious under incomplete quantifier reasoning). Z3's
    plain driver reports even a genuine refutation as [Unknown "sat"], so a
    verified counterexample is in practice a [Candidate]. Meaningful only
    alongside an [Invalid] {!result}. *)
type status = Disproved | Candidate [@@deriving sexp_of, compare]

val result_of_answer : output:string -> Call_provers.prover_answer -> result
(** Maps a prover's raw answer onto {!result}. Exposed so the [Failed] branch
    can be exercised with a stubbed answer instead of a real timeout. *)

val status_of_answer : Call_provers.prover_answer -> status
(** Classifies a raw answer's confidence for {!type-status}. Exposed for the
    same stubbed-answer testing as {!result_of_answer}. *)

val smtlib_of_term : Task.task -> Term.term -> string
(** [smtlib_of_term task term] serialises [term] as the goal on [task] to the
    SMT-LIB2 text Z3 would receive, {e without} running any prover. The browser
    path prints obligations with this and solves them in a Z3-wasm worker
    instead of {!prove_term}. *)

val prove :
  float option -> Task.task -> Term.vsymbol list -> Term.term -> result
(** [prove timeout task vars goal] universally closes [goal] over [vars], adds
    it to [task] as the goal, and runs Alt-Ergo. [timeout] is a per-goal time
    limit in seconds ([None] for unlimited). *)

val prove_term : float option -> Task.task -> Term.term -> result
(** As {!prove} but for a term that is already the goal (no universal closure).
    Used to discharge an individual split obligation. *)

val prove_term_status :
  float option -> Task.task -> Term.term -> result * status
(** As {!prove_term}, but also reports the {!type-status} of the verdict so a
    caller can tell a confirmed refutation from a candidate one. *)

val split_obligations :
  Task.task ->
  Term.vsymbol list ->
  Term.term ->
  (Term.term * string * Loc.position option) list
(** [split_obligations task vars goal] closes [goal] over [vars] and splits it
    with [split_goal_right] into one already-closed subgoal per obligation, each
    paired with its explanation attribute and its source location (the first
    located node in the subgoal, [None] if the obligation was untagged). No
    prover is run: the caller builds a suitable per-subgoal task (e.g.
    minimising theory inclusion) and discharges it with {!prove_term}. *)

val counterexample :
  float option ->
  Task.task ->
  Term.vsymbol list ->
  Term.term ->
  (string * Model_parser.concrete_syntax_term) list
(** [counterexample timeout task expose f] asks Z3's counterexamples driver for
    a model of a failed obligation [f] (a subgoal from {!split_obligations}, on
    [task]'s theories), returning the [(variable, value)] pairs it assigns the
    variables in [expose] (the procedure's entry-state variables). Values are
    raw [Model_parser.concrete_syntax_term]s so the caller can shape them for
    display -- notably expanding an array's map literal into a concrete list.
    Best-effort and advisory: [[]] if no CE prover is configured or no model was
    produced, so it never affects the verdict. *)

val reset_browser_ce : unit -> unit
(** Clear the state {!smtlib_ce_of_obligation} retains between printing an
    obligation and parsing its model. Call at the start of each obligation pass
    so a superseded run cannot accumulate or leak state into the next. *)

val smtlib_ce_of_obligation :
  Task.task -> Term.vsymbol list -> Term.term -> (string * int) option
(** The browser split of {!counterexample}'s printing half: serialise the
    counterexample obligation for [f] (exposing the entry-state variables
    [expose]) to SMT-LIB2 whose Z3 output carries a model, and stash the Why3
    [printing_info] under a fresh integer handle. Returns [(smtlib, handle)];
    the caller solves [smtlib] in Z3-wasm and passes the output back with
    [handle] to {!browser_ce}. [None] if no counterexamples driver is loaded
    (native builds, or a failed embedded load). *)

val browser_ce :
  int -> string -> (string * Model_parser.concrete_syntax_term) list
(** [browser_ce handle output] parses the Z3-wasm [output] for the obligation
    printed under [handle] by {!smtlib_ce_of_obligation}, returning the same
    exposed [(variable, value)] pairs as native {!counterexample}. [[]] if the
    handle is unknown (a superseded run cleared by {!reset_browser_ce}) or no
    model was produced. *)
