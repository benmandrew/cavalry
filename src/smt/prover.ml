open Core
open Why3

(* Browser override. In the browser there is no filesystem and no native Z3
   binary, so Why3's config detection ([init_config] / [why3 config detect])
   cannot run -- it fails trying to read [provers-detection-data.conf]. When
   [configure_browser] has been called, [main]/[env]/[z3_driver] are built
   instead from files embedded in the js_of_ocaml pseudo-filesystem: [env] reads
   theories from the embedded loadpath, and the driver is loaded from an embedded
   [.drv] via a hand-built [config_prover] with the datadir pinned to the
   embedded tree -- bypassing detection entirely. On native builds [browser] is
   [None] and the original Whyconf path runs unchanged.

   [ce_driver_file] is the embedded counterexamples driver (Z3's
   [z3_487_counterexample.drv]); it drives the browser's best-effort
   counterexample path the same way [z3_ce] does natively. *)
type browser_env = {
  loadpath : string list;
  driver_file : string;
  ce_driver_file : string;
}

let browser : browser_env option ref = ref None

let configure_browser ~loadpath ~driver_file ~ce_driver_file =
  browser := Some { loadpath; driver_file; ce_driver_file }

(* Why3 configuration and the Z3 driver are loaded *lazily*: OCaml evaluates
   every linked module's top-level bindings at process startup, so binding these
   eagerly made [cav run] -- which never proves anything -- pay the full Why3
   config-detection and driver-load cost. Deferring them behind [lazy] keeps
   that ~100 ms off the interpreter path; the thunks are forced from
   [run_prover]/[counterexample], the only places a prover is actually run. *)
let config =
  lazy
    (let dir =
       None
       (* (Some "/Users/benmandrew/.opam/cavalry/share/why3/provers-detection-data.conf") *)
     in
     Whyconf.init_config dir)

(* [main] is only needed to pass to [load_driver_for_prover]; the theory
   environment is built separately (see [env]). In browser mode its datadir is
   pinned to the embedded tree (the driver file's grandparent, i.e. [/why3]) so
   the driver's [import]s resolve there. Its loadpath is deliberately left as the
   default -- [set_loadpath] appends rather than replaces, which under node
   (where the opam datadir is also visible) would make a stdlib file resolvable
   two ways and clash as [AmbiguousPath]; env avoids [main]'s loadpath entirely. *)
let main =
  lazy
    (match !browser with
    | Some b ->
        let datadir = Filename.dirname (Filename.dirname b.driver_file) in
        Whyconf.set_datadir
          (Whyconf.get_main (Whyconf.default_config ""))
          datadir
    | None -> Whyconf.get_main (Lazy.force config))

(* Pinned to match the [z3] constraint in dune-project/cavalry.opam. The filter
   is versioned so that a why3 config pointing at a different Z3 fails fast here
   rather than silently discharging goals against a prover we haven't vetted.
   Z3 is used both for the yes/no verdict and (a later milestone) for the
   counterexample models Alt-Ergo could not produce usefully. *)
let z3_version = "4.16.0"

let z3 =
  lazy
    (let open Printf in
     let fp = Whyconf.parse_filter_prover ("Z3," ^ z3_version) in
     let provers = Whyconf.filter_provers (Lazy.force config) fp in
     (* [filter_provers] also matches Z3's alternative configurations, e.g. "Z3
        (counterexamples)" and "Z3 (noBV)"; pin the plain build (empty [altern])
        for a deterministic verdict prover. *)
     let base =
       Whyconf.Mprover.filter
         (fun p _ -> String.is_empty p.Whyconf.prover_altern)
         provers
     in
     let chosen = if Whyconf.Mprover.is_empty base then provers else base in
     if Whyconf.Mprover.is_empty chosen then (
       eprintf "Prover Z3 %s not installed or not configured\n" z3_version;
       exit 1);
     snd (Whyconf.Mprover.max_binding chosen))

let env =
  lazy
    (match !browser with
    | Some b -> Env.create_env b.loadpath
    | None -> Env.create_env (Whyconf.loadpath (Lazy.force main)))

(* Fabricate a [config_prover] for the embedded driver, so the browser path can
   call [load_driver_for_prover] without a detected prover. Only [driver] (the
   embedded [.drv] path) and [prover] matter here; [command] and the rest are
   unused because we never spawn -- we only print. *)
let browser_config_prover driver_file : Whyconf.config_prover =
  {
    Whyconf.prover =
      {
        Whyconf.prover_name = "Z3";
        prover_version = z3_version;
        prover_altern = "";
      };
    command = "";
    command_steps = None;
    driver = (None, driver_file);
    in_place = false;
    editor = "";
    interactive = false;
    extra_options = [];
    extra_drivers = [];
  }

let z3_driver =
  lazy
    (let open Format in
     try
       let cp =
         match !browser with
         | Some b -> browser_config_prover b.driver_file
         | None -> Lazy.force z3
       in
       Driver.load_driver_for_prover (Lazy.force main) (Lazy.force env) cp
     with e ->
       eprintf "Failed to load driver for Z3: %a\n" Exn_printer.exn_printer e;
       exit 1)

(* Z3's [counterexamples] alternative, loaded best-effort: it drives the
   *advisory* counterexample on a failed obligation, so if it is missing or its
   driver fails to load we simply produce no counterexample -- never affecting
   the verdict. *)
let z3_ce =
  lazy
    (try
       let fp =
         Whyconf.parse_filter_prover
           (Printf.sprintf "Z3,%s,counterexamples" z3_version)
       in
       let provers = Whyconf.filter_provers (Lazy.force config) fp in
       if Whyconf.Mprover.is_empty provers then None
       else
         let cp = snd (Whyconf.Mprover.max_binding provers) in
         Some
           ( cp,
             Driver.load_driver_for_prover (Lazy.force main) (Lazy.force env) cp
           )
     with _ -> None)

(* The browser counterpart to [z3_ce]. Native config detection is unavailable in
   the browser (see [config]), so the counterexamples driver is loaded from the
   embedded [.drv] via a hand-built [config_prover], exactly as [z3_driver] loads
   the plain one. [None] on native builds (the [z3_ce] path is used there) and,
   best-effort, if the embedded driver fails to load -- the browser then simply
   shows no counterexample. *)
let browser_ce_driver =
  lazy
    (match !browser with
    | None -> None
    | Some b -> (
        try
          let cp = browser_config_prover b.ce_driver_file in
          Some
            (Driver.load_driver_for_prover (Lazy.force main) (Lazy.force env) cp)
        with _ -> None))

type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

(* Confidence in an [Invalid] verdict. Z3's plain driver reports a genuine
   counter-model as [Unknown "sat"], not [Invalid]: on quantified goals (arrays,
   loop invariants) a [sat] can rest on incomplete quantifier instantiation, so
   the model may be spurious. Only a hard [Invalid] answer is therefore
   [Disproved] (the goal is definitely false); every [Unknown] is a [Candidate]
   whose witness is advisory. Meaningful only alongside an [Invalid] result. *)
type status = Disproved | Candidate [@@deriving sexp_of, compare]

let status_of_answer (answer : Call_provers.prover_answer) : status =
  match answer with Invalid -> Disproved | _ -> Candidate

(* Maps a prover's raw answer onto our tri-state result. Factored out of
   [prove] so the [Failed] branch can be tested with a stubbed answer rather
   than by provoking a real (slow, flaky) timeout. *)
let result_of_answer ~output (answer : Call_provers.prover_answer) : result =
  match answer with
  | Valid -> Valid
  | Invalid | Unknown _ -> Invalid
  | Timeout | OutOfMemory | StepLimitExceeded | HighFailure _ | Failure _ ->
      Failed output

(* Run Z3 on a task whose goal is already in place, returning its raw result.
   Shared by the whole-goal [prove] and the per-obligation [prove_term]. *)
let run_prover timeout task =
  let limit =
    match timeout with
    | None -> Call_provers.empty_limits
    | Some limit_time ->
        { Call_provers.limit_time; limit_mem = -1; limit_steps = -1 }
  in
  let main = Lazy.force main and z3 = Lazy.force z3 in
  Driver.prove_task ~limits:limit ~config:main ~command:z3.Whyconf.command
    (Lazy.force z3_driver) task
  |> Call_provers.wait_on_call

(* Serialise the goal [term] on [base_task] to the exact SMT-LIB2 text Z3 would
   receive, without spawning any prover. This is the browser path's substitute
   for [run_prover]: the returned string is handed to a Z3-wasm worker, whose
   [sat]/[unsat]/[unknown] answer is mapped back through [result_of_answer]. The
   driver's own logic file is reused so the theory prelude matches the native
   prove path exactly. *)
let smtlib_of_term base_task term =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_prop_decl base_task Decl.Pgoal goal_id term in
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Driver.print_task (Lazy.force z3_driver) fmt task;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let prove_term_status timeout base_task term =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_prop_decl base_task Decl.Pgoal goal_id term in
  let result = run_prover timeout task in
  ( result_of_answer ~output:result.pr_output result.pr_answer,
    status_of_answer result.pr_answer )

let prove_term timeout base_task term =
  fst (prove_term_status timeout base_task term)

let prove timeout base_task vars t =
  Term.t_forall_close vars [] t |> prove_term timeout base_task

(* Split the closed goal into one already-closed subgoal formula per obligation,
   each paired with its explanation attribute (the string the caller attached
   via an [expl:] attribute). Purely structural -- no prover is run -- so the
   caller can build a minimal per-subgoal task before discharging it. Splitting
   here rather than proving the whole conjunction at once is also what lets a
   failure be pinned to a single obligation. *)
(* First located node in a top-down traversal. A split subgoal has the shape
   [hypotheses -> obligation]; the obligation's atoms carry the source location
   (set by the caller on every node) while the [->]/hypotheses do not, so the
   first location found is the obligation's. *)
let rec first_loc t =
  match t.Term.t_loc with
  | Some _ as l -> l
  | None ->
      Term.t_fold
        (fun acc sub -> match acc with Some _ -> acc | None -> first_loc sub)
        None t

let split_obligations base_task vars t =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task =
    Task.add_prop_decl base_task Decl.Pgoal goal_id
      (Term.t_forall_close vars [] t)
  in
  Trans.apply Split_goal.split_goal_right task
  |> List.map ~f:(fun st ->
      let _, expl, _ = Termcode.goal_expl_task ~root:false st in
      let f = Task.task_goal_fmla st in
      (f, expl, first_loc f))

(* Turn the outer universally-quantified variables of a split subgoal
   [forall xs. body] into fresh 0-ary constants, each tagged [model_trace:<name>]
   and given a (dummy) location -- both required for Z3 to report the symbol in
   its model. Only the outermost block is opened (the entry-state variables);
   inner havoc quantifiers stay bound. Returns the constants to declare and the
   opened body. Which of these are shown to the user is decided by name in
   [counterexample], since the WLP alpha-renames variables so identity is not a
   reliable filter. *)
let skolemise f =
  match f.Term.t_node with
  | Term.Tquant (Term.Tforall, tq) ->
      let vsl, _, body = Term.t_open_quant tq in
      let consts, map =
        List.fold_left
          ~f:(fun (consts, map) v ->
            let name = v.Term.vs_name.Ident.id_string in
            let attrs =
              Ident.Sattr.singleton
                (Ident.create_attribute ("model_trace:" ^ name))
            in
            let loc = Loc.user_position "" 0 0 0 0 in
            let ls =
              Term.create_lsymbol
                (Ident.id_fresh ~attrs ~loc name)
                [] (Some v.Term.vs_ty)
            in
            ( ls :: consts,
              Term.Mvs.add v (Term.t_app ls [] (Some v.Term.vs_ty)) map ))
          ~init:([], Term.Mvs.empty) vsl
      in
      (consts, Term.t_subst map body)
  | _ -> ([], f)

(* Extract the [(name, value)] assignments a Why3 [model] gives the *source*
   variables named in [expose_names], dropping the WLP's internal havoc/variant
   vars (introduced existentially in the negation) and anything not exposed.
   Shared by the native and browser counterexample paths, which differ only in
   how they obtain the [model]. *)
let expose_model_elements expose_names model =
  Model_parser.get_model_elements model
  |> List.map ~f:(fun (e : Model_parser.model_element) ->
      ( Model_parser.get_lsymbol_or_model_trace_name e,
        e.Model_parser.me_concrete_value ))
  |> List.filter ~f:(fun (n, _) -> Set.mem expose_names n)
  |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)

(* Best-effort counterexample for a failed obligation [f] (a split subgoal):
   skolemise its entry-state variables into model-reportable constants, ask Z3's
   counterexamples driver for a model, and return the [(name, value)] pairs it
   assigns the *source* variables named in [expose]. Z3 also surfaces the WLP's
   internal havoc/frozen-variant vars (introduced existentially in the negation);
   those, and any not named among [expose], are dropped. Empty if no CE prover is
   configured or no model was produced. Values are returned as raw Why3
   [concrete_syntax_term]s (not printed strings) so the caller can, for example,
   expand an array literal into a concrete list using its length -- which lives
   in a separate [#len#] model entry the AST layer knows how to pair up. *)
let counterexample timeout base_task expose f =
  match Lazy.force z3_ce with
  | None -> []
  | Some (cp, driver) -> (
      let expose_names =
        List.map ~f:(fun v -> v.Term.vs_name.Ident.id_string) expose
        |> String.Set.of_list
      in
      let consts, body = skolemise f in
      let task = List.fold_left ~f:Task.add_param_decl ~init:base_task consts in
      let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
      let task = Task.add_prop_decl task Decl.Pgoal goal_id body in
      let limits =
        match timeout with
        | None -> Call_provers.empty_limits
        | Some limit_time ->
            { Call_provers.limit_time; limit_mem = -1; limit_steps = -1 }
      in
      let r =
        Driver.prove_task ~command:cp.Whyconf.command ~config:(Lazy.force main)
          ~limits driver task
        |> Call_provers.wait_on_call
      in
      match r.Call_provers.pr_models with
      | [] -> []
      | (_, model) :: _ -> expose_model_elements expose_names model)

(* {2 Browser counterexample path}

   Native [counterexample] both runs Z3 (via [wait_on_call]) and parses its
   model, because Why3 spawns the prover as a subprocess. In the browser Z3-wasm
   runs the solver in JavaScript, so the two halves are split: OCaml prints the
   counterexample obligation to SMT-LIB2 here ([smtlib_ce_of_obligation]), JS
   solves it, then hands the raw model text back for parsing ([browser_ce]). The
   [printing_info] the model parser needs -- Why3's map from the printed SMT
   names back to source symbols -- is retained across that async round-trip in
   [ce_state], keyed by an integer handle returned to JS with the SMT-LIB2. *)

let ce_state : (int, Printer.printing_info * String.Set.t) Hashtbl.t =
  Hashtbl.create (module Int)

let ce_counter = ref 0

(* Clear the retained [printing_info]s and restart handles from 0. Called at the
   start of each obligation-printing pass so a superseded run's state cannot
   accumulate or be mistaken for the current one's. *)
let reset_browser_ce () =
  Hashtbl.clear ce_state;
  ce_counter := 0

(* Print the counterexample obligation for [f] to SMT-LIB2 and stash the
   [printing_info] under a fresh handle, returned with the text. The
   counterexamples driver makes the printer emit [(set-option :produce-models
   true)] and a trailing [(get-model)], so Z3's output on [sat] carries the model
   for [browser_ce] to parse. [None] if no CE driver is loaded (native, or a
   failed embedded load). *)
let smtlib_ce_of_obligation base_task expose f =
  match Lazy.force browser_ce_driver with
  | None -> None
  | Some driver ->
      let consts, body = skolemise f in
      let task = List.fold_left ~f:Task.add_param_decl ~init:base_task consts in
      let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
      let task = Task.add_prop_decl task Decl.Pgoal goal_id body in
      let task = Driver.prepare_task driver task in
      let buf = Buffer.create 1024 in
      let fmt = Format.formatter_of_buffer buf in
      let info = Driver.print_task_prepared driver fmt task in
      Format.pp_print_flush fmt ();
      let expose_names =
        List.map ~f:(fun v -> v.Term.vs_name.Ident.id_string) expose
        |> String.Set.of_list
      in
      let id = !ce_counter in
      Int.incr ce_counter;
      Hashtbl.set ce_state ~key:id ~data:(info, expose_names);
      Some (Buffer.contents buf, id)

let smtv2_model_parser = lazy (Model_parser.lookup_model_parser "smtv2")

(* Z3's raw output is the answer token ([sat]/[unknown]) followed by the model
   s-expression; the smtv2 model parser wants that s-expression alone (native
   [Call_provers] likewise feeds it only the post-answer text). Slice from the
   first [(]. *)
let model_sexp_of_output out =
  match String.index out '(' with
  | Some i -> String.subo out ~pos:i
  | None -> ""

(* Parse the model Z3-wasm produced for the obligation printed under [id],
   returning the same [(name, value)] pairs as native [counterexample]: the
   source variables named in the stashed expose set, dropping WLP-internal vars.
   Empty if the handle is unknown (a superseded run) or no model was produced. *)
let browser_ce id output =
  match Hashtbl.find ce_state id with
  | None -> []
  | Some (info, expose_names) ->
      let sexp = model_sexp_of_output output in
      if String.is_empty (String.strip sexp) then []
      else
        let model = (Lazy.force smtv2_model_parser) info sexp in
        expose_model_elements expose_names model
