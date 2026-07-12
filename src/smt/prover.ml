open Core
open Why3

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

let main = lazy (Whyconf.get_main (Lazy.force config))

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

let env = lazy (Env.create_env (Whyconf.loadpath (Lazy.force main)))

let z3_driver =
  lazy
    (let open Format in
     try
       Driver.load_driver_for_prover (Lazy.force main) (Lazy.force env)
         (Lazy.force z3)
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
      | (_, model) :: _ ->
          Model_parser.get_model_elements model
          |> List.map ~f:(fun (e : Model_parser.model_element) ->
              ( Model_parser.get_lsymbol_or_model_trace_name e,
                e.Model_parser.me_concrete_value ))
          |> List.filter ~f:(fun (n, _) -> Set.mem expose_names n)
          |> List.dedup_and_sort ~compare:(fun (a, _) (b, _) ->
              String.compare a b))
