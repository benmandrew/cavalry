open Core
open Why3

let config =
  let dir =
    None
    (* (Some "/Users/benmandrew/.opam/cavalry/share/why3/provers-detection-data.conf") *)
  in
  Whyconf.init_config dir

let main = Whyconf.get_main config

(* Pinned to match the [alt-ergo] constraint in dune-project/cavalry.opam.
   The filter is versioned so that a why3 config pointing at a different
   Alt-Ergo (e.g. a stray 2.6.2 on PATH) fails fast here rather than silently
   discharging goals against a prover we haven't vetted. *)
let alt_ergo_version = "2.4.3"

let alt_ergo =
  let open Printf in
  let fp = Whyconf.parse_filter_prover ("Alt-Ergo," ^ alt_ergo_version) in
  let provers = Whyconf.filter_provers config fp in
  if Whyconf.Mprover.is_empty provers then (
    eprintf "Prover Alt-Ergo %s not installed or not configured\n"
      alt_ergo_version;
    exit 1);
  snd (Whyconf.Mprover.max_binding provers)

let env = Env.create_env (Whyconf.loadpath main)

let alt_ergo_driver =
  let open Format in
  try Driver.load_driver_for_prover main env alt_ergo
  with e ->
    eprintf "Failed to load driver for alt-ergo: %a\n" Exn_printer.exn_printer e;
    exit 1

type result = Valid | Invalid | Failed of string [@@deriving sexp_of, ord]

(* Maps a prover's raw answer onto our tri-state result. Factored out of
   [prove] so the [Failed] branch can be tested with a stubbed answer rather
   than by provoking a real (slow, flaky) timeout. *)
let result_of_answer ~output (answer : Call_provers.prover_answer) : result =
  match answer with
  | Valid -> Valid
  | Invalid | Unknown _ -> Invalid
  | Timeout | OutOfMemory | StepLimitExceeded | HighFailure _ | Failure _ ->
      Failed output

(* Run Alt-Ergo on a task whose goal is already in place, returning its raw
   result. Shared by the whole-goal [prove] and the per-obligation [prove_term]. *)
let run_prover timeout task =
  let limit =
    match timeout with
    | None -> Call_provers.empty_limits
    | Some limit_time ->
        { Call_provers.limit_time; limit_mem = -1; limit_steps = -1 }
  in
  Driver.prove_task ~limits:limit ~config:main ~command:alt_ergo.Whyconf.command
    alt_ergo_driver task
  |> Call_provers.wait_on_call

let prove_term timeout base_task term =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_prop_decl base_task Decl.Pgoal goal_id term in
  let result = run_prover timeout task in
  result_of_answer ~output:result.pr_output result.pr_answer

let prove timeout base_task vars t =
  Term.t_forall_close vars [] t |> prove_term timeout base_task

(* Split the closed goal into one already-closed subgoal formula per obligation,
   each paired with its explanation attribute (the string the caller attached
   via an [expl:] attribute). Purely structural -- no prover is run -- so the
   caller can build a minimal per-subgoal task before discharging it. Splitting
   here rather than proving the whole conjunction at once is also what lets a
   failure be pinned to a single obligation. *)
let split_obligations base_task vars t =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task =
    Task.add_prop_decl base_task Decl.Pgoal goal_id
      (Term.t_forall_close vars [] t)
  in
  Trans.apply Split_goal.split_goal_right task
  |> List.map ~f:(fun st ->
      let _, expl, _ = Termcode.goal_expl_task ~root:false st in
      (Task.task_goal_fmla st, expl))
