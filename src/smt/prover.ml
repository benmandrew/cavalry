open Core
open Why3

let config = Whyconf.init_config (Some "/Users/benmandrew/.opam/cavalry/share/why3/provers-detection-data.conf")
let main = Whyconf.get_main config
(* let provers = Whyconf.get_provers config *)

let alt_ergo =
  let open Printf in
  let fp = Whyconf.parse_filter_prover "Alt-Ergo" in
  let provers = Whyconf.filter_provers config fp in
  if Whyconf.Mprover.is_empty provers then (
    eprintf "Prover Alt-Ergo not installed or not configured\n";
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

let prove timeout base_task term =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_prop_decl base_task Decl.Pgoal goal_id term in
  let limit =
    match timeout with
    | None -> Call_provers.empty_limit
    | Some limit_time ->
        Call_provers.{ limit_time; limit_mem = -1; limit_steps = -1 }
  in
  let result =
    Driver.prove_task ~limit ~config:main ~command:alt_ergo.Whyconf.command
      alt_ergo_driver task
    |> Call_provers.wait_on_call
  in
  match (result.pr_answer : Call_provers.prover_answer) with
  | Valid -> Valid
  | Invalid | Unknown _ -> Invalid
  | Timeout | OutOfMemory | StepLimitExceeded | HighFailure | Failure _ ->
      Failed result.pr_output

let prove_implies timeout base_task vars t t' =
  Term.t_forall_close vars [] (Term.t_implies t t') |> prove timeout base_task
