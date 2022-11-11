open Why3

let config = Whyconf.init_config None
let main = Whyconf.get_main config
let libdir = Whyconf.libdir main
let datadir = Whyconf.datadir main
(* let provers = Whyconf.get_provers config *)

let alt_ergo =
  let open Printf in
  let fp = Whyconf.parse_filter_prover "Alt-Ergo" in
  let provers = Whyconf.filter_provers config fp in
  if Whyconf.Mprover.is_empty provers then (
    eprintf "Prover Alt-Ergo not installed or not configured\n";
    exit 1)
  else printf "Versions of Alt-Ergo found:\n";
  Whyconf.(Mprover.iter (fun k _ -> printf "- %s\n" k.prover_version) provers);
  printf "\n";
  snd (Whyconf.Mprover.max_binding provers)

let env = Env.create_env (Whyconf.loadpath main)

let alt_ergo_driver =
  let open Format in
  try Whyconf.load_driver main env alt_ergo
  with e ->
    eprintf "Failed to load driver for alt-ergo: %a\n" Exn_printer.exn_printer e;
    exit 1

let prove base_task term =
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "goal") in
  let task = Task.add_prop_decl base_task Decl.Pgoal goal_id term in
  let result =
    Driver.prove_task ~limit:Call_provers.empty_limit ~libdir ~datadir
      ~command:alt_ergo.Whyconf.command alt_ergo_driver task
    |> Call_provers.wait_on_call
  in
  let open Call_provers in
  (* Format.printf "@[term =@ %a@]@." Pretty.print_term term; *)
  match result.pr_answer with
  | Valid -> true
  | Invalid -> false
  | Timeout | OutOfMemory | StepLimitExceeded | HighFailure | Unknown _
  | Failure _ ->
      Printf.printf "output: %s\n" result.pr_output;
      false

let prove_implies base_task vars t t' =
  Term.t_forall_close vars [] (Term.t_implies t t') |> prove base_task
