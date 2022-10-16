(* open Cavalry

   let _ = Printf.printf "%d\n" (Main.exec "a.cvl") *)

open Why3

let fmla_true = Term.t_true
let fmla_false = Term.t_false
let fmla1 = Term.t_or fmla_true fmla_false
let prop_var_A = Term.create_psymbol (Ident.id_fresh "A") []
let prop_var_B = Term.create_psymbol (Ident.id_fresh "B") []
let atom_A = Term.ps_app prop_var_A []
let atom_B = Term.ps_app prop_var_B []
let fmla2 = Term.t_implies (Term.t_and atom_A atom_B) atom_A
let task1 : Task.task = None
let goal_id1 = Decl.create_prsymbol (Ident.id_fresh "goal1")
let task1 = Task.add_prop_decl task1 Decl.Pgoal goal_id1 fmla1
let task2 : Task.task = None
let task2 = Task.add_param_decl task2 prop_var_A
let task2 = Task.add_param_decl task2 prop_var_B
let goal_id2 = Decl.create_prsymbol (Ident.id_fresh "goal2")
let task2 = Task.add_prop_decl task2 Decl.Pgoal goal_id2 fmla2

open Format

let () =
  printf "@[task 1 is:@\n%a@]@." Pretty.print_task task1;
  printf "@[task 2 is:@\n%a@]@." Pretty.print_task task2

let config = Whyconf.init_config None
let main = Whyconf.get_main config
let libdir = Whyconf.libdir main
let datadir = Whyconf.datadir main
(* let provers = Whyconf.get_provers config *)

let alt_ergo =
  let fp = Whyconf.parse_filter_prover "Alt-Ergo" in
  let provers = Whyconf.filter_provers config fp in
  if Whyconf.Mprover.is_empty provers then (
    eprintf "Prover Alt-Ergo not installed or not configured@.";
    exit 1)
  else (
    printf "Versions of Alt-Ergo found:";
    Whyconf.(Mprover.iter (fun k _ -> printf "%s" k.prover_version) provers);
    printf "@.";
    snd (Whyconf.Mprover.max_binding provers))

let env = Env.create_env (Whyconf.loadpath main)

let alt_ergo_driver =
  try Whyconf.load_driver main env alt_ergo
  with e ->
    eprintf "Failed to load driver for alt-ergo: %a@." Exn_printer.exn_printer e;
    exit 1

let result2 =
  Call_provers.wait_on_call
    (Driver.prove_task ~limit:Call_provers.empty_limit ~libdir ~datadir
       ~command:alt_ergo.Whyconf.command alt_ergo_driver task2)

let () =
  printf "@[On task 2, Alt-Ergo answers %a@."
    (Call_provers.print_prover_result ?json:None)
    result2
