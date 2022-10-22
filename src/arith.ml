open Why3

let int_theory : Theory.theory = Env.read_theory Prover.env [ "int" ] "Int"

let plus_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix +" ]

let mul_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix *" ]

let lt_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix <" ]

let plus a b = Term.t_app_infer plus_symbol [ a; b ]
let mul a b = Term.t_app_infer mul_symbol [ a; b ]

let int_task =
  let task = Task.add_ty_decl None Ty.ts_int in
  List.fold_left
    (fun task s -> Task.add_param_decl task s)
    task
    [ plus_symbol; mul_symbol; lt_symbol ]
