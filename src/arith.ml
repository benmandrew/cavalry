open Why3

module T = Term

let int_theory : Theory.theory =
  Env.read_theory Prover.env [ "int" ] "Int"

let plus_symbol : T.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix +" ]

let mul_symbol : T.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix *" ]

let lt_symbol : T.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export [ "infix <" ]

let plus a b = T.t_app plus_symbol [ a; b ] (Some Ty.ty_int)
let mul a b = T.t_app mul_symbol [ a; b ] (Some Ty.ty_int)
let lt a b = T.ps_app lt_symbol [ a; b ]

let int_task =
  let task = Task.add_ty_decl None Ty.ts_int in
  let task = Task.use_export task int_theory in
  List.fold_left
    (fun task s -> Task.add_param_decl task s)
    task
    [ plus_symbol; mul_symbol; lt_symbol ]
