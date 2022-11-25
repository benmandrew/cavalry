open Why3
module T = Term

let int_theory : Theory.theory = Env.read_theory Smt.Prover.env [ "int" ] "Int"

let find_symbol s = Theory.ns_find_ls int_theory.Theory.th_export [ s ]

let plus_symbol = find_symbol "infix +"
let mul_symbol = find_symbol "infix *"
let eq_symbol = find_symbol "infix ="
let lt_symbol = find_symbol "infix <"
let leq_symbol = find_symbol "infix <="
let gt_symbol = find_symbol "infix >"
let geq_symbol = find_symbol "infix >="

let plus a b = T.t_app plus_symbol [ a; b ] (Some Ty.ty_int)
let mul a b = T.t_app mul_symbol [ a; b ] (Some Ty.ty_int)
let eq a b = T.ps_app eq_symbol [ a; b ]
let neq a b = T.t_not (eq a b)
let lt a b = T.ps_app lt_symbol [ a; b ]
let leq a b = T.ps_app leq_symbol [ a; b ]
let gt a b = T.ps_app gt_symbol [ a; b ]
let geq a b = T.ps_app geq_symbol [ a; b ]

let int_task =
  let task = Task.add_ty_decl None Ty.ts_int in
  let task = Task.use_export task int_theory in
  List.fold_left
    (fun task s -> Task.add_param_decl task s)
    task
    [ plus_symbol; mul_symbol; lt_symbol ]
