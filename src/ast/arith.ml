open Why3
module T = Term

let int_theory : Theory.theory = Env.read_theory Smt.Prover.env [ "int" ] "Int"

let div_theory : Theory.theory =
  Env.read_theory Smt.Prover.env [ "int" ] "EuclideanDivision"

let find_symbol theory s = Theory.ns_find_ls theory.Theory.th_export [ s ]
let plus_symbol = find_symbol int_theory "infix +"
let sub_symbol = find_symbol int_theory "infix -"
let mul_symbol = find_symbol int_theory "infix *"
let div_symbol = find_symbol div_theory "div"
let eq_symbol = find_symbol int_theory "infix ="
let lt_symbol = find_symbol int_theory "infix <"
let leq_symbol = find_symbol int_theory "infix <="
let gt_symbol = find_symbol int_theory "infix >"
let geq_symbol = find_symbol int_theory "infix >="
let plus a b = T.t_app plus_symbol [ a; b ] (Some Ty.ty_int)
let sub a b = T.t_app sub_symbol [ a; b ] (Some Ty.ty_int)
let mul a b = T.t_app mul_symbol [ a; b ] (Some Ty.ty_int)
let div a b = T.t_app div_symbol [ a; b ] (Some Ty.ty_int)
let eq a b = T.ps_app eq_symbol [ a; b ]
let neq a b = T.t_not (eq a b)
let lt a b = T.ps_app lt_symbol [ a; b ]
let leq a b = T.ps_app leq_symbol [ a; b ]
let gt a b = T.ps_app gt_symbol [ a; b ]
let geq a b = T.ps_app geq_symbol [ a; b ]

let base_task =
  let task = Task.add_ty_decl None Ty.ts_int in
  List.fold_left
    (fun task theory -> Task.use_export task theory)
    task [ int_theory; div_theory ]
