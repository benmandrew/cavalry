open Why3

let int_theory : Theory.theory = Env.read_theory Prover.env ["int"] "Int"

  let plus_symbol : Term.lsymbol =
    Theory.ns_find_ls int_theory.Theory.th_export ["infix +"]

let plus a b = Term.t_app_infer plus_symbol [a; b]