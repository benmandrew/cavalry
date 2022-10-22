open Why3
open Cavalry

let _ =
  let ast = Main.get_ast "a.cvl" in
  let vars = Hoare.collect_variables ast in
  let q =
    Term.ps_app Arith.lt_symbol
      [ Term.t_var (Hoare.get_var vars "x"); Term.t_nat_const 7 ]
  in
  Main.verify vars ast q
