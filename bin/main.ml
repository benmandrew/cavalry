open Why3
open Cavalry

let () =
  let ast = Main.get_ast "a.cvl" in
  let vars = Hoare.collect_variables ast in
  let x = Term.t_var (Hoare.get_var vars "x") in
  (* let p = Arith.lt x (Term.t_nat_const 2) in *)
  let q = Arith.lt x (Term.t_nat_const 190) in
  Main.verify vars ast Term.t_true q
