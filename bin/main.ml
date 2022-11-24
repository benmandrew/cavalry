open Why3
open Cavalry

let () =
  let ast = Main.get_ast "a.cvl" in
  let vars = Hoare.collect_variables ast in
  let x = Term.t_var (Hoare.get_var vars "x") in
  (* let p = Arith.lt x (Term.t_nat_const 2) in *)
  let q = Ast.Arith.lt x (Term.t_nat_const 190) in
  if Main.verify vars ast Term.t_true q then
    Printf.printf "verification successful\n"
  else
    Printf.printf
      "verification unsuccessful: precondition does not imply postcondition\n"
