open Why3
open Cavalry

(* Term.t_const (BigInt.of_int 7) *)

let _ =
  let ast = Main.get_ast "a.cvl" in
  let vars = Hoare.collect_variables ast in
  let x = Term.t_var (Hoare.get_var vars "x") in
  let q =
    Arith.lt x (Term.t_nat_const 7)
  in
  Main.verify vars ast q
