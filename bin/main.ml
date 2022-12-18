open Cavalry

let () =
  Printf.printf "%d\n" (Main.exec "a.cvl")
  (* let ast = Main.get_ast "a.cvl" in
  let vars = Ast.Var_collection.collect ast in

  let open Smt.Prover in
  match Main.verify vars ast with
  | Valid -> Printf.printf "verification successful\n"
  | Invalid ->
      Printf.printf
        "verification unsuccessful: precondition does not imply postcondition\n"
  | Failed s -> Printf.printf "verification failure: %s\n" s *)
