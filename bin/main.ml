open Cavalry

let () =
  (* Printf.printf "%d\n" (Main.exec "example.cvl") *)
  let program = Main.get_ast "example.cvl" in
  let open Smt.Prover in
  match Main.verify program with
  | Valid -> Printf.printf "verification successful\n"
  | Invalid ->
      Printf.printf
        "verification unsuccessful: precondition does not imply postcondition\n"
  | Failed s -> Printf.printf "verification failure: %s\n" s
