open Cavalry

let () =
  let ast = Main.get_ast "a.cvl" in
  let vars = Ast.Var_collection.collect ast in
  let result =
    if Main.verify vars ast then "verification successful"
    else "verification unsuccessful: precondition does not imply postcondition"
  in
  Printf.printf "%s\n" result
