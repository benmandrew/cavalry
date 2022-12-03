open Cavalry

let () =
  let ast = Main.get_ast "a.cvl" in
  let vars = Hoare.collect_variables ast.c in
  let result =
    if Main.verify vars ast then "verification successful"
    else "verification unsuccessful: precondition does not imply postcondition"
  in
  Printf.printf "%s\n" result
