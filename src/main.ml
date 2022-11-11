let get_ast path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ut_ast = Parser.main Lexer.main lexbuf in
  In_channel.close file;
  Ast.translate_cmd ut_ast

let verify vars c p q =
  (* Format.printf "generated wlp: %a\n" Why3.Pretty.print_term
     (Hoare.wlp vars c q); *)
  if Hoare.verify vars c p q then Printf.printf "verification successful\n"
  else
    Printf.printf
      "verification unsuccessful: precondition does not imply postcondition\n"

let exec path =
  let ast = get_ast path in
  Ast.exec ast
