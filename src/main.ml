let get_ast path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ut_ast = Parser.main Lexer.main lexbuf in
  In_channel.close file;
  Ast.translate_cmd ut_ast

let verify vars c q =
  match Hoare.verify vars c q with
  | None -> Printf.printf "\nVerification did not succeed\n"
  | Some _ -> Printf.printf "\nVerification succeeded\n"

let exec path =
  let ast = get_ast path in
  Ast.exec ast
