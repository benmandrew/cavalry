open Cavalry

let calc () =
  let open Stdio in
  let file = In_channel.create "test.cvl" in
  let lexbuf = Lexing.from_channel file in
  let ast = Parser.main Lexer.main lexbuf in
  In_channel.close file;
  let result = Ast.exec ast in
  print_int result;
  print_newline ();
  flush stdout

let _ = calc ()

