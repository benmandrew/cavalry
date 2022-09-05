open Cavalry


let calc () =
  let lexbuf = Lexing.from_channel stdin in
  while true do
    let ast = Parser.main Lexer.main lexbuf in
    let result = Ast.exec ast in
    print_int result;
    print_newline ();
    flush stdout
  done

let _ = calc ()

