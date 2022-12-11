module Ast = Ast
module Parser = Parse.Parser
module Lexer = Parse.Lexer

let get_ast path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ut_ast = Parser.main Lexer.main lexbuf in
  In_channel.close file;
  match ut_ast with
  | { p; u; q } ->
      let c = Ast.Program.translate_cmd u in
      Ast.Triple.{ p; c; q }

let verify = Hoare.verify

let exec path =
  let ast = get_ast path in
  Ast.Runtime.exec ast.c
