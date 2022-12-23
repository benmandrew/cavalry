module Ast = Ast
module Parser = Parse.Parser
module Lexer = Parse.Lexer

let get_ast path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ut_ast = Parser.top Lexer.main lexbuf in
  In_channel.close file;
  let f Ast.Triple.{ p; u; q } =
    Ast.Triple.{ p; c = Ast.Program.translate_cmd u; q }
  in
  List.map f ut_ast

let verify = Hoare.verify

let exec path =
  let ast = get_ast path in
  Ast.Runtime.exec (List.map (fun Ast.Triple.{ c; _ } -> c) ast)
