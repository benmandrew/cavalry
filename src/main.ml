module Ast = Ast
module Parser = Parse.Parser
module Lexer = Parse.Lexer
open Ast

let get_ast path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ut_ast = Parser.top Lexer.main lexbuf in
  In_channel.close file;
  let f { Triple.p; q; ws; f; ps; u } =
    let proc = { Triple.p; q; ws; f; ps; c = Program.translate_cmd u } in
    (proc, Var_collection.collect proc)
  in
  List.map f ut_ast

let verify = Hoare.verify

let exec path =
  get_ast path |> List.map (fun p -> fst p |> Runtime.to_proc_t) |> Runtime.exec
