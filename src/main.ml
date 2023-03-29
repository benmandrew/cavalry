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
  let f { Triple.p; f; ps; u; q } =
    let proc = { Triple.p; f; ps; c = Program.translate_cmd u; q } in
    (proc, Var_collection.collect proc)
  in
  List.map f ut_ast

let verify = Hoare.verify

let exec path =
  get_ast path |> List.map (fun p -> fst p |> Runtime.to_proc_t) |> Runtime.exec
