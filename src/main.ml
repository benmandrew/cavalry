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
    { Triple.p; q; ws; f; ps; c = Program.translate_cmd u }
  in
  List.map f ut_ast |> Var_collection.collect

let verify = Hoare.verify

let exec path =
  get_ast path |> List.map (fun p -> fst p |> Runtime.to_proc_t) |> Runtime.exec

(* Raised when [compile]'s verification gate rejects the program. A compiled
   binary asserts the program meets its spec, so by default we refuse to emit
   one for a program that does not verify. *)
exception Verification_failed of string

(* Compile [path] to a native executable at [output], transpiling to OCaml and
   building it with the external toolchain. With [debug], also dump the
   generated OCaml to stdout. Unless [verify] is [false], run the Hoare-logic
   verifier first and raise [Verification_failed] (emitting nothing) if the
   program does not verify.

   The gate verifies against the same integer model the code is compiled in, so
   the proof matches what the binary computes: unbounded integers for the
   default Zarith backend, and 63-bit machine integers (proving overflow-freedom)
   for [native_int]. That pairing is what makes native-int codegen *sound* --
   the gate rejects any program whose arithmetic could overflow. Skipping the
   gate ([verify = false]) with [native_int] forfeits that guarantee. *)
let compile ?(debug = false) ?(verify = true) ?(native_int = false) ~output path
    =
  let ast = get_ast path in
  (if verify then
     match Hoare.verify ~machine_int:native_int ast with
     | Smt.Prover.Valid -> ()
     | Smt.Prover.Invalid ->
         raise (Verification_failed "precondition does not imply postcondition")
     | Smt.Prover.Failed s ->
         raise (Verification_failed ("internal failure: " ^ s)));
  let ocaml = Compile.emit ~native_int ast in
  if debug then (
    print_string ocaml;
    flush stdout);
  Compile.to_native ~native_int ~output ocaml
