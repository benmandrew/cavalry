(* Browser binding: the synchronous half of the client-side verifier.

   Exposes [self.cavalryObligations(src)] to JavaScript. It runs the pure OCaml
   pipeline -- parse -> typecheck -> translate -> variable collection -> WLP ->
   SMT-LIB2 printing -- and returns the result as a JSON string. It solves
   nothing: the caller (the Web Worker) hands each obligation's SMT-LIB2 to
   Z3-wasm and decides the verdict. Keeping this side synchronous is what lets
   the async Z3 solve loop live entirely in JavaScript.

   JSON shape:
     ok:    { "ok": true, "procedures": [ { "name", "obligations":
              [ { "expl", "loc": {"line","col"}|null, "smtlib" } ] } ] }
     error: { "ok": false, "kind": "lex"|"parse"|"type",
              "error": string, "loc": {"line","col"}|null } *)

open Js_of_ocaml
module Loc = Ast.Loc

(* Serve the embedded Why3 data (stdlib theories + prover drivers) as a virtual
   directory at [/why3/], then point the SMT layer at it. [mount] resolves each
   file lazily through [Embedded_why3.read] -- no directories to pre-create --
   so [Env.read_theory] and the driver load find their sources with no real
   filesystem. Runs once, before any obligation is printed. *)
let () =
  Sys_js.mount ~path:"/why3/" (fun ~prefix:_ ~path -> Embedded_why3.read path);
  Smt.Prover.configure_browser ~loadpath:[ "/why3/stdlib" ]
    ~driver_file:"/why3/drivers/z3_487.drv"

let json_of_loc : Loc.t option -> Yojson.Safe.t = function
  | None -> `Null
  | Some { Loc.line; col; _ } ->
      `Assoc [ ("line", `Int line); ("col", `Int col) ]

let err ?(loc = `Null) kind msg : Yojson.Safe.t =
  `Assoc
    [
      ("ok", `Bool false);
      ("kind", `String kind);
      ("error", `String msg);
      ("loc", loc);
    ]

let verify_json (src : string) : string =
  let lexbuf = Lexing.from_string src in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = "<editor>" };
  (* The lexer's current position, as a JSON [{line, col}], for the parse/lex
     errors that carry no location of their own -- it points at the token the
     parser choked on. *)
  let here () : Yojson.Safe.t =
    let p = lexbuf.Lexing.lex_curr_p in
    `Assoc
      [
        ("line", `Int p.Lexing.pos_lnum);
        ("col", `Int (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1));
      ]
  in
  let result : Yojson.Safe.t =
    match
      (* Mirror [Main.get_ast]'s elaboration, but keep the parse inline so a
         lex/parse failure can be located from [lexbuf] (see [here]). Learn which
         variables/arrays/parameters are boolean so [translate] types them. *)
      let ut = Parse.Parser.top Parse.Lexer.main lexbuf in
      let { Ast.Typecheck.bool_vars; bool_arrays; proc_bool_params } =
        Ast.Typecheck.check ut
      in
      let is_bool x = List.mem x bool_vars in
      let is_bool_array x = List.mem x bool_arrays in
      let proc_bool_params f =
        match List.assoc_opt f proc_bool_params with
        | Some bs -> bs
        | None -> []
      in
      List.map
        (Ast.Triple.translate ~is_bool ~is_bool_array ~proc_bool_params)
        ut
      |> Ast.Var_collection.collect |> Cavalry.Main.obligations_smtlib
    with
    | procs ->
        let obligation (expl, loc, smtlib) : Yojson.Safe.t =
          `Assoc
            [
              ("expl", `String expl);
              ("loc", json_of_loc loc);
              ("smtlib", `String smtlib);
            ]
        in
        let procedure (name, obs) : Yojson.Safe.t =
          `Assoc
            [
              ("name", `String name);
              ("obligations", `List (List.map obligation obs));
            ]
        in
        `Assoc
          [
            ("ok", `Bool true); ("procedures", `List (List.map procedure procs));
          ]
    | exception Ast.Typecheck.Type_error (loc, msg) ->
        err ~loc:(json_of_loc loc) "type" msg
    | exception Parse.Lexer.Error (Parse.Lexer.Illegal_character c) ->
        err ~loc:(here ()) "lex" (Printf.sprintf "illegal character %C" c)
    | exception Parse.Parser.Error -> err ~loc:(here ()) "parse" "syntax error"
  in
  Yojson.Safe.to_string result

let () =
  Js.Unsafe.set Js.Unsafe.global "cavalryObligations"
    (Js.wrap_callback (fun src -> Js.string (verify_json (Js.to_string src))))
