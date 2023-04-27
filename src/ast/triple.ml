open Sexplib.Std

type ut_t = {
  p : Logic.expr; (* Precondition *)
  q : Logic.expr; (* Postcondition *)
  ws : string list; (* Written global variables *)
  f : string; (* Procedure name *)
  ps : string list; (* Formal parameters *)
  u : Program.ut_expr; (* Procedure body *)
}
[@@deriving sexp_of]

type t = {
  p : Logic.expr; (* Precondition *)
  q : Logic.expr; (* Postcondition *)
  ws : string list; (* Written global variables *)
  f : string; (* Procedure name *)
  ps : string list; (* Formal parameters *)
  c : Program.cmd; (* Procedure body *)
}
[@@deriving sexp_of]

let translate { p; f; ws; ps; u; q } =
  { p; f; ws; ps; c = Program.translate_cmd u; q }
