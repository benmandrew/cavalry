open Sexplib.Std

type ut_t = {
  p : Logic.expr; (* Precondition *)
  q : Logic.expr; (* Postcondition *)
  variant : Logic.arith_expr option; (* Optional termination measure *)
  ws : string list; (* Written global variables *)
  f : string; (* Procedure name *)
  ps : string list; (* Formal parameters *)
  u : Program.ut_expr; (* Procedure body *)
}
[@@deriving sexp_of]

type t = {
  p : Logic.expr; (* Precondition *)
  q : Logic.expr; (* Postcondition *)
  variant : Logic.arith_expr option; (* Optional termination measure *)
  ws : string list; (* Written global variables *)
  f : string; (* Procedure name *)
  ps : string list; (* Formal parameters *)
  c : Program.cmd; (* Procedure body *)
}
[@@deriving sexp_of]

let translate { p; f; variant; ws; ps; u; q } =
  { p; f; variant; ws; ps; c = Program.translate_cmd u; q }
