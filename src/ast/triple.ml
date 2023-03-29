open Sexplib.Std

type ut_t = {
  p : Logic.expr;
  f : string;
  ps : string list;
  u : Program.ut_expr;
  q : Logic.expr;
}
[@@deriving sexp_of]

type t = {
  p : Logic.expr;
  f : string;
  ps : string list;
  c : Program.cmd;
  q : Logic.expr;
}
[@@deriving sexp_of]

let translate { p; f; ps; u; q } = { p; f; ps; c = Program.translate_cmd u; q }
