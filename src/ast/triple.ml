type ut_t = { p : Logic.expr; u : Program.ut_expr; q : Logic.expr }
[@@deriving sexp_of]

type t = { p : Logic.expr; c : Program.cmd; q : Logic.expr }
[@@deriving sexp_of]
