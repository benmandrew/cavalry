open Sexplib.Std

type ut_t = {
  p : Logic.expr; (* Precondition *)
  q : Logic.expr; (* Postcondition *)
  variant : Logic.arith_expr option; (* Optional termination measure *)
  ws : string list; (* Written global variables *)
  f : string; (* Procedure name *)
  ps : (string * Ty.t option) list;
      (* Formal parameters, optionally annotated *)
  result : (string * Ty.t) option;
      (* Result binder and its type, from a [returns { r : ty }] clause; [None]
         for a procedure (or [main]) that returns nothing. *)
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
  result : (string * Ty.t) option; (* Result binder and type, if any *)
  c : Program.cmd; (* Procedure body *)
}
[@@deriving sexp_of]

(* Parameter type annotations are validated by {!Typecheck} and dropped here:
   the typed AST and everything downstream ([Hoare], [Runtime], [Compile]) work
   with plain parameter names. [is_bool] (also from {!Typecheck}) directs the
   elaboration of boolean variables. [result] names the procedure's result
   binder (if any); an assignment to it inside the body elaborates to
   [Program.ResAssgn] rather than a global [Assgn]. *)
let translate ~is_bool ~is_bool_array ~proc_bool_params
    { p; f; variant; ws; ps; u; q; result } =
  {
    p;
    f;
    variant;
    ws;
    ps = List.map fst ps;
    result;
    c =
      Program.translate_cmd ~is_bool ~is_bool_array ~proc_bool_params
        ~result:(Option.map fst result) u;
    q;
  }
