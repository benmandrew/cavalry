open Core

type _ value =
  | Unit : unit -> unit value
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value
  | BoolVar : string -> bool value
[@@deriving sexp_of]

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Neq : int expr * int expr -> bool expr
  | Lt : int expr * int expr -> bool expr
  | Leq : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | Geq : int expr * int expr -> bool expr
  | Beq : bool expr * bool expr -> bool expr (* boolean equality [b = c] *)
  | Bneq : bool expr * bool expr -> bool expr (* boolean inequality [b != c] *)
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr
  | Mod : int expr * int expr -> int expr
  | Get : string * int expr -> int expr (* array element a[i] *)
  | Len : string -> int expr (* array length len(a) *)
[@@deriving sexp_of]

(* An expression of statically-unknown type: the right-hand side of an
   assignment, which may be an integer or a boolean now that variables can be
   either. The tag recovers the type the GADT erased so consumers (interpreter,
   compiler, WLP) can dispatch on it. *)
type anyexpr = IntE of int expr | BoolE of bool expr [@@deriving sexp_of]

type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | Assgn of string * anyexpr
  | Let of string * anyexpr
  | Proc of string * anyexpr list
  | If of bool expr * cmd * cmd
  | While of Logic.expr * Logic.arith_expr option * bool expr * cmd
    (* invariant, optional variant (decreasing measure), guard, body *)
  | Print of int expr
  | ArrMake of string * int expr (* a := array(n) *)
  | ArrAssgn of string * int expr * int expr (* a[i] := e *)
  | Located of Loc.t * cmd
    (* a command annotated with its source location, used to point error
       messages at the construct whose obligation failed *)
[@@deriving sexp_of]

type ut_expr =
  | UInt of int
  | UBool of bool
  | UVar of string
  | USeq of ut_expr * ut_expr
  | UAssgn of string * ut_expr
  | ULet of string * ut_expr
  | UProc of string * ut_expr list
  | UIf of ut_expr * ut_expr * ut_expr
  | UWhile of Logic.expr * Logic.arith_expr option * ut_expr * ut_expr
  | UPrint of ut_expr
  | UEq of ut_expr * ut_expr
  | UNeq of ut_expr * ut_expr
  | ULt of ut_expr * ut_expr
  | ULeq of ut_expr * ut_expr
  | UGt of ut_expr * ut_expr
  | UGeq of ut_expr * ut_expr
  | UAnd of ut_expr * ut_expr
  | UOr of ut_expr * ut_expr
  | UNot of ut_expr
  | UPlus of ut_expr * ut_expr
  | USub of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr
  | UDiv of ut_expr * ut_expr
  | UMod of ut_expr * ut_expr
  | UGet of string * ut_expr
  | ULen of string
  | UArrMake of string * ut_expr
  | UArrAssgn of string * ut_expr * ut_expr
  | ULoc of Loc.t * ut_expr (* a command tagged with its source location *)
[@@deriving sexp_of, show]

exception TypeError of string

(* Whether an expression is boolean-typed, used to send [=]/[!=] to the boolean
   or the integer comparison. A bare variable is boolean iff [is_bool] says so;
   the syntactic boolean forms are boolean regardless. [Typecheck] has validated
   that both operands agree, so testing the left one suffices. *)
let rec is_bool_operand ~is_bool = function
  | UBool _ | UEq _ | UNeq _ | ULt _ | ULeq _ | UGt _ | UGeq _ | UAnd _ | UOr _
  | UNot _ ->
      true
  | UVar x -> is_bool x
  | ULoc (_, e) -> is_bool_operand ~is_bool e
  | _ -> false

let rec t_int_expr = function
  | UInt v -> Value (Int v)
  | UVar v -> Value (VarInst v)
  | UPlus (a, b) -> Plus (t_int_expr a, t_int_expr b)
  | USub (a, b) -> Sub (t_int_expr a, t_int_expr b)
  | UMul (a, b) -> Mul (t_int_expr a, t_int_expr b)
  | UDiv (a, b) -> Div (t_int_expr a, t_int_expr b)
  | UMod (a, b) -> Mod (t_int_expr a, t_int_expr b)
  | UGet (a, i) -> Get (a, t_int_expr i)
  | ULen a -> Len a
  | e -> raise (TypeError (show_ut_expr e))

and t_bool_expr ~is_bool e =
  let recur = t_bool_expr ~is_bool in
  match e with
  | UBool v -> Value (Bool v)
  (* A bare variable in boolean position is a boolean variable; [Typecheck] has
     already established that [v] is boolean-typed. *)
  | UVar v -> Value (BoolVar v)
  | UEq (a, b) ->
      if is_bool_operand ~is_bool a then Beq (recur a, recur b)
      else Eq (t_int_expr a, t_int_expr b)
  | UNeq (a, b) ->
      if is_bool_operand ~is_bool a then Bneq (recur a, recur b)
      else Neq (t_int_expr a, t_int_expr b)
  | ULt (a, b) -> Lt (t_int_expr a, t_int_expr b)
  | ULeq (a, b) -> Leq (t_int_expr a, t_int_expr b)
  | UGt (a, b) -> Gt (t_int_expr a, t_int_expr b)
  | UGeq (a, b) -> Geq (t_int_expr a, t_int_expr b)
  | UAnd (a, b) -> And (recur a, recur b)
  | UOr (a, b) -> Or (recur a, recur b)
  | UNot a -> Not (recur a)
  | e -> raise (TypeError (show_ut_expr e))

(* A bare expression used as a statement: its value is discarded. Any integer
   expression is valid here ([t_int_expr] raises [TypeError] on a boolean or a
   nested command); [Typecheck] has already ruled those out by this point. *)
let expr_to_cmd e = IntExpr (t_int_expr e)

(* Elaborate an assignment right-hand side at its target's type: a boolean-typed
   variable ([is_bool x]) takes a boolean expression, any other an integer one.
   [Typecheck] guarantees the source expression matches. *)
let t_rhs ~is_bool x e =
  if is_bool x then BoolE (t_bool_expr ~is_bool e) else IntE (t_int_expr e)

let rec t_cmd ~is_bool ~proc_bool_params c =
  let recur = t_cmd ~is_bool ~proc_bool_params in
  match c with
  | ULoc (loc, e) -> Located (loc, recur e)
  | USeq (c, c') -> Seq (recur c, recur c')
  | UAssgn (s, e) -> Assgn (s, t_rhs ~is_bool s e)
  | ULet (s, e) -> Let (s, t_rhs ~is_bool s e)
  | UProc (f, ps) ->
      (* Each actual is elaborated at its formal's type: a boolean parameter
         takes a boolean argument. [Typecheck] has matched the arity. *)
      let arg is_b e =
        if is_b then BoolE (t_bool_expr ~is_bool e) else IntE (t_int_expr e)
      in
      Proc (f, List.map2_exn (proc_bool_params f) ps ~f:arg)
  | UIf (e, c, c') -> If (t_bool_expr ~is_bool e, recur c, recur c')
  | UWhile (inv, variant, e, c) ->
      While (inv, variant, t_bool_expr ~is_bool e, recur c)
  | UPrint e -> Print (t_int_expr e)
  | UArrMake (a, n) -> ArrMake (a, t_int_expr n)
  | UArrAssgn (a, i, e) -> ArrAssgn (a, t_int_expr i, t_int_expr e)
  | v -> expr_to_cmd v

let translate_cmd ~is_bool ~proc_bool_params = t_cmd ~is_bool ~proc_bool_params
