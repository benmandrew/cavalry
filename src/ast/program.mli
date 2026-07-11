(** The executable program AST, in both of its forms: the untyped {!ut_expr} the
    parser produces, and the GADT-typed {!value}/{!expr}/{!cmd} that
    {!translate_cmd} type-checks it into. The typed AST is the shared input to
    both consumers -- {!Runtime}'s interpreter and [Hoare]'s WLP calculus.

    Program {e assertions} live separately in {!Logic}; this module is the code
    that runs. *)

(** Leaf values, GADT-indexed by their OCaml type. [VarInst] is a variable
    occurrence (always integer-valued, hence [int value]); it names a binding
    resolved later against the {!Vars} environment. *)
type _ value =
  | Unit : unit -> unit value
  | Int : int -> int value
  | Bool : bool -> bool value
  | VarInst : string -> int value
[@@deriving sexp_of]

(** Pure expressions, indexed by result type so the type system rules out
    ill-typed combinations (e.g. adding a comparison): comparisons yield
    [bool expr], arithmetic and the array projections yield [int expr]. *)
type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Neq : int expr * int expr -> bool expr
  | Lt : int expr * int expr -> bool expr
  | Leq : int expr * int expr -> bool expr
  | Gt : int expr * int expr -> bool expr
  | Geq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr
  | Mod : int expr * int expr -> int expr
  | Get : string * int expr -> int expr
  | Len : string -> int expr
[@@deriving sexp_of]

(** Commands (statements). [Let] introduces a local binding, [Assgn] mutates an
    existing variable, [Proc] is a call by name. [While] carries its loop
    invariant and optional variant (decreasing measure) alongside guard and
    body. [ArrMake] is [a <- array(n)] and [ArrAssgn] is [a[i] <- e]. *)
type cmd =
  | IntExpr of int expr
  | Seq of cmd * cmd
  | Assgn of string * int expr
  | Let of string * int expr
  | Proc of string * int expr list
  | If of bool expr * cmd * cmd
  | While of Logic.expr * Logic.arith_expr option * bool expr * cmd
  | Print of int expr
  | ArrMake of string * int expr
  | ArrAssgn of string * int expr * int expr
  | Located of Loc.t * cmd
[@@deriving sexp_of]

(* Untyped AST to play nice with the Menhir parser generator *)

(** The untyped AST the parser emits, with expressions and commands in one flat
    variant. {!translate_cmd} resolves it into the typed {!cmd}. *)
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
  | UPlus of ut_expr * ut_expr
  | USub of ut_expr * ut_expr
  | UMul of ut_expr * ut_expr
  | UDiv of ut_expr * ut_expr
  | UMod of ut_expr * ut_expr
  | UGet of string * ut_expr
  | ULen of string
  | UArrMake of string * ut_expr
  | UArrAssgn of string * ut_expr * ut_expr
  | ULoc of Loc.t * ut_expr
[@@deriving sexp_of, show]

exception TypeError of string
(** Raised by {!translate_cmd} on an untyped tree that does not type-check --
    e.g. a boolean where an integer is required, or an expression used where a
    command is expected. Carries the offending node, rendered by [show]. *)

val translate_cmd : ut_expr -> cmd
(** Type-check and translate the untyped parser output into the typed {!cmd}
    AST.

    @raise TypeError on malformed input. *)
