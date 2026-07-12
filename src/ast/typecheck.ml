(* An explicit well-typedness pass over the untyped AST, run as a gate before
   [Triple.translate] elaborates it (see [Main.get_ast]).

   Historically Cavalry had no type environment: [Program.translate_cmd] decided
   a node's type purely from its shape ([VarInst] is hardwired to [int]), and a
   name was treated as an array iff it structurally appeared in an array position
   ([Var_collection]'s [arrays_*] scanners). That leaves gaps -- a name used as
   both a scalar and an array slips through to a Why3 sort clash rather than a
   clean diagnostic, and there is nowhere to hang a type annotation. This pass
   makes the discipline explicit: it classifies every name, checks each
   expression against an expected type, and reports a located {!Type_error}.

   It is deliberately behaviour-preserving: every program that verified before
   still type-checks, because the only inhabited variable types today are [int]
   and [int] arrays. First-class boolean variables (a later milestone) will build
   on this pass; for now a boolean-typed *variable* is rejected as unsupported,
   even though boolean *expressions* (comparisons, [&&]/[||]/[!]) are fine. *)

exception Type_error of Loc.t option * string
(** Raised on an ill-typed program: the source location of the offending
    construct (when known) and a human-readable message. *)

module SS = Set.Make (String)

let fail loc fmt =
  Printf.ksprintf (fun msg -> raise (Type_error (loc, msg))) fmt

(* ===== Array-name collection (untyped) =====
   A name is array-typed iff it appears in an array position anywhere in the
   program: indexed [a[i]], measured [len(a)], created [a := array(n)], or
   element-assigned [a[i] := e]. Assertions count too, so [len(a)] in a
   postcondition classifies [a] as an array. This mirrors
   [Var_collection]'s classification, kept in sync so the checker and the
   downstream variable environment agree. *)

let rec arrays_arith (e : Logic.arith_expr) =
  match e with
  | Logic.Get (a, e) -> SS.add a (arrays_arith e)
  | Logic.Len a -> SS.singleton a
  | Logic.Plus (a, b)
  | Logic.Sub (a, b)
  | Logic.Mul (a, b)
  | Logic.Div (a, b)
  | Logic.Mod (a, b) ->
      SS.union (arrays_arith a) (arrays_arith b)
  | Logic.Int _ | Logic.Var _ -> SS.empty

let rec arrays_logic (e : Logic.logic_expr) =
  match e with
  | Logic.Bool _ -> SS.empty
  | Logic.Not e | Logic.Forall (_, e) | Logic.Exists (_, e) -> arrays_logic e
  | Logic.And (a, b) | Logic.Or (a, b) | Logic.Impl (a, b) ->
      SS.union (arrays_logic a) (arrays_logic b)
  | Logic.Eq (a, b)
  | Logic.Neq (a, b)
  | Logic.Lt (a, b)
  | Logic.Leq (a, b)
  | Logic.Gt (a, b)
  | Logic.Geq (a, b) ->
      SS.union (arrays_arith a) (arrays_arith b)

let arrays_measure = function Some m -> arrays_arith m | None -> SS.empty

let rec arrays_body (e : Program.ut_expr) =
  let open Program in
  let ( ++ ) = SS.union in
  match e with
  | UGet (a, i) -> SS.add a (arrays_body i)
  | ULen a -> SS.singleton a
  | UArrMake (a, n) -> SS.add a (arrays_body n)
  | UArrAssgn (a, i, e) -> SS.add a (arrays_body i ++ arrays_body e)
  | UInt _ | UBool _ | UVar _ -> SS.empty
  | ULoc (_, e) | UPrint e | UNot e | UAssgn (_, e) | ULet (_, e) ->
      arrays_body e
  | USeq (a, b)
  | UEq (a, b)
  | UNeq (a, b)
  | ULt (a, b)
  | ULeq (a, b)
  | UGt (a, b)
  | UGeq (a, b)
  | UAnd (a, b)
  | UOr (a, b)
  | UPlus (a, b)
  | USub (a, b)
  | UMul (a, b)
  | UDiv (a, b)
  | UMod (a, b) ->
      arrays_body a ++ arrays_body b
  | UIf (c, a, b) -> arrays_body c ++ arrays_body a ++ arrays_body b
  | UWhile (inv, variant, c, body) ->
      arrays_logic inv ++ arrays_measure variant ++ arrays_body c
      ++ arrays_body body
  | UProc (_, args) ->
      List.fold_left (fun s e -> s ++ arrays_body e) SS.empty args

let program_arrays (program : Triple.ut_t list) =
  List.fold_left
    (fun s (t : Triple.ut_t) ->
      SS.union s
        (SS.union (arrays_body t.u)
           (SS.union (arrays_logic t.p)
              (SS.union (arrays_logic t.q) (arrays_measure t.variant)))))
    SS.empty program

(* ===== Boolean-variable inference =====
   A variable is boolean when a boolean context forces it, with no annotation
   required: it is assigned a syntactically-boolean expression (a comparison, a
   connective, or a literal), or it appears as a bare variable in a boolean
   position -- a loop/if guard, or an operand of &&/||/!. A bare variable merely
   copied from another (`y := b`) is *not* inferred boolean; that ambiguous case
   is left to a future annotation. Consistency -- that such a name is not also
   used as an integer or array -- is checked afterwards by the main pass. *)

let is_syntactically_bool (e : Program.ut_expr) =
  match e with
  | UBool _ | UEq _ | UNeq _ | ULt _ | ULeq _ | UGt _ | UGeq _ | UAnd _ | UOr _
  | UNot _ ->
      true
  | _ -> false

let rec classify_bool (e : Program.ut_expr) =
  let open Program in
  let ( ++ ) = SS.union in
  (* A bare variable occupying a position that demands a boolean. *)
  let bvar = function UVar x -> SS.singleton x | _ -> SS.empty in
  match e with
  | UAssgn (x, rhs) | ULet (x, rhs) ->
      let here =
        if is_syntactically_bool rhs then SS.singleton x else SS.empty
      in
      here ++ classify_bool rhs
  | UAnd (a, b) | UOr (a, b) ->
      bvar a ++ bvar b ++ classify_bool a ++ classify_bool b
  | UNot a -> bvar a ++ classify_bool a
  | UIf (c, a, b) ->
      bvar c ++ classify_bool c ++ classify_bool a ++ classify_bool b
  | UWhile (_inv, _variant, c, body) ->
      bvar c ++ classify_bool c ++ classify_bool body
  | USeq (a, b) -> classify_bool a ++ classify_bool b
  | ULoc (_, e) | UPrint e -> classify_bool e
  | UArrMake (_, n) -> classify_bool n
  | UArrAssgn (_, i, e) -> classify_bool i ++ classify_bool e
  | UProc (_, args) ->
      List.fold_left (fun s a -> s ++ classify_bool a) SS.empty args
  | UEq (a, b)
  | UNeq (a, b)
  | ULt (a, b)
  | ULeq (a, b)
  | UGt (a, b)
  | UGeq (a, b)
  | UPlus (a, b)
  | USub (a, b)
  | UMul (a, b)
  | UDiv (a, b)
  | UMod (a, b) ->
      classify_bool a ++ classify_bool b
  | UGet (_, i) -> classify_bool i
  | UInt _ | UBool _ | UVar _ | ULen _ -> SS.empty

let program_bools (program : Triple.ut_t list) =
  List.fold_left
    (fun s (t : Triple.ut_t) -> SS.union s (classify_bool t.u))
    SS.empty program

(* ===== Expression typing ===== *)

type env = { arrays : SS.t; bools : SS.t; procs : (string * int) list }

let require_array env loc a =
  if not (SS.mem a env.arrays) then
    fail loc "'%s' is used as an array but is not one" a

(* The type a bare scalar variable has: boolean if inferred so, otherwise
   integer. (Array names never reach here -- they are rejected as scalars.) *)
let scalar_type env x = if SS.mem x env.bools then Ty.Bool else Ty.Int

let rec synth env loc (e : Program.ut_expr) : Ty.t =
  let open Program in
  match e with
  | UInt _ -> Ty.Int
  | UBool _ -> Ty.Bool
  | UVar x ->
      if SS.mem x env.arrays then
        fail loc "array '%s' cannot be used as a scalar value" x
      else scalar_type env x
  | UGet (a, i) ->
      require_array env loc a;
      expect env loc Ty.Int i;
      Ty.Int
  | ULen a ->
      require_array env loc a;
      Ty.Int
  | UPlus (a, b) | USub (a, b) | UMul (a, b) | UDiv (a, b) | UMod (a, b) ->
      expect env loc Ty.Int a;
      expect env loc Ty.Int b;
      Ty.Int
  | ULt (a, b) | ULeq (a, b) | UGt (a, b) | UGeq (a, b) ->
      expect env loc Ty.Int a;
      expect env loc Ty.Int b;
      Ty.Bool
  (* Equality is homogeneous: both operands must have the same type (integer or
     boolean), and the result is boolean. *)
  | UEq (a, b) | UNeq (a, b) ->
      expect env loc (synth env loc a) b;
      Ty.Bool
  | UAnd (a, b) | UOr (a, b) ->
      expect env loc Ty.Bool a;
      expect env loc Ty.Bool b;
      Ty.Bool
  | UNot a ->
      expect env loc Ty.Bool a;
      Ty.Bool
  | ULoc (loc, e) -> synth env (Some loc) e
  | USeq _ | UAssgn _ | ULet _ | UProc _ | UIf _ | UWhile _ | UPrint _
  | UArrMake _ | UArrAssgn _ ->
      fail loc "statement used where an expression is expected"

and expect env loc (expected : Ty.t) (e : Program.ut_expr) =
  let actual = synth env loc e in
  if not (Ty.equal actual expected) then
    fail loc "expected %s but got %s" (Ty.to_string expected)
      (Ty.to_string actual)

(* ===== Command typing ===== *)

let check_call env loc f args =
  match List.assoc_opt f env.procs with
  | None -> fail loc "call to undeclared procedure '%s'" f
  | Some arity ->
      let n = List.length args in
      if n <> arity then
        fail loc "procedure '%s' expects %d argument(s) but got %d" f arity n;
      (* Parameters are integer scalars, so every actual must be an [int]. *)
      List.iter (expect env loc Ty.Int) args

let rec check_cmd env loc (e : Program.ut_expr) : unit =
  let open Program in
  match e with
  | ULoc (loc, e) -> check_cmd env (Some loc) e
  | USeq (a, b) ->
      check_cmd env loc a;
      check_cmd env loc b
  | UAssgn (x, e) | ULet (x, e) ->
      if SS.mem x env.arrays then
        fail loc
          "array '%s' cannot be assigned as a scalar (use '%s[i] := ...' or \
           '%s := array(n)')"
          x x x;
      (* A boolean variable takes a boolean right-hand side, an integer one an
         integer -- assigning across types is a type error. *)
      expect env loc (scalar_type env x) e
  | UProc (f, args) -> check_call env loc f args
  | UIf (c, a, b) ->
      expect env loc Ty.Bool c;
      check_cmd env loc a;
      check_cmd env loc b
  | UWhile (_inv, _variant, c, body) ->
      (* The invariant/variant are logic assertions, typed on the [Logic] side;
         here we only constrain the loop guard to be boolean and check the
         body. *)
      expect env loc Ty.Bool c;
      check_cmd env loc body
  | UPrint e -> expect env loc Ty.Int e
  | UArrMake (a, n) ->
      require_array env loc a;
      expect env loc Ty.Int n
  | UArrAssgn (a, i, e) ->
      require_array env loc a;
      expect env loc Ty.Int i;
      expect env loc Ty.Int e
  (* A bare expression as a statement (its value is discarded, e.g. a program's
     final result). It must be a well-typed integer expression -- the same
     restriction [Program.expr_to_cmd] elaborates against. *)
  | UInt _ | UBool _ | UVar _ | UGet _ | ULen _ | UPlus _ | USub _ | UMul _
  | UDiv _ | UMod _ | UEq _ | UNeq _ | ULt _ | ULeq _ | UGt _ | UGeq _ | UAnd _
  | UOr _ | UNot _ ->
      expect env loc Ty.Int e

(* Validate optional parameter annotations. Parameters are integer scalars
   today, so an [int] annotation is accepted (and redundant) while [bool] or an
   array type is rejected as not-yet-supported rather than silently ignored. *)
let check_params (ps : (string * Ty.t option) list) =
  List.iter
    (function
      | _, None | _, Some Ty.Int -> ()
      | name, Some t ->
          fail None
            "parameter '%s' has type %s, but only integer parameters are \
             supported"
            name (Ty.to_string t))
    ps

let check (program : Triple.ut_t list) : string list =
  let arrays = program_arrays program in
  let bools = program_bools program in
  (* A name cannot be both an array and a boolean. *)
  (match SS.choose_opt (SS.inter arrays bools) with
  | Some x -> fail None "variable '%s' is used as both an array and a boolean" x
  | None -> ());
  (* Boolean procedure parameters are not yet supported; reject an inferred one
     (an annotated one is caught by [check_params]). *)
  let params =
    List.fold_left
      (fun s (t : Triple.ut_t) ->
        List.fold_left (fun s (p, _) -> SS.add p s) s t.ps)
      SS.empty program
  in
  (match SS.choose_opt (SS.inter params bools) with
  | Some x ->
      fail None
        "parameter '%s' is boolean, but only integer parameters are supported" x
  | None -> ());
  (* Every triple but the last is a named procedure; the last is [main] and is
     not callable. Signatures are (name, arity). *)
  let procs =
    match List.rev program with
    | _main :: rev_procs ->
        List.rev_map
          (fun (t : Triple.ut_t) -> (t.f, List.length t.ps))
          rev_procs
    | [] -> []
  in
  let env = { arrays; bools; procs } in
  List.iter
    (fun (t : Triple.ut_t) ->
      check_params t.ps;
      check_cmd env None t.u)
    program;
  SS.elements bools
