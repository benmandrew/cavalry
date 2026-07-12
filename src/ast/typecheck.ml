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
  | Logic.Bool _ | Logic.BoolVar _ -> SS.empty
  | Logic.BGet (a, i) -> SS.add a (arrays_arith i)
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

(* Boolean names mentioned as bare propositions in an assertion. *)
let rec bools_logic (e : Logic.logic_expr) =
  match e with
  | Logic.BoolVar x -> SS.singleton x
  | Logic.Bool _ | Logic.BGet _ | Logic.Eq _ | Logic.Neq _ | Logic.Lt _
  | Logic.Leq _ | Logic.Gt _ | Logic.Geq _ ->
      SS.empty
  | Logic.Not e | Logic.Forall (_, e) | Logic.Exists (_, e) -> bools_logic e
  | Logic.And (a, b) | Logic.Or (a, b) | Logic.Impl (a, b) ->
      SS.union (bools_logic a) (bools_logic b)

(* Boolean arrays mentioned via an element proposition ([a[i]]) in an assertion. *)
let rec bool_arrays_logic (e : Logic.logic_expr) =
  match e with
  | Logic.BGet (a, _) -> SS.singleton a
  | Logic.BoolVar _ | Logic.Bool _ | Logic.Eq _ | Logic.Neq _ | Logic.Lt _
  | Logic.Leq _ | Logic.Gt _ | Logic.Geq _ ->
      SS.empty
  | Logic.Not e | Logic.Forall (_, e) | Logic.Exists (_, e) ->
      bool_arrays_logic e
  | Logic.And (a, b) | Logic.Or (a, b) | Logic.Impl (a, b) ->
      SS.union (bool_arrays_logic a) (bool_arrays_logic b)

(* Copy edges [(v, x)] from each assignment [x := v] of a bare variable: if [v]
   is boolean then so is [x]. Saturating these (see [check]) infers the type of
   a variable copied from a boolean, which [classify_bool] alone leaves
   ambiguous. *)
let rec copy_edges (e : Program.ut_expr) : (string * string) list =
  let open Program in
  match e with
  | UAssgn (x, UVar v) | ULet (x, UVar v) -> [ (v, x) ]
  | UAssgn (_, rhs) | ULet (_, rhs) -> copy_edges rhs
  | USeq (a, b) -> copy_edges a @ copy_edges b
  | ULoc (_, e) | UPrint e -> copy_edges e
  | UIf (_, a, b) -> copy_edges a @ copy_edges b
  | UWhile (_, _, _, body) -> copy_edges body
  | _ -> []

let program_copy_edges (program : Triple.ut_t list) =
  List.concat_map (fun (t : Triple.ut_t) -> copy_edges t.u) program

(* Propagate boolean-ness along copy edges to a fixpoint. *)
let rec saturate_bools bools edges =
  let bools' =
    List.fold_left
      (fun acc (v, x) -> if SS.mem v acc then SS.add x acc else acc)
      bools edges
  in
  if SS.equal bools' bools then bools else saturate_bools bools' edges

let program_bools (program : Triple.ut_t list) =
  List.fold_left
    (fun s (t : Triple.ut_t) ->
      SS.union s
        (SS.union (classify_bool t.u)
           (SS.union (bools_logic t.p) (bools_logic t.q))))
    SS.empty program

(* An array is boolean-element when a boolean context forces its elements: an
   element assigned a syntactically-boolean expression ([a[i] := b < c]), or an
   element [a[i]] used as a bare boolean (a guard or an operand of &&/||/!).
   Mirrors [classify_bool] for scalars. *)
let rec classify_bool_array (e : Program.ut_expr) =
  let open Program in
  let ( ++ ) = SS.union in
  (* An element access occupying a position that demands a boolean. *)
  let barr = function UGet (a, _) -> SS.singleton a | _ -> SS.empty in
  match e with
  | UArrAssgn (a, i, e) ->
      let here = if is_syntactically_bool e then SS.singleton a else SS.empty in
      here ++ classify_bool_array i ++ classify_bool_array e
  | UAnd (a, b) | UOr (a, b) ->
      barr a ++ barr b ++ classify_bool_array a ++ classify_bool_array b
  | UNot a -> barr a ++ classify_bool_array a
  | UIf (c, a, b) ->
      barr c ++ classify_bool_array c ++ classify_bool_array a
      ++ classify_bool_array b
  | UWhile (_inv, _variant, c, body) ->
      barr c ++ classify_bool_array c ++ classify_bool_array body
  | USeq (a, b) -> classify_bool_array a ++ classify_bool_array b
  | ULoc (_, e) | UPrint e | UAssgn (_, e) | ULet (_, e) ->
      classify_bool_array e
  | UArrMake (_, n) -> classify_bool_array n
  | UProc (_, args) ->
      List.fold_left (fun s a -> s ++ classify_bool_array a) SS.empty args
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
      classify_bool_array a ++ classify_bool_array b
  | UGet (_, i) -> classify_bool_array i
  | UInt _ | UBool _ | UVar _ | ULen _ -> SS.empty

let program_bool_arrays (program : Triple.ut_t list) =
  List.fold_left
    (fun s (t : Triple.ut_t) ->
      SS.union s
        (SS.union (classify_bool_array t.u)
           (SS.union (bool_arrays_logic t.p) (bool_arrays_logic t.q))))
    SS.empty program

(* ===== Expression typing ===== *)

type env = {
  arrays : SS.t;
  bools : SS.t;
  bool_arrays : SS.t;
  procs : (string * (Ty.t list * Ty.t option)) list;
      (* per procedure: its parameter types and its optional result type *)
}

(* The element type of an array: boolean if inferred so, otherwise integer. *)
let element_type env a = if SS.mem a env.bool_arrays then Ty.Bool else Ty.Int

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
      element_type env a
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

(* ===== Assertion typing =====
   The logic language carries no source locations, so its diagnostics name the
   offending variable rather than a line. [bound] holds quantifier-bound names,
   which shadow program variables and are integer-valued (like [Vars.create_fresh]). *)

let rec synth_arith env bound (e : Logic.arith_expr) : Ty.t =
  match e with
  | Logic.Int _ -> Ty.Int
  | Logic.Var x ->
      if SS.mem x bound then Ty.Int
      else if SS.mem x env.arrays then
        fail None "array '%s' cannot be used as a scalar in an assertion" x
      else if SS.mem x env.bools then Ty.Bool
      else Ty.Int
  | Logic.Plus (a, b)
  | Logic.Sub (a, b)
  | Logic.Mul (a, b)
  | Logic.Div (a, b)
  | Logic.Mod (a, b) ->
      expect_arith env bound Ty.Int a;
      expect_arith env bound Ty.Int b;
      Ty.Int
  | Logic.Get (a, i) ->
      require_array env None a;
      expect_arith env bound Ty.Int i;
      element_type env a
  | Logic.Len a ->
      require_array env None a;
      Ty.Int

and expect_arith env bound expected e =
  let actual = synth_arith env bound e in
  if not (Ty.equal actual expected) then
    fail None "expected %s but got %s in an assertion" (Ty.to_string expected)
      (Ty.to_string actual)

let rec check_logic env bound (e : Logic.logic_expr) : unit =
  match e with
  | Logic.Bool _ -> ()
  | Logic.BoolVar x ->
      if SS.mem x bound then
        fail None "quantified variable '%s' is not a boolean proposition" x
      else if SS.mem x env.arrays then
        fail None "array '%s' cannot be used as a proposition" x
      else if not (SS.mem x env.bools) then
        fail None "'%s' is not a boolean but is used as a proposition" x
  | Logic.BGet (a, i) ->
      require_array env None a;
      if not (SS.mem a env.bool_arrays) then
        fail None
          "array '%s' is not boolean, so its element cannot be a proposition" a;
      expect_arith env bound Ty.Int i
  | Logic.Not e -> check_logic env bound e
  | Logic.And (a, b) | Logic.Or (a, b) | Logic.Impl (a, b) ->
      check_logic env bound a;
      check_logic env bound b
  (* Equality is homogeneous over integers or booleans; the other comparisons
     are integer-only. *)
  | Logic.Eq (a, b) | Logic.Neq (a, b) ->
      expect_arith env bound (synth_arith env bound a) b
  | Logic.Lt (a, b) | Logic.Leq (a, b) | Logic.Gt (a, b) | Logic.Geq (a, b) ->
      expect_arith env bound Ty.Int a;
      expect_arith env bound Ty.Int b
  | Logic.Forall (x, e) | Logic.Exists (x, e) ->
      check_logic env (SS.add x bound) e

let check_measure env = function
  | Some m -> expect_arith env SS.empty Ty.Int m
  | None -> ()

(* ===== Command typing ===== *)

(* Check a call's arguments against [f]'s signature and return its result type
   (if any). Shared by a bare call statement (result discarded) and [x := f(..)]
   (result bound). *)
let check_call env loc f args =
  match List.assoc_opt f env.procs with
  | None -> fail loc "call to undeclared procedure '%s'" f
  | Some (tys, result) ->
      let arity = List.length tys in
      let n = List.length args in
      if n <> arity then
        fail loc "procedure '%s' expects %d argument(s) but got %d" f arity n;
      (* Each actual must match its formal's type. *)
      List.iter2 (fun ty arg -> expect env loc ty arg) tys args;
      result

let rec check_cmd env loc (e : Program.ut_expr) : unit =
  let open Program in
  match e with
  | ULoc (loc, e) -> check_cmd env (Some loc) e
  | USeq (a, b) ->
      check_cmd env loc a;
      check_cmd env loc b
  (* [x := f(args)]: the call must return a value, and its result type must
     match the target variable's type. *)
  | UAssgn (x, UProc (f, args)) -> (
      if SS.mem x env.arrays then
        fail loc "array '%s' cannot be assigned a scalar result" x;
      let xt = scalar_type env x in
      match check_call env loc f args with
      | None -> fail loc "procedure '%s' does not return a value" f
      | Some ty ->
          if not (Ty.equal ty xt) then
            fail loc "cannot assign %s result of '%s' to %s variable '%s'"
              (Ty.to_string ty) f (Ty.to_string xt) x)
  | UAssgn (x, e) | ULet (x, e) ->
      if SS.mem x env.arrays then
        fail loc
          "array '%s' cannot be assigned as a scalar (use '%s[i] := ...' or \
           '%s := array(n)')"
          x x x;
      (* A boolean variable takes a boolean right-hand side, an integer one an
         integer -- assigning across types is a type error. *)
      expect env loc (scalar_type env x) e
  (* A bare call as a statement: any result is discarded. *)
  | UProc (f, args) -> ignore (check_call env loc f args : Ty.t option)
  | UIf (c, a, b) ->
      expect env loc Ty.Bool c;
      check_cmd env loc a;
      check_cmd env loc b
  | UWhile (inv, variant, c, body) ->
      check_logic env SS.empty inv;
      check_measure env variant;
      expect env loc Ty.Bool c;
      check_cmd env loc body
  | UPrint e -> expect env loc Ty.Int e
  | UArrMake (a, n) ->
      require_array env loc a;
      expect env loc Ty.Int n
  | UArrAssgn (a, i, e) ->
      require_array env loc a;
      expect env loc Ty.Int i;
      (* The value matches the array's element type. *)
      expect env loc (element_type env a) e
  (* A bare expression as a statement (its value is discarded, e.g. a program's
     final result). It must be a well-typed integer expression -- the same
     restriction [Program.expr_to_cmd] elaborates against. *)
  | UInt _ | UBool _ | UVar _ | UGet _ | ULen _ | UPlus _ | USub _ | UMul _
  | UDiv _ | UMod _ | UEq _ | UNeq _ | ULt _ | ULeq _ | UGt _ | UGeq _ | UAnd _
  | UOr _ | UNot _ ->
      expect env loc Ty.Int e

(* Validate optional parameter annotations against the inferred classification.
   Integer and boolean parameters are supported; arrays (always globals) are
   not. An [int] annotation must not contradict a boolean use. *)
let check_params ~arrays ~bools (ps : (string * Ty.t option) list) =
  List.iter
    (fun (name, ann) ->
      if SS.mem name arrays then
        fail None "array parameter '%s' is not supported" name;
      match ann with
      | None | Some Ty.Bool -> ()
      | Some Ty.Int ->
          if SS.mem name bools then
            fail None "parameter '%s' is annotated int but used as a boolean"
              name
      | Some (Ty.Array _) ->
          fail None "array parameter '%s' is not supported" name)
    ps

(* Targets of a call-assignment [x := f(args)]: the pairs [(x, f)]. Used to
   classify [x] boolean when [f]'s result is boolean, mirroring how a
   syntactically-boolean right-hand side classifies its target. *)
let rec call_targets (e : Program.ut_expr) : (string * string) list =
  let open Program in
  match e with
  | UAssgn (x, UProc (f, _)) -> [ (x, f) ]
  | USeq (a, b) -> call_targets a @ call_targets b
  | ULoc (_, e) -> call_targets e
  | UIf (_, a, b) -> call_targets a @ call_targets b
  | UWhile (_, _, _, body) -> call_targets body
  | _ -> []

type checked = {
  bool_vars : string list;  (** names of the program's boolean variables *)
  bool_arrays : string list;  (** names of the program's boolean arrays *)
  proc_bool_params : (string * bool list) list;
      (** per procedure, whether each formal parameter is boolean *)
}

let check (program : Triple.ut_t list) : checked =
  let arrays = program_arrays program in
  let bool_arrays = program_bool_arrays program in
  (* Each procedure's declared result type, from its [returns] clause (the last
     triple, [main], is not callable and returns nothing). Result types are
     explicit annotations, so this needs no inference. *)
  let proc_ret f =
    List.find_map
      (fun (t : Triple.ut_t) ->
        if String.equal t.f f then Some (Option.map snd t.result) else None)
      program
    |> Option.join
  in
  (* Result binders declared boolean, and targets of [x := f(..)] where [f]
     returns a boolean: both make their name boolean, like an annotated
     parameter or a syntactically-boolean assignment. *)
  let result_bool_names =
    List.filter_map
      (fun (t : Triple.ut_t) ->
        match t.result with Some (r, Ty.Bool) -> Some r | _ -> None)
      program
    |> SS.of_list
  in
  let call_bool_targets =
    List.concat_map (fun (t : Triple.ut_t) -> call_targets t.u) program
    |> List.filter_map (fun (x, f) ->
        match proc_ret f with Some Ty.Bool -> Some x | _ -> None)
    |> SS.of_list
  in
  (* Boolean names: inferred from bodies and assertions, plus any parameter
     annotated [: bool], plus boolean result binders and call targets. *)
  let annotated_bools =
    List.fold_left
      (fun s (t : Triple.ut_t) ->
        List.fold_left
          (fun s (p, ann) ->
            match ann with Some Ty.Bool -> SS.add p s | _ -> s)
          s t.ps)
      SS.empty program
  in
  (* Base boolean names, then saturated along copy edges so a variable copied
     from a boolean (`y := b`) is itself boolean. *)
  let bools =
    saturate_bools
      (SS.union
         (SS.union (program_bools program) annotated_bools)
         (SS.union result_bool_names call_bool_targets))
      (program_copy_edges program)
  in
  (* A name cannot be both an array and a boolean. *)
  (match SS.choose_opt (SS.inter arrays bools) with
  | Some x -> fail None "variable '%s' is used as both an array and a boolean" x
  | None -> ());
  (* Validate each result binder against its declared type: it must be a scalar,
     and an [int] binder must not be used as a boolean. *)
  List.iter
    (fun (t : Triple.ut_t) ->
      match t.result with
      | None -> ()
      | Some (r, ty) ->
          if SS.mem r arrays then
            fail None "result binder '%s' cannot be an array" r;
          if Ty.equal ty Ty.Int && SS.mem r bools then
            fail None
              "result binder '%s' is annotated int but used as a boolean" r)
    program;
  (* A parameter's type: boolean if inferred or annotated so, else integer.
     (Array parameters are rejected by [check_params].) *)
  let param_ty n = if SS.mem n bools then Ty.Bool else Ty.Int in
  (* Every triple but the last is a named procedure; the last is [main] and is
     not callable. A signature is the parameter types and the result type. *)
  let procs =
    match List.rev program with
    | _main :: rev_procs ->
        List.rev_map
          (fun (t : Triple.ut_t) ->
            ( t.f,
              (List.map (fun (n, _) -> param_ty n) t.ps, Option.map snd t.result)
            ))
          rev_procs
    | [] -> []
  in
  let env = { arrays; bools; bool_arrays; procs } in
  List.iter
    (fun (t : Triple.ut_t) ->
      check_params ~arrays ~bools t.ps;
      check_logic env SS.empty t.p;
      check_logic env SS.empty t.q;
      check_measure env t.variant;
      check_cmd env None t.u)
    program;
  {
    bool_vars = SS.elements bools;
    bool_arrays = SS.elements bool_arrays;
    proc_bool_params =
      List.map (fun (f, (tys, _)) -> (f, List.map (Ty.equal Ty.Bool) tys)) procs;
  }
