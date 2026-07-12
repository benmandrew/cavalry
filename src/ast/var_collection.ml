module Str_set = Set.Make (String)

(* Split a list [l @ \[m\]] into the tuple [(l, m)] *)
let split_last l =
  let rec aux acc = function
    | [] -> failwith "Can't take the last element of an empty list"
    | [ x ] -> (List.rev acc, x)
    | x :: l -> aux (x :: acc) l
  in
  aux [] l

let rec collect_arith_expr (e : Logic.arith_expr) =
  let open Logic in
  match e with
  | Var s -> Str_set.singleton s
  | Int _ -> Str_set.empty
  | Plus (e, e') | Sub (e, e') | Mul (e, e') | Div (e, e') | Mod (e, e') ->
      Str_set.union (collect_arith_expr e) (collect_arith_expr e')
  | Get (a, e) -> Str_set.add a (collect_arith_expr e)
  | Len a -> Str_set.singleton a

(* Variables of a loop variant's measure (if any), collected like the
   invariant's. *)
let collect_measure = function
  | Some m -> collect_arith_expr m
  | None -> Str_set.empty

let collect_logic e =
  let open Logic in
  let rec collect_logic_expr = function
    | Bool _ -> Str_set.empty
    | Not e -> collect_logic_expr e
    | And (e, e') | Or (e, e') | Impl (e, e') ->
        Str_set.union (collect_logic_expr e) (collect_logic_expr e')
    | Eq (e, e')
    | Neq (e, e')
    | Lt (e, e')
    | Leq (e, e')
    | Gt (e, e')
    | Geq (e, e') ->
        Str_set.union (collect_arith_expr e) (collect_arith_expr e')
    (* [x] is bound by the quantifier, not a program variable, so drop it. *)
    | Forall (x, e) | Exists (x, e) -> Str_set.remove x (collect_logic_expr e)
  in
  collect_logic_expr e

let collect_program c =
  let open Program in
  let collect_value : type a. a value -> Str_set.t = function
    | VarInst str | BoolVar str -> Str_set.singleton str
    | Int _ | Bool _ | Unit () -> Str_set.empty
  in
  let rec collect_expr : type a. a expr -> Str_set.t = function
    | Value v -> collect_value v
    | Plus (e, e')
    | Sub (e, e')
    | Mul (e, e')
    | Div (e, e')
    | Mod (e, e')
    | Eq (e, e')
    | Neq (e, e')
    | Lt (e, e')
    | Leq (e, e')
    | Gt (e, e')
    | Geq (e, e') ->
        Str_set.union (collect_expr e) (collect_expr e')
    | And (e, e') | Or (e, e') | Beq (e, e') | Bneq (e, e') ->
        Str_set.union (collect_expr e) (collect_expr e')
    | Not e -> collect_expr e
    | Get (a, e) -> Str_set.add a (collect_expr e)
    | Len a -> Str_set.singleton a
  in
  let collect_any = function
    | IntE e -> collect_expr e
    | BoolE e -> collect_expr e
  in
  let rec collect_cmd = function
    | Located (_, c) -> collect_cmd c
    | IntExpr e -> collect_expr e
    | Seq (c, c') -> Str_set.union (collect_cmd c) (collect_cmd c')
    | Assgn (x, e) | Let (x, e) -> Str_set.(union (singleton x) (collect_any e))
    | Proc (_f, ps) ->
        List.fold_left
          (fun s e -> collect_expr e |> Str_set.union s)
          Str_set.empty ps
    | If (b, e, e') ->
        Str_set.union (collect_expr b)
          (Str_set.union (collect_cmd e) (collect_cmd e'))
    | While (inv, variant, b, c) ->
        Str_set.union
          (Str_set.union (collect_logic inv) (collect_measure variant))
          (Str_set.union (collect_expr b) (collect_cmd c))
    | Print e -> collect_expr e
    | ArrMake (a, n) -> Str_set.add a (collect_expr n)
    | ArrAssgn (a, i, e) ->
        Str_set.add a (Str_set.union (collect_expr i) (collect_expr e))
  in
  collect_cmd c

(* Names used as arrays: anything indexed ([a\[i\]]), measured ([len(a)]),
   created ([a := array(n)]), or element-assigned ([a\[i\] := e]). A name is
   array-typed iff it appears in one of these positions; every other name is a
   scalar integer. Array names get a [map int int] vsymbol plus a companion
   length variable (see [str_set_to_vars]). *)
let rec arrays_arith (e : Logic.arith_expr) =
  let open Logic in
  match e with
  | Get (a, e) -> Str_set.add a (arrays_arith e)
  | Len a -> Str_set.singleton a
  | Plus (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) | Mod (a, b) ->
      Str_set.union (arrays_arith a) (arrays_arith b)
  | Int _ | Var _ -> Str_set.empty

let arrays_measure = function Some m -> arrays_arith m | None -> Str_set.empty

let arrays_logic e =
  let open Logic in
  let arith = arrays_arith in
  let rec logic = function
    | Bool _ -> Str_set.empty
    | Not e | Forall (_, e) | Exists (_, e) -> logic e
    | And (a, b) | Or (a, b) | Impl (a, b) -> Str_set.union (logic a) (logic b)
    | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b)
      ->
        Str_set.union (arith a) (arith b)
  in
  logic e

let arrays_program c =
  let open Program in
  let rec expr : type a. a expr -> Str_set.t = function
    | Value _ -> Str_set.empty
    | Get (a, e) -> Str_set.add a (expr e)
    | Len a -> Str_set.singleton a
    | Plus (a, b)
    | Sub (a, b)
    | Mul (a, b)
    | Div (a, b)
    | Mod (a, b)
    | Eq (a, b)
    | Neq (a, b)
    | Lt (a, b)
    | Leq (a, b)
    | Gt (a, b)
    | Geq (a, b) ->
        Str_set.union (expr a) (expr b)
    | And (a, b) | Or (a, b) | Beq (a, b) | Bneq (a, b) ->
        Str_set.union (expr a) (expr b)
    | Not a -> expr a
  in
  let any = function IntE e -> expr e | BoolE e -> expr e in
  let rec cmd = function
    | Located (_, c) -> cmd c
    | IntExpr e | Print e -> expr e
    | Assgn (_, e) | Let (_, e) -> any e
    | Seq (a, b) -> Str_set.union (cmd a) (cmd b)
    | If (b, c, c') -> Str_set.union (expr b) (Str_set.union (cmd c) (cmd c'))
    | While (inv, variant, b, c) ->
        Str_set.union
          (Str_set.union (arrays_logic inv) (arrays_measure variant))
          (Str_set.union (expr b) (cmd c))
    | Proc (_, ps) ->
        List.fold_left (fun s e -> Str_set.union s (expr e)) Str_set.empty ps
    | ArrMake (a, n) -> Str_set.add a (expr n)
    | ArrAssgn (a, i, e) -> Str_set.add a (Str_set.union (expr i) (expr e))
  in
  cmd c

(* Names used as booleans: a variable read as a [BoolVar], or the target of an
   assignment whose right-hand side is boolean ([BoolE]). Parallels
   [arrays_program]; assertions are omitted since the logic language has no
   boolean program-variable occurrences. A boolean name is given a [bool]-sorted
   symbol by [str_set_to_vars]. *)
let bools_program c =
  let open Program in
  let value : type a. a value -> Str_set.t = function
    | BoolVar x -> Str_set.singleton x
    | VarInst _ | Int _ | Bool _ | Unit () -> Str_set.empty
  in
  let rec expr : type a. a expr -> Str_set.t = function
    | Value v -> value v
    | Get (_, e) -> expr e
    | Len _ -> Str_set.empty
    | Plus (a, b)
    | Sub (a, b)
    | Mul (a, b)
    | Div (a, b)
    | Mod (a, b)
    | Eq (a, b)
    | Neq (a, b)
    | Lt (a, b)
    | Leq (a, b)
    | Gt (a, b)
    | Geq (a, b) ->
        Str_set.union (expr a) (expr b)
    | And (a, b) | Or (a, b) | Beq (a, b) | Bneq (a, b) ->
        Str_set.union (expr a) (expr b)
    | Not a -> expr a
  in
  let rec cmd = function
    | Located (_, c) -> cmd c
    | IntExpr e | Print e -> expr e
    | Assgn (x, BoolE e) | Let (x, BoolE e) -> Str_set.add x (expr e)
    | Assgn (_, IntE e) | Let (_, IntE e) -> expr e
    | Seq (a, b) -> Str_set.union (cmd a) (cmd b)
    | If (b, c, c') -> Str_set.union (expr b) (Str_set.union (cmd c) (cmd c'))
    | While (_inv, _variant, b, c) -> Str_set.union (expr b) (cmd c)
    | Proc (_, ps) ->
        List.fold_left (fun s e -> Str_set.union s (expr e)) Str_set.empty ps
    | ArrMake (_, n) -> expr n
    | ArrAssgn (_, i, e) -> Str_set.union (expr i) (expr e)
  in
  cmd c

(* Every variable a procedure declares in [writes] is a global it mutates, so it
   must be classified as a global even if it never appears in [main]. Otherwise
   it would be misfiled as a procedure-local and a caller's havoc (which resolves
   [writes] against the globals) would fail to find it. *)
let all_writes (program : Triple.t list) =
  List.fold_left
    (fun s (t : Triple.t) -> List.fold_left (fun s w -> Str_set.add w s) s t.ws)
    Str_set.empty program

let all_arrays (program : Triple.t list) =
  List.fold_left
    (fun s (t : Triple.t) ->
      Str_set.union s
        (Str_set.union (arrays_measure t.variant)
           (Str_set.union (arrays_logic t.p)
              (Str_set.union (arrays_logic t.q) (arrays_program t.c)))))
    Str_set.empty program

let all_bools (program : Triple.t list) =
  List.fold_left
    (fun s (t : Triple.t) -> Str_set.union s (bools_program t.c))
    Str_set.empty program

let collect_procedure globals (t : Triple.t) =
  let p_vars = collect_logic t.p in
  let q_vars = collect_logic t.q in
  let c_vars = collect_program t.c in
  let v_vars = collect_measure t.variant in
  let vars = Str_set.(union v_vars (union p_vars (union q_vars c_vars))) in
  (* If global variables occur in the procedure, don't add them as local variables *)
  Str_set.fold (fun global vars -> Str_set.remove global vars) globals vars

let str_set_to_vars ~arrays ~bools vars =
  let f x vs =
    if Str_set.mem x arrays then
      (* An array contributes two symbols: its [map int int] element store and a
         companion integer length under [Vars.len_key]. *)
      Vars.add x
        (Vars.create_fresh_array x)
        (Vars.add (Vars.len_key x) (Vars.create_fresh (Vars.len_key x)) vs)
    else if Str_set.mem x bools then Vars.add x (Vars.create_fresh_bool x) vs
    else Vars.add x (Vars.create_fresh x) vs
  in
  Str_set.fold f vars Vars.empty

let collect (program : Triple.t list) =
  let procedures, main = split_last program in
  let arrays = all_arrays program in
  let bools = all_bools program in
  let globals =
    Str_set.union (collect_procedure Str_set.empty main) (all_writes program)
  in
  let procedures =
    List.map
      (fun proc ->
        let vars =
          str_set_to_vars ~arrays ~bools (collect_procedure globals proc)
        in
        (proc, vars))
      procedures
  in
  procedures @ [ (main, str_set_to_vars ~arrays ~bools globals) ]
