module Str_set = Set.Make (String)

let collect_logic e =
  let open Logic in
  let collect_arith_expr = function
    | Var str -> Str_set.singleton str
    | Int _ | Plus _ | Sub _ | Mul _ -> Str_set.empty
  in
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
  in
  collect_logic_expr e

let collect_program c =
  let open Program in
  let collect_value : type a. a value -> Str_set.t = function
    | VarInst str -> Str_set.singleton str
    | Int _ | Bool _ | Unit () -> Str_set.empty
  in
  let rec collect_expr : type a. a expr -> Str_set.t = function
    | Value v -> collect_value v
    | Plus (e, e')
    | Sub (e, e')
    | Mul (e, e')
    | Eq (e, e')
    | Neq (e, e')
    | Lt (e, e')
    | Leq (e, e')
    | Gt (e, e')
    | Geq (e, e') ->
        Str_set.union (collect_expr e) (collect_expr e')
  in
  let rec collect_cmd = function
    | IntExpr e -> collect_expr e
    | Seq (c, c') -> Str_set.union (collect_cmd c) (collect_cmd c')
    | EAssgn (x, e) -> Str_set.union (Str_set.singleton x) (collect_expr e)
    | PAssgn (x, _f, ps) ->
        let params =
          List.fold_left
            (fun s e -> collect_expr e |> Str_set.union s)
            Str_set.empty ps
        in
        Str_set.union (Str_set.singleton x) params
    | If (b, e, e') ->
        Str_set.union (collect_expr b)
          (Str_set.union (collect_cmd e) (collect_cmd e'))
    | While (_, b, c) -> Str_set.union (collect_expr b) (collect_cmd c)
    | Print e -> collect_expr e
  in
  collect_cmd c

let collect { Triple.p; c; q; _ } =
  let p_vars = collect_logic p in
  let q_vars = collect_logic q in
  let c_vars = collect_program c in
  let vars = Str_set.union p_vars (Str_set.union q_vars c_vars) in
  let f vs x =
    let symbol = Vars.create_fresh x in
    Vars.add x symbol vs
  in
  List.fold_left f Vars.empty (Str_set.elements vars)
