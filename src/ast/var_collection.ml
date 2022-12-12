module StrSet = Set.Make (String)

let collect_logic e =
  let open Logic in
  let collect_arith_expr = function
    | Var str -> StrSet.singleton str
    | Int _ | Plus _ | Mul _ -> StrSet.empty
  in
  let rec collect_logic_expr = function
    | Bool _ -> StrSet.empty
    | Not e -> collect_logic_expr e
    | And (e, e') | Or (e, e') | Impl (e, e') ->
        StrSet.union (collect_logic_expr e) (collect_logic_expr e')
    | Eq (e, e')
    | Neq (e, e')
    | Lt (e, e')
    | Leq (e, e')
    | Gt (e, e')
    | Geq (e, e') ->
        StrSet.union (collect_arith_expr e) (collect_arith_expr e')
  in
  collect_logic_expr e

let collect_program c =
  let open Program in
  let collect_value : type a. a value -> StrSet.t = function
    | VarInst str -> StrSet.singleton str
    | Int _ | Bool _ | Unit () -> StrSet.empty
  in
  let rec collect_expr : type a. a expr -> StrSet.t = function
    | Value v -> collect_value v
    | Eq (e, e') | Plus (e, e') | Mul (e, e') ->
        StrSet.union (collect_expr e) (collect_expr e')
  in
  let rec collect_cmd = function
    | IntExpr e -> collect_expr e
    | Seq (c, c') -> StrSet.union (collect_cmd c) (collect_cmd c')
    | Assgn (x, e) -> StrSet.union (StrSet.singleton x) (collect_expr e)
    | If (b, e, e') ->
        StrSet.union (collect_expr b)
          (StrSet.union (collect_cmd e) (collect_cmd e'))
    | While (_, b, c) -> StrSet.union (collect_expr b) (collect_cmd c)
  in
  collect_cmd c

let collect Triple.{ p; c; q } =
  let p_vars = collect_logic p in
  let q_vars = collect_logic q in
  let c_vars = collect_program c in
  let vars = StrSet.union p_vars (StrSet.union q_vars c_vars) in
  let f vs x =
    let symbol = Vars.create_fresh x in
    Vars.add x symbol vs
  in
  List.fold_left f Vars.empty (StrSet.elements vars)
