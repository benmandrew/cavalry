module Str_set = Set.Make (String)

(* Split a list [l @ \[m\]] into the tuple [(l, m)] *)
let split_last l =
  let rec aux acc = function
    | [] -> failwith "Can't take the last element of an empty list"
    | [ x ] -> (List.rev acc, x)
    | x :: l -> aux (x :: acc) l
  in
  aux [] l

let collect_logic e =
  let open Logic in
  let rec collect_arith_expr = function
    | Var s -> Str_set.singleton s
    | Int _ -> Str_set.empty
    | Plus (e, e') | Sub (e, e') | Mul (e, e') ->
        Str_set.union (collect_arith_expr e) (collect_arith_expr e')
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
    | Assgn (x, e) | Let (x, e) ->
        Str_set.(union (singleton x) (collect_expr e))
    | Proc (_f, ps) ->
        List.fold_left
          (fun s e -> collect_expr e |> Str_set.union s)
          Str_set.empty ps
    | If (b, e, e') ->
        Str_set.union (collect_expr b)
          (Str_set.union (collect_cmd e) (collect_cmd e'))
    | While (_, b, c) -> Str_set.union (collect_expr b) (collect_cmd c)
    | Print e -> collect_expr e
  in
  collect_cmd c

let collect_procedure globals (t : Triple.t) =
  let p_vars = collect_logic t.p in
  let q_vars = collect_logic t.q in
  let c_vars = collect_program t.c in
  let vars = Str_set.(union p_vars (union q_vars c_vars)) in
  (* If global variables occur in the procedure, don't add them as local variables *)
  Str_set.fold (fun global vars -> Str_set.remove global vars) globals vars

let str_set_to_vars vars =
  let f x vs =
    let symbol = Vars.create_fresh x in
    Vars.add x symbol vs
  in
  Str_set.fold f vars Vars.empty

let collect (program : Triple.t list) =
  let procedures, main = split_last program in
  let globals = collect_procedure Str_set.empty main in
  let procedures =
    List.map
      (fun proc ->
        let vars = str_set_to_vars (collect_procedure globals proc) in
        (proc, vars))
      procedures
  in
  procedures @ [ (main, str_set_to_vars globals) ]
