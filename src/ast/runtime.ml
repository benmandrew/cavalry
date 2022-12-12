open Core
open Program
module RuntimeVarMap = Map.Make (String)

type var_map = int RuntimeVarMap.t

let add_var vars x v = RuntimeVarMap.add_exn vars ~key:x ~data:v

exception UnboundVarError of string

let exec_value (vars : var_map) (type a) (v : a value) : a =
  match v with
  | Unit () -> ()
  | Int i -> i
  | Bool b -> b
  | VarInst x -> (
      match RuntimeVarMap.find vars x with
      | None -> raise (UnboundVarError x)
      | Some v -> v)

let rec exec_expr : type a. var_map -> a expr -> a * var_map =
 fun vars v ->
  match v with
  | Value v -> (exec_value vars v, vars)
  | Plus (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 + v2, vars'')
  | Mul (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 * v2, vars'')
  | Eq (a, b) ->
      let v1, vars' = exec_expr vars a in
      let v2, vars'' = exec_expr vars' b in
      (v1 = v2, vars'')

let rec exec_cmd vars = function
  | Seq (c, c') ->
      let _, vars' = exec_cmd vars c in
      exec_cmd vars' c'
  | Assgn (x, e) ->
      let v, vars' = exec_expr vars e in
      let vars'' = add_var vars' x v in
      (v, vars'')
  | If (e, c, c') ->
      let b, vars' = exec_expr vars e in
      if b then exec_cmd vars' c else exec_cmd vars' c'
  | While (_, e, c) as loop ->
      let b, vars' = exec_expr vars e in
      if b then
        let _, vars'' = exec_cmd vars' c in
        exec_cmd vars'' loop
      else (0, vars')
  | IntExpr v -> exec_expr vars v

let exec t = fst @@ exec_cmd RuntimeVarMap.empty t
