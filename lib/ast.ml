open Core

module VarMap = Map.Make(String)

type var_map = int VarMap.t

type ast =
  | Int of int
  | Var of string
  | Let of (string * ast * ast)
  | Plus of (ast * ast)
  | Mul of (ast * ast)

let add_var vars x (v : int) =
  VarMap.add_exn vars ~key:x ~data:v

let rec exec_aux vars t =
  match t with
  | Int a -> a
  | Var x -> VarMap.find_exn vars x
  | Let (x, e, e') ->
      let v = exec_aux vars e in
      let new_vars = add_var vars x v in
      exec_aux new_vars e'
  | Plus (a, b) -> (exec_aux vars a) + (exec_aux vars b)
  | Mul (a, b) -> (exec_aux vars a) * (exec_aux vars b)

let exec t = exec_aux VarMap.empty t
