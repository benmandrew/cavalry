(* open Core *)
open Program

module Runtime = struct
  module BoundVars = Map.Make (String)
  module BoundFuncs = Set.Make (String)

  exception UnboundError of string

  type t = {
    vars : int BoundVars.t;
    funcs : BoundFuncs.t;
    func_map : (string, cmd) Hashtbl.t;
  }

  let add_var { vars; funcs; func_map } x v =
    { vars = BoundVars.add x v vars; funcs; func_map }

  let find_var { vars; _ } x =
    match BoundVars.find_opt x vars with
    | None -> raise (UnboundError x)
    | Some v -> v

  let add_func { vars; funcs; func_map } f c =
    Hashtbl.add func_map f c;
    { vars; funcs = BoundFuncs.add f funcs; func_map }

  let find_func { funcs; func_map; _ } f =
    match BoundFuncs.find_opt f funcs with
    | None -> raise (UnboundError f)
    | Some _ -> Hashtbl.find func_map f

  let empty =
    {
      vars = BoundVars.empty;
      funcs = BoundFuncs.empty;
      func_map = Hashtbl.create 32;
    }
end

let exec_value r (type a) (v : a value) : a =
  match v with
  | Unit () -> ()
  | Int i -> i
  | Bool b -> b
  | VarInst x -> Runtime.find_var r x

let rec exec_expr : type a. Runtime.t -> a expr -> a * Runtime.t =
 fun r v ->
  let binary_app r a b =
    let v1, r' = exec_expr r a in
    let v2, r'' = exec_expr r' b in
    (v1, v2, r'')
  in
  match v with
  | Value v -> (exec_value r v, r)
  | Plus (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 + v2, r')
  | Sub (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 - v2, r')
  | Mul (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 * v2, r')
  | Eq (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 = v2, r')
  | Neq (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 != v2, r')
  | Lt (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 < v2, r')
  | Leq (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 <= v2, r')
  | Gt (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 > v2, r')
  | Geq (a, b) ->
      let v1, v2, r' = binary_app r a b in
      (v1 >= v2, r')
  | App (f, ps) -> (
      let ps, r' =
        List.fold_left
          (fun (ps, r) p ->
            let p', r' = exec_expr r p in
            (p' :: ps, r'))
          ([], r) ps
      in
      match Runtime.find_func r' f with
      | Func (_, fps, c) ->
          let r_fun = List.fold_left2 Runtime.add_var r' fps ps in
          exec_cmd r_fun c
      | _ -> raise (Program.TypeError "Tried to apply a non-function"))

and exec_cmd r c : int * Runtime.t =
  match c with
  | Func (f, _, _) as f' -> (0, Runtime.add_func r f f')
  | Seq (c, c') ->
      let _, r' = exec_cmd r c in
      exec_cmd r' c'
  | Assgn (x, e) ->
      let v, r' = exec_expr r e in
      let r'' = Runtime.add_var r' x v in
      (v, r'')
  | If (e, c, c') ->
      let b, r' = exec_expr r e in
      if b then exec_cmd r' c else exec_cmd r' c'
  | While (_, e, c) as loop ->
      let b, r' = exec_expr r e in
      if b then
        let _, r'' = exec_cmd r' c in
        exec_cmd r'' loop
      else (0, r')
  | Print e ->
      Printf.printf "%d\n" (fst @@ exec_expr r e);
      (0, r)
  | IntExpr v -> exec_expr r v

let exec ast =
  let main, functions =
    let rev = List.rev ast in
    (List.hd rev, List.tl rev |> List.rev)
  in
  let r =
    List.fold_left (fun r t -> snd @@ exec_cmd r t) Runtime.empty functions
  in
  fst @@ exec_cmd r main
