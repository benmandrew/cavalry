(* open Core *)
open Program

type proc_t = { f : string; ps : string list; c : cmd }

let to_proc_t { Triple.f; ps; c; _ } = { f; ps; c }

module Runtime = struct
  module BoundVars = Map.Make (String)
  module BoundProcs = Set.Make (String)

  exception UnboundError of string

  type t = {
    global_vars : int BoundVars.t;
    local_vars : int BoundVars.t;
    procs : BoundProcs.t;
    proc_map : (string, proc_t) Hashtbl.t;
  }

  let add_global_var ({ global_vars; _ } as t) x v =
    { t with global_vars = BoundVars.add x v global_vars }

  let add_local_var ({ local_vars; _ } as t) x v =
    { t with local_vars = BoundVars.add x v local_vars }

  let find_var { global_vars; local_vars; _ } x =
    match BoundVars.find_opt x local_vars with
    | None -> (
        match BoundVars.find_opt x global_vars with
        | None -> raise (UnboundError x)
        | Some v -> v)
    | Some v -> v

  let add_proc ({ procs; proc_map; _ } as t) ({ f; _ } as proc) =
    Hashtbl.add proc_map f proc;
    { t with procs = BoundProcs.add f procs }

  let find_proc { procs; proc_map; _ } f =
    match BoundProcs.find_opt f procs with
    | None -> raise (UnboundError f)
    | Some _ -> Hashtbl.find proc_map f

  let empty =
    {
      global_vars = BoundVars.empty;
      local_vars = BoundVars.empty;
      procs = BoundProcs.empty;
      proc_map = Hashtbl.create 32;
    }
end

let exec_value r (type a) (v : a value) : a =
  match v with
  | Unit () -> ()
  | Int i -> i
  | Bool b -> b
  | VarInst x -> Runtime.find_var r x

let rec exec_expr : type a. Runtime.t -> a expr -> a =
 fun r v ->
  let binary_app r a b = (exec_expr r a, exec_expr r b) in
  match v with
  | Value v -> exec_value r v
  | Plus (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 + v2
  | Sub (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 - v2
  | Mul (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 * v2
  | Eq (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 = v2
  | Neq (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 != v2
  | Lt (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 < v2
  | Leq (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 <= v2
  | Gt (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 > v2
  | Geq (a, b) ->
      let v1, v2 = binary_app r a b in
      v1 >= v2

and exec_cmd r c : int * Runtime.t =
  match c with
  | Seq (c, c') ->
      let _, r' = exec_cmd r c in
      exec_cmd r' c'
  | Assgn (x, e) ->
      let v = exec_expr r e in
      let r' = Runtime.add_global_var r x v in
      (v, r')
  | Let (x, e) ->
      let v = exec_expr r e in
      let r' = Runtime.add_local_var r x v in
      (v, r')
  | Proc (f, ps) ->
      let ps = List.map (exec_expr r) ps in
      let v, r_proc =
        match Runtime.find_proc r f with
        | { ps = fps; c; _ } ->
            let r_fun = List.fold_left2 Runtime.add_local_var r fps ps in
            exec_cmd r_fun c
      in
      (* Ignore changes to procedure-local context *)
      (v, { r with global_vars = r_proc.global_vars })
  | If (e, c, c') ->
      let b = exec_expr r e in
      if b then exec_cmd r c else exec_cmd r c'
  | While (_, e, c) as loop ->
      let b = exec_expr r e in
      if b then
        let _, r' = exec_cmd r c in
        exec_cmd r' loop
      else (0, r)
  | Print e ->
      Printf.printf "%d\n" (exec_expr r e);
      (0, r)
  | IntExpr v -> (exec_expr r v, r)

let exec (ast : proc_t list) =
  let main, procs =
    let rev = List.rev ast in
    (List.hd rev, List.tl rev |> List.rev)
  in
  let r =
    List.fold_left (fun r proc -> Runtime.add_proc r proc) Runtime.empty procs
  in
  match main with { c = entrypoint; _ } -> fst @@ exec_cmd r entrypoint
