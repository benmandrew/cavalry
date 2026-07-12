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
    (* Arrays are always globals (created by [a := array(n)], written by
       [a\[i\] := e]). The name->array *map* is persistent like the scalar
       stores, but the array a name points at is mutated *in place*: the
       environment is threaded linearly, so no live environment ever aliases an
       array a newer one has written, and an element store need not copy it.
       (Scalars stay in a persistent map because programs bind only a handful of
       them -- a 2-3 node tree beats hashing a string key on every access.) *)
    global_arrays : int array BoundVars.t;
    local_vars : int BoundVars.t;
    procs : BoundProcs.t;
    proc_map : (string, proc_t) Hashtbl.t;
  }

  let add_global_var ({ global_vars; _ } as t) x v =
    { t with global_vars = BoundVars.add x v global_vars }

  let add_global_array ({ global_arrays; _ } as t) x a =
    { t with global_arrays = BoundVars.add x a global_arrays }

  let find_array { global_arrays; _ } x =
    match BoundVars.find_opt x global_arrays with
    | None -> raise (UnboundError x)
    | Some a -> a

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
      global_arrays = BoundVars.empty;
      local_vars = BoundVars.empty;
      procs = BoundProcs.empty;
      proc_map = Hashtbl.create 32;
    }
end

exception Out_of_fuel
exception Overflow

(* Overflow-checked native arithmetic, enabled only on the fuel-bounded path
   ([exec_env]) used by the fuzzer. Cavalry integers are Why3's *unbounded*
   [int]; the interpreter uses native 63-bit ints, so a concrete run that
   overflows would silently diverge from the symbolic semantics and manufacture
   spurious counterexamples. When [check_overflow] is set we detect the
   divergence and abort the run (treated as a discard) instead. [cav run]
   leaves the flag unset and keeps plain wrapping arithmetic. *)
let check_overflow = ref false

let add_ovf a b =
  let s = a + b in
  if !check_overflow && a lxor b >= 0 && a lxor s < 0 then raise Overflow;
  s

let sub_ovf a b =
  let s = a - b in
  if !check_overflow && a lxor b < 0 && a lxor s < 0 then raise Overflow;
  s

let mul_ovf a b =
  let p = a * b in
  if !check_overflow && a <> 0 && (p / a <> b || (a = -1 && b = min_int)) then
    raise Overflow;
  p

let exec_value r (type a) (v : a value) : a =
  match v with
  | Unit () -> ()
  | Int i -> i
  | Bool b -> b
  | VarInst x -> Runtime.find_var r x
  (* Boolean variables share the integer store, encoded 0/1 (see the [Assgn]
     case); a read decodes back to a boolean. *)
  | BoolVar x -> Runtime.find_var r x <> 0

let rec exec_expr : type a. Runtime.t -> a expr -> a =
 fun r v ->
  (* Both operands are evaluated with explicit [let]s rather than a shared
     [(exec_expr a, exec_expr b)] tuple: without flambda that tuple is a real
     per-operation allocation, and this is the interpreter's hottest path.
     Cavalry expressions are side-effect-free, so the left-to-right order is
     unobservable. *)
  match v with
  | Value v -> exec_value r v
  | Plus (a, b) ->
      let v1 = exec_expr r a in
      add_ovf v1 (exec_expr r b)
  | Sub (a, b) ->
      let v1 = exec_expr r a in
      sub_ovf v1 (exec_expr r b)
  | Mul (a, b) ->
      let v1 = exec_expr r a in
      mul_ovf v1 (exec_expr r b)
  (* Truncated division/remainder (native [/], [mod]); [b = 0] raises
     [Division_by_zero]. [verify] discharges a [divisor <> 0] obligation, so a
     verified program never reaches that. *)
  | Div (a, b) ->
      let v1 = exec_expr r a in
      v1 / exec_expr r b
  | Mod (a, b) ->
      let v1 = exec_expr r a in
      v1 mod exec_expr r b
  | Get (a, i) -> (Runtime.find_array r a).(exec_expr r i)
  (* Boolean arrays share the integer array store, encoded 0/1 like scalars. *)
  | BGet (a, i) -> (Runtime.find_array r a).(exec_expr r i) <> 0
  | Len a -> Array.length (Runtime.find_array r a)
  | Eq (a, b) ->
      let v1 = exec_expr r a in
      v1 = exec_expr r b
  | Neq (a, b) ->
      let v1 = exec_expr r a in
      v1 != exec_expr r b
  | Lt (a, b) ->
      let v1 = exec_expr r a in
      v1 < exec_expr r b
  | Leq (a, b) ->
      let v1 = exec_expr r a in
      v1 <= exec_expr r b
  | Gt (a, b) ->
      let v1 = exec_expr r a in
      v1 > exec_expr r b
  | Geq (a, b) ->
      let v1 = exec_expr r a in
      v1 >= exec_expr r b
  (* Short-circuiting, matching OCaml's [&&]/[||] and the compiled backend. *)
  | And (a, b) -> exec_expr r a && exec_expr r b
  | Or (a, b) -> exec_expr r a || exec_expr r b
  | Not a -> not (exec_expr r a)
  | Beq (a, b) ->
      let v1 = exec_expr r a in
      v1 = exec_expr r b
  | Bneq (a, b) ->
      let v1 = exec_expr r a in
      v1 <> exec_expr r b

and exec_cmd ?(fuel = ref max_int) r c : int * Runtime.t =
  let exec_cmd r c = exec_cmd ~fuel r c in
  match c with
  | Located (_, c) -> exec_cmd r c
  | Seq (c, c') ->
      let _, r' = exec_cmd r c in
      exec_cmd r' c'
  (* A boolean right-hand side is stored 0/1 in the same integer store; the
     statement's own (discarded) value is 0. *)
  | Assgn (x, IntE e) ->
      let v = exec_expr r e in
      (v, Runtime.add_global_var r x v)
  | Assgn (x, BoolE e) ->
      let v = if exec_expr r e then 1 else 0 in
      (0, Runtime.add_global_var r x v)
  | Let (x, IntE e) ->
      let v = exec_expr r e in
      (v, Runtime.add_local_var r x v)
  | Let (x, BoolE e) ->
      let v = if exec_expr r e then 1 else 0 in
      (0, Runtime.add_local_var r x v)
  | Proc (f, ps) ->
      (* Boolean arguments are passed 0/1, like boolean variables. *)
      let eval = function
        | IntE e -> exec_expr r e
        | BoolE e -> if exec_expr r e then 1 else 0
      in
      let ps = List.map eval ps in
      let v, r_proc =
        match Runtime.find_proc r f with
        | { ps = fps; c; _ } ->
            let r_fun = List.fold_left2 Runtime.add_local_var r fps ps in
            exec_cmd r_fun c
      in
      (* Keep the callee's global scalar and array writes, but drop its local
         context by restoring the caller's [local_vars]. (Arrays are mutated in
         place, so [global_arrays] here differs from the caller's only when the
         callee created a new array with [array(n)].) *)
      ( v,
        {
          r with
          global_vars = r_proc.global_vars;
          global_arrays = r_proc.global_arrays;
        } )
  | If (e, c, c') ->
      let b = exec_expr r e in
      if b then exec_cmd r c else exec_cmd r c'
  | While (_, _, e, c) as loop ->
      let b = exec_expr r e in
      if b then (
        if !fuel <= 0 then raise Out_of_fuel;
        decr fuel;
        let _, r' = exec_cmd r c in
        exec_cmd r' loop)
      else (0, r)
  | Print e ->
      Printf.printf "%d\n" (exec_expr r e);
      (0, r)
  | ArrMake (a, n) ->
      let arr = Array.make (exec_expr r n) 0 in
      (0, Runtime.add_global_array r a arr)
  | ArrAssgn (a, i, e) ->
      let idx = exec_expr r i in
      (* A boolean value is stored 0/1, like a boolean scalar. *)
      let v =
        match e with
        | IntE e -> exec_expr r e
        | BoolE e -> if exec_expr r e then 1 else 0
      in
      (* Mutate in place: [exec_cmd] threads the environment linearly (every
         caller discards its input once it holds the successor), so no live
         environment aliases this array. Same array object, same map -- O(1). *)
      let arr = Runtime.find_array r a in
      arr.(idx) <- v;
      (v, r)
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

(* Outcome of a fuel-bounded run. [Terminated] exposes the whole final global
   environment (not just the return value) so a differential harness can read
   every variable to build a postcondition. [OutOfFuel] covers both the fuel
   cap and detected overflow -- both mean "this run is not a usable witness",
   so callers discard it. [Raised] is any other runtime failure, e.g. reading
   an unbound variable. *)
(* The environment a run ends in, keyed by variable name. Alias of the
   interpreter's internal variable map, surfaced at the top level so callers
   need not spell out the (confusingly self-named) nested [Runtime] module. *)
module Env = Runtime.BoundVars

type exec_result = Terminated of int Env.t | OutOfFuel | Raised

(* Like [exec], but bounds loop iterations by [fuel] and returns the final
   environment rather than the entrypoint's value. Used by the soundness
   fuzzer, where non-termination must be discarded (WLP is only a *liberal*
   precondition) rather than treated as a counterexample. *)
let exec_env ?(fuel = 10000) (ast : proc_t list) : exec_result =
  let main, procs =
    let rev = List.rev ast in
    (List.hd rev, List.tl rev |> List.rev)
  in
  let r =
    List.fold_left (fun r proc -> Runtime.add_proc r proc) Runtime.empty procs
  in
  let fuel = ref fuel in
  let entrypoint = main.c in
  check_overflow := true;
  Fun.protect
    ~finally:(fun () -> check_overflow := false)
    (fun () ->
      try
        let _, r' = exec_cmd ~fuel r entrypoint in
        Terminated r'.global_vars
      with
      | Out_of_fuel | Overflow -> OutOfFuel
      | Runtime.UnboundError _ -> Raised)
