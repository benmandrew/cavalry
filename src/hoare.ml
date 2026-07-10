open Ast
open Ast.Program
module T = Why3.Term
module Ty = Why3.Ty

(* Split a list [l @ \[m\]] into the tuple [(l, m)] *)
let split_last l =
  let rec aux acc = function
    | [] -> failwith "Can't take the last element of an empty list"
    | [ x ] -> (List.rev acc, x)
    | x :: l -> aux (x :: acc) l
  in
  aux [] l

let debug = ref false

let debug_print f vars p q p_gen =
  let open Printf in
  if !debug then (
    printf "===== Procedure '%s'\n" f;
    printf "===== p           =====\n";
    Logic.print_term p;
    printf "\n===== q           =====\n";
    Logic.print_term q;
    printf "\n===== p -> wlp(q) =====\n";
    Logic.print_term T.(t_forall_close vars [] (t_implies p p_gen));
    printf "\n\n";
    flush stdout)

let list_of_var_map (vm : Vars.t) = Vars.bindings vm |> List.map snd
let unit_term = T.t_app (T.fs_tuple 0) [] (Some (Ty.ty_app (Ty.ts_tuple 0) []))

let val_to_term : type a. g_vars:Vars.t -> ?l_vars:Vars.t -> a value -> T.term =
 fun ~g_vars ?l_vars v ->
  match v with
  | Unit _ -> unit_term
  | Int v -> T.t_nat_const v
  | Bool b -> if b then T.t_bool_true else T.t_bool_false
  | VarInst x -> (
      match l_vars with
      | Some l_vars -> T.t_var @@ Vars.find_fallback x l_vars g_vars
      | None -> T.t_var @@ Vars.find x g_vars)

let rec expr_to_term : type a.
    g_vars:Vars.t -> ?l_vars:Vars.t -> a expr -> T.term =
 fun ~g_vars ?l_vars e ->
  let f = expr_to_term ~g_vars ?l_vars in
  let open Arith in
  match e with
  | Value v -> val_to_term ~g_vars ?l_vars v
  | Eq (e, e') -> eq (f e) (f e')
  | Neq (e, e') -> neq (f e) (f e')
  | Lt (e, e') -> lt (f e) (f e')
  | Leq (e, e') -> leq (f e) (f e')
  | Gt (e, e') -> gt (f e) (f e')
  | Geq (e, e') -> geq (f e) (f e')
  | Plus (e, e') -> plus (f e) (f e')
  | Sub (e, e') -> sub (f e) (f e')
  | Mul (e, e') -> mul (f e) (f e')
  | Div (e, e') -> div (f e) (f e')
  | Mod (e, e') -> modulo (f e) (f e')

(* Overflow-freedom obligation for an expression: the conjunction of
   [Arith.in_bounds] over every arithmetic *result* it computes (each
   [Plus]/[Sub]/[Mul] node). Leaves ([Int] literals, [VarInst] reads) are in
   range by assumption -- literals fit by construction, and variables are kept
   in range by the invariant that every prior write was itself proven safe (the
   WLP will add that as a hypothesis) -- so they contribute nothing. Comparisons
   yield a boolean, not a machine integer, so they add no bound of their own but
   still require their integer operands to be safe. [t_and_simp] keeps fully-safe
   expressions as [true] rather than a pile of [true /\ ...]. *)
let rec safe : type a. g_vars:Vars.t -> ?l_vars:Vars.t -> a expr -> T.term =
 fun ~g_vars ?l_vars e ->
  let self = safe ~g_vars ?l_vars in
  match e with
  | Value _ -> T.t_true
  | Plus (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) ->
      (* Division overflows in exactly one case, [min_int / -1] (its result
         [2^62] is one past [max_int]), so it carries a bound like the others.
         Well-definedness (a non-zero divisor) is a separate, always-on
         obligation -- see [defined]. *)
      let operands = T.t_and_simp (self a) (self b) in
      let result = Arith.in_bounds (expr_to_term ~g_vars ?l_vars e) in
      T.t_and_simp operands result
  | Mod (a, b) ->
      (* [a mod b] is bounded in magnitude by [b], so if the operands are in
         range the result is too -- no bound of its own. *)
      T.t_and_simp (self a) (self b)
  | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
      T.t_and_simp (self a) (self b)

(* Well-definedness obligation for an expression: the conjunction of
   [divisor <> 0] over every [Div]/[Mod] node it contains. Unlike [safe] (which
   guards against overflow and is only imposed in machine-integer mode), this is
   *always* required -- [Arith.div]/[modulo] are left unspecified by a zero
   divisor and the interpreter/compiled binary would raise [Division_by_zero],
   so a verified program must prove it never divides by zero. [t_and_simp]
   collapses division-free expressions back to [true]. *)
let rec defined : type a. g_vars:Vars.t -> ?l_vars:Vars.t -> a expr -> T.term =
 fun ~g_vars ?l_vars e ->
  let self = defined ~g_vars ?l_vars in
  match e with
  | Value _ -> T.t_true
  | Plus (a, b) | Sub (a, b) | Mul (a, b) -> T.t_and_simp (self a) (self b)
  | Div (a, b) | Mod (a, b) ->
      let nonzero =
        Arith.neq (expr_to_term ~g_vars ?l_vars b) (T.t_nat_const 0)
      in
      T.t_and_simp (T.t_and_simp (self a) (self b)) nonzero
  | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
      T.t_and_simp (self a) (self b)

module Proc_map = Map.Make (String)

(* Variables a command may assign: assignment targets plus the [writes] of any
   procedure it calls. Used both to havoc the variables a loop modifies and to
   check that a procedure's declared [writes] covers what it actually mutates. *)
let rec assigned_vars procs c =
  match c with
  | Assgn (x, _) | Let (x, _) -> [ x ]
  | Seq (a, b) | If (_, a, b) -> assigned_vars procs a @ assigned_vars procs b
  | While (_, _, body) -> assigned_vars procs body
  | Proc (f, _) -> (
      match Proc_map.find_opt f procs with
      | Some ((t : Triple.t), _) -> t.ws
      | None -> [])
  | IntExpr _ | Print _ -> []

(* https://en.wikipedia.org/wiki/Predicate_transformer_semantics *)
module Wlp = struct
  let sub_old_vars ~g_vars ~l_vars ws p =
    let map =
      List.filter_map
        (fun w ->
          let x = Vars.find_opt ("_" ^ w) l_vars in
          Option.map (fun x -> (x, T.t_var (Vars.find w g_vars))) x)
        ws
    in
    T.(t_subst (Mvs.of_list map) p)

  (* Restrict havoc'd variables to the machine range. [ys] are fresh variables
     standing for post-havoc values -- a loop iteration's modified vars or a
     procedure call's written globals. On a real machine those are always
     representable, so guard the havoc'd formula with [in_bounds] for each;
     otherwise the overflow obligations inside would also have to hold for
     out-of-range values and would spuriously fail. A no-op outside
     machine-integer mode, so the default WLP is unchanged. *)
  let havoc_in_bounds ~machine_int ys body =
    if not machine_int then body
    else
      let premise =
        List.fold_left
          (fun acc y -> T.t_and_simp acc (Arith.in_bounds (T.t_var y)))
          T.t_true ys
      in
      T.t_implies_simp premise body

  let proc ~machine_int g_vars q l_vars ps (t : Triple.t) =
    let p_f = Logic.translate_term ~g_vars ~l_vars t.p in
    let q_f = Logic.translate_term ~g_vars ~l_vars t.q in
    let sub_params p =
      let map =
        List.map2
          (fun fp e -> (Vars.find fp l_vars, expr_to_term ~g_vars ~l_vars e))
          t.ps ps
      in
      T.(t_subst (Mvs.of_list map) p)
    in
    (* q_f[x_i <- e_i] *)
    let post = sub_params q_f in
    (* p_f[x_i <- e_i] *)
    let pre = sub_params p_f in
    let sub_written_vars p =
      let ys = List.map (fun _ -> Vars.create_fresh "y") t.ws in
      let map =
        List.map2 (fun w y -> (Vars.find w g_vars, T.t_var y)) t.ws ys
      in
      T.t_forall_close ys []
        (havoc_in_bounds ~machine_int ys (T.t_subst (T.Mvs.of_list map) p))
    in
    (* forall y. ((q_f[x_i <- e_i] -> q)[x <- y])[x_i@old <- x_i] *)
    let q_sub =
      T.t_implies post q |> sub_written_vars
      |> sub_old_vars ~g_vars ~l_vars t.ws
    in
    (* p_f[x_i <- e_i] /\ forall y. (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i] -> q[x_i <- y_i]) *)
    T.(t_and pre q_sub)

  let rec cmd procs ~machine_int ~g_vars ~l_vars c q =
    let cmd = cmd procs ~machine_int ~g_vars ~l_vars in
    (* Well-definedness obligations for an expression evaluated by [c]. The
       divisor-non-zero part ([defined]) is always imposed; the overflow-freedom
       part ([safe]) only in machine-integer mode. In the default (unbounded,
       division-free) case both collapse to [true] and the WLP is unchanged. *)
    let safe_e : type a. a expr -> T.term =
     fun e ->
      let overflow = if machine_int then safe ~g_vars ~l_vars e else T.t_true in
      T.t_and_simp (defined ~g_vars ~l_vars e) overflow
    in
    match c with
    (* [IntExpr]/[Print] do not change the state but do evaluate their argument,
       so its arithmetic must not overflow. *)
    | IntExpr e -> T.t_and_simp (safe_e e) q
    | Print e -> T.t_and_simp (safe_e e) q
    | Seq (c, c') -> cmd c (cmd c' q)
    | Assgn (x, e) | Let (x, e) ->
        (* safe(e) /\ forall y. y = e -> q[ x <- y ] *)
        let e_t = expr_to_term ~g_vars ~l_vars e in
        let x = Vars.find_fallback x l_vars g_vars in
        let y = Vars.create_fresh "y" in
        let y_t = T.t_var y in
        let q_sub = T.t_subst_single x y_t q in
        let assign =
          T.(t_forall_close [ y ] [] (t_implies (t_equ y_t e_t) q_sub))
        in
        T.t_and_simp (safe_e e) assign
    | Proc (f, ps) ->
        (* safe(e_i) for each actual argument (evaluated in the caller's scope)
            /\ p_f[x_i <- e_i]
            /\ forall y.
              (q_f[x_i <- e_i][x_i <- y_i][x_i@old <- x_i]
              -> q[x_i <- y_i]) *)
        let args_safe =
          List.fold_left (fun acc e -> T.t_and_simp acc (safe_e e)) T.t_true ps
        in
        let triple, callee_l_vars = Proc_map.find f procs in
        T.t_and_simp args_safe
          (proc ~machine_int g_vars q callee_l_vars ps triple)
    | If (b, c, c') ->
        (* safe(b) /\ ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
        let t = expr_to_term ~g_vars ~l_vars b in
        let wp = cmd c q in
        let wp' = cmd c' q in
        let branches = T.(t_and (t_implies t wp) (t_implies (t_not t) wp')) in
        T.t_and_simp (safe_e b) branches
    | While (inv, b, c) ->
        (* inv
            /\ forall y_i.
                ((inv -> safe(b))
                /\ ((b /\ inv) -> wlp(c, inv))
                /\ ((~b /\ inv) -> q))[x_i <- y_i]
           where the x_i are the variables the body modifies. Havoc'ing them
           (fresh y_i + t_forall_close, as for procedure calls) makes the two
           obligations hold for an arbitrary iteration rather than only for the
           loop's entry state. In machine-integer mode the guard is evaluated in
           every reachable (invariant-satisfying) state, so its arithmetic must
           not overflow there; the havoc'd y_i are also restricted to the machine
           range (see [havoc_in_bounds]). *)
        let guard = expr_to_term ~g_vars ~l_vars b in
        let inv_t = Logic.translate_term ~g_vars ~l_vars inv in
        let s = cmd c inv_t in
        let guard_safe = T.t_implies_simp inv_t (safe_e b) in
        let iterate = T.(t_implies (t_and guard inv_t) s) in
        let postcond = T.(t_implies (t_and (t_not guard) inv_t) q) in
        let modified = List.sort_uniq String.compare (assigned_vars procs c) in
        let ys = List.map (fun _ -> Vars.create_fresh "y") modified in
        let map =
          List.map2
            (fun x y -> (Vars.find_fallback x l_vars g_vars, T.t_var y))
            modified ys
        in
        let havoc p =
          T.t_forall_close ys []
            (havoc_in_bounds ~machine_int ys (T.t_subst (T.Mvs.of_list map) p))
        in
        T.t_and inv_t (havoc T.(t_and iterate (t_and_simp postcond guard_safe)))
end

let merge_in vm x =
  match Vars.find_opt x vm with
  | None -> Vars.(add x (create_fresh x)) vm
  | Some _ -> vm

let verify_procedure ?timeout ~machine_int ~is_main g_vars procs
    ((t : Triple.t), l_vars) =
  let p, q =
    if is_main then
      (Logic.translate_term ~g_vars t.p, Logic.translate_term ~g_vars t.q)
    else
      ( Logic.translate_term ~g_vars ~l_vars t.p,
        Logic.translate_term ~g_vars ~l_vars t.q )
  in
  let p_gen =
    Wlp.(
      sub_old_vars ~g_vars ~l_vars t.ws
      @@ cmd procs ~machine_int ~g_vars ~l_vars t.c q)
  in
  let merged_vars =
    List.fold_left merge_in (Vars.union l_vars g_vars) t.ps |> list_of_var_map
  in
  (* In machine-integer mode assume every live variable already holds a
     representable value on entry -- the invariant that the machine only ever
     stores in-range integers (each prior write was proven [safe]). Without
     this the overflow obligations would be unprovable. *)
  let antecedent =
    if machine_int then
      List.fold_left
        (fun acc v -> T.t_and_simp acc (Arith.in_bounds (T.t_var v)))
        p merged_vars
    else p
  in
  debug_print t.f merged_vars p q p_gen;
  let goal = T.t_implies antecedent p_gen in
  Smt.Prover.prove timeout (Arith.task_for goal) merged_vars goal

exception Proc_invalid of string

(* A procedure's [writes] clause must name every global it actually assigns;
   otherwise a caller's WLP would never havoc that global and could "prove"
   properties the body violates. Variables that are procedure-local (absent
   from [globals]) are invisible to callers and need not be declared. *)
let writes_are_declared globals procs (t : Triple.t) =
  assigned_vars procs t.c
  |> List.for_all (fun x ->
      match Vars.find_opt x globals with
      | None -> true
      | Some _ -> List.mem x t.ws)

let verify ?debug:d ?timeout ?(machine_int = false) program =
  debug := Option.value ~default:false d;
  let procs, (main, globals) = split_last program in
  let f ~is_main procs proc =
    let (t : Triple.t) = fst proc in
    let result =
      if (not is_main) && not (writes_are_declared globals procs t) then
        Smt.Prover.Invalid
      else verify_procedure ?timeout ~machine_int ~is_main globals procs proc
    in
    match result with
    | Valid -> Proc_map.add t.f proc procs
    | Invalid | Failed _ -> raise (Proc_invalid t.f)
  in
  try
    let proc_map = List.fold_left (f ~is_main:false) Proc_map.empty procs in
    let (_ : (Triple.t * Vars.t) Proc_map.t) =
      f ~is_main:true proc_map (main, globals)
    in
    Smt.Prover.Valid
  with Proc_invalid s ->
    if !debug then Printf.printf "Procedure '%s' cannot be verified\n" s;
    Smt.Prover.Invalid
