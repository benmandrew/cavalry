open Ast
open Ast.Program
module T = Why3.Term
module Ty = Why3.Ty

(* Why a verification obligation can fail. Each proof obligation the WLP emits
   is tagged (via [tag]) with one of these; when Alt-Ergo rejects a split
   subgoal, its tag is recovered to classify the failure for the user. *)
type reason =
  | Postcondition
  | Loop_invariant_init
  | Loop_invariant_preserved
  | Loop_variant
  | Recursive_variant
  | Call_precondition
  | Array_bounds
  | Array_length_nonneg
  | Nonzero_divisor
  | No_overflow
  | Undeclared_write
[@@deriving sexp_of, compare]

(* The human-readable explanation carried in the Why3 [expl:] attribute. This is
   both what the splitter propagates onto each subgoal and what the CLI prints,
   so it must round-trip through [reason_of_expl]. *)
let expl_of_reason = function
  | Postcondition -> "postcondition may not hold"
  | Loop_invariant_init -> "loop invariant may not hold on entry"
  | Loop_invariant_preserved -> "loop invariant may not be preserved"
  | Loop_variant -> "loop variant may not decrease"
  | Recursive_variant -> "recursive call's variant may not decrease"
  | Call_precondition -> "callee precondition may not hold"
  | Array_bounds -> "array index may be out of bounds"
  | Array_length_nonneg -> "array length may be negative"
  | Nonzero_divisor -> "divisor may be zero"
  | No_overflow -> "arithmetic may overflow"
  | Undeclared_write -> "assigns a global absent from its writes clause"

let all_reasons =
  [
    Postcondition;
    Loop_invariant_init;
    Loop_invariant_preserved;
    Loop_variant;
    Recursive_variant;
    Call_precondition;
    Array_bounds;
    Array_length_nonneg;
    Nonzero_divisor;
    No_overflow;
    Undeclared_write;
  ]

let reason_of_expl s =
  List.find_opt (fun r -> String.equal (expl_of_reason r) s) all_reasons

(* Tag every node of [t] with [reason]'s [expl:] attribute and, when given, its
   source location. Tagging only the root is not enough: [split_goal_right]
   breaks a conjunctive obligation into one subgoal per conjunct, and an
   attribute (or location) on the conjunction node is lost -- so both must be
   present on every atom the split can isolate. *)
let tag ?loc reason t =
  let attr = Why3.Ident.create_attribute ("expl:" ^ expl_of_reason reason) in
  let rec go t =
    let t = T.t_map go t in
    T.t_attr_set ?loc (Why3.Ident.Sattr.add attr t.T.t_attrs) t
  in
  go t

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
  let resolve x =
    match l_vars with
    | Some l_vars -> Vars.find_fallback x l_vars g_vars
    | None -> Vars.find x g_vars
  in
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
  | Get (a, i) -> aget (T.t_var (resolve a)) (f i)
  | Len a -> T.t_var (resolve (Vars.len_key a))

(* [0 <= i < len(a)]: the well-definedness obligation for accessing [a\[i\]],
   shared by array reads (in [defined]) and element writes (in [Wlp.cmd]). *)
let index_in_bounds ~g_vars ?l_vars ?loc a i_term =
  let len = expr_to_term ~g_vars ?l_vars (Len a) in
  tag ?loc Array_bounds
    (T.t_and (Arith.leq (T.t_nat_const 0) i_term) (Arith.lt i_term len))

(* Overflow-freedom obligation for an expression: the conjunction of
   [Arith.in_bounds] over every arithmetic *result* it computes (each
   [Plus]/[Sub]/[Mul] node). Leaves ([Int] literals, [VarInst] reads) are in
   range by assumption -- literals fit by construction, and variables are kept
   in range by the invariant that every prior write was itself proven safe (the
   WLP will add that as a hypothesis) -- so they contribute nothing. Comparisons
   yield a boolean, not a machine integer, so they add no bound of their own but
   still require their integer operands to be safe. [t_and_simp] keeps fully-safe
   expressions as [true] rather than a pile of [true /\ ...]. *)
let rec safe : type a.
    g_vars:Vars.t ->
    ?l_vars:Vars.t ->
    ?loc:Why3.Loc.position ->
    a expr ->
    T.term =
 fun ~g_vars ?l_vars ?loc e ->
  let self = safe ~g_vars ?l_vars ?loc in
  match e with
  | Value _ -> T.t_true
  | Plus (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) ->
      (* Division overflows in exactly one case, [min_int / -1] (its result
         [2^62] is one past [max_int]), so it carries a bound like the others.
         Well-definedness (a non-zero divisor) is a separate, always-on
         obligation -- see [defined]. *)
      let operands = T.t_and_simp (self a) (self b) in
      let result =
        tag ?loc No_overflow (Arith.in_bounds (expr_to_term ~g_vars ?l_vars e))
      in
      T.t_and_simp operands result
  | Mod (a, b) ->
      (* [a mod b] is bounded in magnitude by [b], so if the operands are in
         range the result is too -- no bound of its own. *)
      T.t_and_simp (self a) (self b)
  | Get (_, i) ->
      (* An element read is in range by the invariant that every stored value
         was itself proven [safe] on write; only the index expression can
         overflow. [Len] is a tracked variable, in range by assumption. *)
      self i
  | Len _ -> T.t_true
  | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
      T.t_and_simp (self a) (self b)

(* Well-definedness obligation for an expression: the conjunction of
   [divisor <> 0] over every [Div]/[Mod] node it contains, and
   [0 <= i < len(a)] over every array read [a\[i\]]. Unlike [safe] (which
   guards against overflow and is only imposed in machine-integer mode), this is
   *always* required -- [Arith.div]/[modulo] are left unspecified by a zero
   divisor and the interpreter/compiled binary would raise [Division_by_zero],
   so a verified program must prove it never divides by zero. [t_and_simp]
   collapses division-free expressions back to [true]. *)
let rec defined : type a.
    g_vars:Vars.t ->
    ?l_vars:Vars.t ->
    ?loc:Why3.Loc.position ->
    a expr ->
    T.term =
 fun ~g_vars ?l_vars ?loc e ->
  let self = defined ~g_vars ?l_vars ?loc in
  match e with
  | Value _ -> T.t_true
  | Plus (a, b) | Sub (a, b) | Mul (a, b) -> T.t_and_simp (self a) (self b)
  | Div (a, b) | Mod (a, b) ->
      let nonzero =
        tag ?loc Nonzero_divisor
          (Arith.neq (expr_to_term ~g_vars ?l_vars b) (T.t_nat_const 0))
      in
      T.t_and_simp (T.t_and_simp (self a) (self b)) nonzero
  | Get (a, i) ->
      let bounds =
        index_in_bounds ~g_vars ?l_vars ?loc a (expr_to_term ~g_vars ?l_vars i)
      in
      T.t_and_simp (self i) bounds
  | Len _ -> T.t_true
  | Eq (a, b) | Neq (a, b) | Lt (a, b) | Leq (a, b) | Gt (a, b) | Geq (a, b) ->
      T.t_and_simp (self a) (self b)

module Proc_map = Map.Make (String)

(* Variables a command may assign: assignment targets plus the [writes] of any
   procedure it calls. Used both to havoc the variables a loop modifies and to
   check that a procedure's declared [writes] covers what it actually mutates. *)
let rec assigned_vars procs c =
  match c with
  | Located (_, c) -> assigned_vars procs c
  | Assgn (x, _) | Let (x, _) -> [ x ]
  | Seq (a, b) | If (_, a, b) -> assigned_vars procs a @ assigned_vars procs b
  | While (_, _, _, body) -> assigned_vars procs body
  | Proc (f, _) -> (
      match Proc_map.find_opt f procs with
      | Some ((t : Triple.t), _) -> t.ws
      | None -> [])
  (* An element write changes the array's map; [array(n)] also resets its
     length, so both variables are modified. *)
  | ArrAssgn (a, _, _) -> [ a ]
  | ArrMake (a, _) -> [ a; Vars.len_key a ]
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
          (fun acc y ->
            (* Only integer havoc vars carry a machine-range constraint; an
               array (map) variable has no such bound. *)
            if Ty.ty_equal y.T.vs_ty Ty.ty_int then
              T.t_and_simp acc (Arith.in_bounds (T.t_var y))
            else acc)
          T.t_true ys
      in
      T.t_implies_simp premise body

  let proc ~machine_int ?loc g_vars q l_vars ps (t : Triple.t) =
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
    (* p_f[x_i <- e_i] -- the callee precondition the caller must establish *)
    let pre = tag ?loc Call_precondition (sub_params p_f) in
    let sub_written_vars p =
      (* Writing an array havocs both its element map and its length (a callee
         may re-create it with [array(n)]); a scalar havocs just itself. *)
      let ws_syms =
        List.concat_map
          (fun w ->
            let m = Vars.find w g_vars in
            match Vars.find_opt (Vars.len_key w) g_vars with
            | Some l -> [ m; l ]
            | None -> [ m ])
          t.ws
      in
      let ys = List.map Vars.fresh_like ws_syms in
      let map = List.map2 (fun w y -> (w, T.t_var y)) ws_syms ys in
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

  (* [self] identifies the procedure whose body is being verified: its name and
     optional variant. A call back to that name is recursive; if the procedure
     declares a variant it must strictly decrease across the call (see the [Proc]
     case). *)
  let rec cmd procs ~machine_int ~self ~g_vars ~l_vars c q =
    let cmd = cmd procs ~machine_int ~self ~g_vars ~l_vars in
    (* Peel the source location off the command; its obligations are tagged with
       it so a failing subgoal can be traced back to this construct. *)
    let loc, c =
      match c with Located (l, c) -> (Some (Loc.to_why3 l), c) | c -> (None, c)
    in
    (* Well-definedness obligations for an expression evaluated by [c]. The
       divisor-non-zero part ([defined]) is always imposed; the overflow-freedom
       part ([safe]) only in machine-integer mode. In the default (unbounded,
       division-free) case both collapse to [true] and the WLP is unchanged. *)
    let safe_e : type a. a expr -> T.term =
     fun e ->
      let overflow =
        if machine_int then safe ~g_vars ~l_vars ?loc e else T.t_true
      in
      T.t_and_simp (defined ~g_vars ~l_vars ?loc e) overflow
    in
    match c with
    (* [Located] is peeled above; this arm is unreachable but keeps the match
       total (a nested wrapper would simply recurse). *)
    | Located (_, c) -> cmd c q
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
    | ArrMake (a, n) ->
        (* a <- array(n): length := n, elements := all zeros.
           safe(n) /\ 0 <= n /\ q[ a <- const 0 ][ len(a) <- n ] *)
        let n_t = expr_to_term ~g_vars ~l_vars n in
        let a_v = Vars.find_fallback a l_vars g_vars in
        let len_v = Vars.find_fallback (Vars.len_key a) l_vars g_vars in
        let q_sub =
          T.t_subst (T.Mvs.of_list [ (a_v, Arith.azero); (len_v, n_t) ]) q
        in
        let nonneg =
          tag ?loc Array_length_nonneg (Arith.leq (T.t_nat_const 0) n_t)
        in
        T.t_and_simp (safe_e n) (T.t_and_simp nonneg q_sub)
    | ArrAssgn (a, i, e) ->
        (* a[i] <- e: safe(i) /\ safe(e) /\ 0 <= i < len(a)
           /\ q[ a <- set a i e ] *)
        let i_t = expr_to_term ~g_vars ~l_vars i in
        let e_t = expr_to_term ~g_vars ~l_vars e in
        let a_v = Vars.find_fallback a l_vars g_vars in
        let q_sub = T.t_subst_single a_v (Arith.aset (T.t_var a_v) i_t e_t) q in
        let bounds = index_in_bounds ~g_vars ~l_vars ?loc a i_t in
        T.t_and_simp
          (T.t_and_simp (safe_e i) (safe_e e))
          (T.t_and_simp bounds q_sub)
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
        (* Termination of recursion: if this call targets the enclosing
           procedure and it declares a variant [V], the measure at the actual
           arguments must be non-negative and strictly below its value at the
           enclosing formals -- [0 <= V[e_i] < V[x_i]]. This is the variant rule
           applied across a recursive call instead of a loop back-edge. Without a
           variant the recursion is only checked for partial correctness. *)
        let decrease =
          match self with
          | self_name, Some measure when String.equal f self_name ->
              let v_formals =
                Logic.translate_arith_term ~g_vars ~l_vars measure
              in
              let sub =
                List.map2
                  (fun fp e ->
                    (Vars.find fp callee_l_vars, expr_to_term ~g_vars ~l_vars e))
                  triple.Triple.ps ps
              in
              let v_actuals = T.t_subst (T.Mvs.of_list sub) v_formals in
              tag ?loc Recursive_variant
                (T.t_and
                   (Arith.leq (T.t_nat_const 0) v_actuals)
                   (Arith.lt v_actuals v_formals))
          | _ -> T.t_true
        in
        T.t_and_simp decrease
          (T.t_and_simp args_safe
             (proc ~machine_int ?loc g_vars q callee_l_vars ps triple))
    | If (b, c, c') ->
        (* safe(b) /\ ( b -> wlp(c, q) ) /\ ( ~b -> wlp(c', q) ) *)
        let t = expr_to_term ~g_vars ~l_vars b in
        let wp = cmd c q in
        let wp' = cmd c' q in
        let branches = T.(t_and (t_implies t wp) (t_implies (t_not t) wp')) in
        T.t_and_simp (safe_e b) branches
    | While (inv, variant, b, c) ->
        (* inv
            /\ forall y_i.
                ((inv -> safe(b))
                /\ ((b /\ inv) -> BODY)
                /\ ((~b /\ inv) -> q))[x_i <- y_i]
           where the x_i are the variables the body modifies. Havoc'ing them
           (fresh y_i + t_forall_close, as for procedure calls) makes the two
           obligations hold for an arbitrary iteration rather than only for the
           loop's entry state. In machine-integer mode the guard is evaluated in
           every reachable (invariant-satisfying) state, so its arithmetic must
           not overflow there; the havoc'd y_i are also restricted to the machine
           range (see [havoc_in_bounds]).

           BODY is [wlp(c, inv)] (partial correctness) unless the loop carries a
           [variant] measure V, in which case it is
             forall w. w = V -> (0 <= V /\ wlp(c, inv /\ V < w))
           the *total*-correctness rule: the measure is non-negative and each
           iteration strictly decreases it. [w] freezes V's pre-body value (it is
           fresh, so [wlp] never rewrites it), while the [V] inside [wlp] is the
           post-body measure -- so the obligation is post-V < pre-V. *)
        let guard = expr_to_term ~g_vars ~l_vars b in
        let inv_t = Logic.translate_term ~g_vars ~l_vars inv in
        (* The invariant plays three roles below: proven to hold on entry
           ([Loop_invariant_init]), proven re-established by the body (as the
           WLP target, [Loop_invariant_preserved]), and assumed as a hypothesis
           (left untagged). *)
        let inv_pres = tag ?loc Loop_invariant_preserved inv_t in
        let s =
          match variant with
          | None -> cmd c inv_pres
          | Some measure ->
              let m = Logic.translate_arith_term ~g_vars ~l_vars measure in
              let w = Vars.create_fresh "variant" in
              let w_t = T.t_var w in
              let decreases =
                cmd c
                  (T.t_and inv_pres (tag ?loc Loop_variant (Arith.lt m w_t)))
              in
              let bounded =
                tag ?loc Loop_variant (Arith.leq (T.t_nat_const 0) m)
              in
              T.t_forall_close [ w ] []
                (T.t_implies (T.t_equ w_t m) (T.t_and bounded decreases))
        in
        let guard_safe = T.t_implies_simp inv_t (safe_e b) in
        let iterate = T.(t_implies (t_and guard inv_t) s) in
        let postcond = T.(t_implies (t_and (t_not guard) inv_t) q) in
        let modified = List.sort_uniq String.compare (assigned_vars procs c) in
        let vss =
          List.map (fun x -> Vars.find_fallback x l_vars g_vars) modified
        in
        let ys = List.map Vars.fresh_like vss in
        let map = List.map2 (fun vs y -> (vs, T.t_var y)) vss ys in
        let havoc p =
          T.t_forall_close ys []
            (havoc_in_bounds ~machine_int ys (T.t_subst (T.Mvs.of_list map) p))
        in
        T.t_and
          (tag ?loc Loop_invariant_init inv_t)
          (havoc T.(t_and iterate (t_and_simp postcond guard_safe)))
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
  (* The procedure's postcondition is the default obligation: everything the WLP
     ultimately establishes is [q], so tag it now and let the tag ride down
     through the calculus. More specific obligations ([safe], bounds, variants,
     ...) are tagged at their own construction sites and, being separate split
     subgoals, are classified on their own. *)
  let q = tag Postcondition q in
  let p_gen =
    Wlp.(
      sub_old_vars ~g_vars ~l_vars t.ws
      @@ cmd procs ~machine_int ~self:(t.f, t.variant) ~g_vars ~l_vars t.c q)
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
        (fun acc v ->
          (* Only scalar (integer) variables carry a machine-range assumption;
             array (map) variables have no such bound. *)
          if Ty.ty_equal v.T.vs_ty Ty.ty_int then
            T.t_and_simp acc (Arith.in_bounds (T.t_var v))
          else acc)
        p merged_vars
    else p
  in
  debug_print t.f merged_vars p q p_gen;
  let goal = T.t_implies antecedent p_gen in
  (* The map theory is also needed when a [map]-typed variable is quantified but
     never has a symbol applied to it (e.g. a postcondition of [true]). *)
  let uses_map =
    Arith.uses_map goal
    || List.exists (fun v -> Ty.ty_equal v.T.vs_ty Arith.ty_int_map) merged_vars
  in
  let split_task = Arith.task ~div:(Arith.uses_div goal) ~map:uses_map in
  let obligations = Smt.Prover.split_obligations split_task merged_vars goal in
  (* Discharge each obligation on its own task, detecting division per subgoal:
     a subgoal that does not mention [div]/[mod] need not carry the
     ComputerDivision axioms (a conservative split inherited from the Alt-Ergo
     backend, whose matching loop could diverge on them -- see [Arith]). Map
     inclusion is harmless, so the whole-goal [uses_map] is reused. The first
     failing obligation determines the reported reason and location. *)
  let rec check = function
    | [] -> (Smt.Prover.Valid, None, None)
    | (f, expl, loc) :: rest -> (
        let task = Arith.task ~div:(Arith.uses_div f) ~map:uses_map in
        match Smt.Prover.prove_term timeout task f with
        | Smt.Prover.Valid -> check rest
        | Smt.Prover.Invalid ->
            (Smt.Prover.Invalid, reason_of_expl expl, Option.map Loc.of_why3 loc)
        | Smt.Prover.Failed s -> (Smt.Prover.Failed s, None, None))
  in
  check obligations

exception
  Proc_invalid of string * Smt.Prover.result * reason option * Loc.t option

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

(* The outcome of verifying a whole program: the prover verdict, plus -- when
   the program is rejected -- the name of the first procedure ([main] included)
   whose body failed to verify, the reason, and the source location of the
   offending construct. [failing_proc] is [None] iff [result] is [Valid]. [loc]
   is [None] for whole-procedure obligations (e.g. a plain postcondition, which
   is not tied to one construct) and for static rejections. *)
type report = {
  result : Smt.Prover.result;
  failing_proc : string option;
  reason : reason option;
  loc : Loc.t option;
}

let verify_report ?debug:d ?timeout ?(machine_int = false) program =
  debug := Option.value ~default:false d;
  let procs, (main, globals) = split_last program in
  let f ~is_main procs proc =
    let (t : Triple.t) = fst proc in
    (* Register the procedure in [procs] *before* verifying its body, so a
       self-recursive call resolves to its own contract (the inductive
       hypothesis). This lifts the earlier bottom-up ordering restriction. main
       is never a callee, so it is not registered. *)
    let procs = if is_main then procs else Proc_map.add t.f proc procs in
    let result, reason, loc =
      if (not is_main) && not (writes_are_declared globals procs t) then
        (Smt.Prover.Invalid, Some Undeclared_write, None)
      else verify_procedure ?timeout ~machine_int ~is_main globals procs proc
    in
    match result with
    | Valid -> procs
    | Invalid | Failed _ -> raise (Proc_invalid (t.f, result, reason, loc))
  in
  try
    let proc_map = List.fold_left (f ~is_main:false) Proc_map.empty procs in
    let (_ : (Triple.t * Vars.t) Proc_map.t) =
      f ~is_main:true proc_map (main, globals)
    in
    {
      result = Smt.Prover.Valid;
      failing_proc = None;
      reason = None;
      loc = None;
    }
  with Proc_invalid (s, result, reason, loc) ->
    { result; failing_proc = Some s; reason; loc }

let verify ?debug ?timeout ?machine_int program =
  (verify_report ?debug ?timeout ?machine_int program).result
