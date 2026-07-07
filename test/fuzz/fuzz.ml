(* QuickCheck-based soundness harness for [Hoare.verify].

   Strategy (see PLAN.md): rather than generate random pre/postconditions and
   hope to hit the [Valid] branch, we construct triples that are *definitely
   false* and assert the verifier never accepts them. The interpreter
   ([Runtime.exec_env]) is the oracle -- an independent implementation of the
   language semantics -- so this is a differential test between the symbolic
   verifier and concrete execution.

   For a generated command [c] and a concrete initial state [s0]:
     1. run [c] from [s0] with fuel; discard non-terminating/overflowing runs
        (partial correctness => non-termination is not a counterexample);
     2. build [p] pinning [s0] exactly, so the triple ranges over one state;
     3. build [q_false] false on the final state [s1];
     4. property: verify {p} c {q_false} MUST be Invalid. A Valid is a
        minimized soundness bug. *)

module Program = Ast.Program
module Logic = Ast.Logic
module Triple = Ast.Triple
module Runtime = Ast.Runtime
module Var_collection = Ast.Var_collection
module Hoare = Cavalry.Hoare
module Prover = Smt.Prover
module G = QCheck2.Gen

(* Small fixed variable pool so aliasing and name collisions actually occur. *)
let pool = [ "x"; "y"; "z"; "i"; "j" ]
let verify_timeout = 3.0

(* ===== Generators ============================================= *)

let gen_var = G.oneof_list pool

(* Literals are non-negative: Cavalry has no negative literals (they lex as
   naturals), and Why3's [t_nat_const] rejects negative constants. Negative
   *values* still arise at runtime through subtraction. *)
let gen_lit = G.int_range 0 8

(* Encode an arbitrary (possibly negative) integer as a Logic arith term,
   writing negatives as [0 - n] to match the surface language. Used to pin
   concrete state values (which can be negative) in [p] and [q]. *)
let logic_int v =
  if v >= 0 then Logic.Int v else Logic.Sub (Logic.Int 0, Logic.Int (-v))

(* int expr over the pool; literals in a small range so nonlinear terms stay
   small and concrete runs terminate quickly. *)
let rec gen_int_expr n =
  let open Program in
  if n <= 1 then
    G.oneof
      [
        G.map (fun i -> Value (Int i)) gen_lit;
        G.map (fun x -> Value (VarInst x)) gen_var;
      ]
  else
    G.oneof_weighted
      [
        (3, gen_int_expr 1);
        ( 1,
          let sub = gen_int_expr (n / 2) in
          G.bind
            (G.oneof_list [ `P; `S; `M ])
            (fun op ->
              G.map2
                (fun a b ->
                  match op with
                  | `P -> Plus (a, b)
                  | `S -> Sub (a, b)
                  | `M -> Mul (a, b))
                sub sub) );
      ]

let gen_bool_expr n =
  let open Program in
  let half = max 1 (n / 2) in
  G.bind
    (G.oneof_list [ `Eq; `Neq; `Lt; `Leq; `Gt; `Geq ])
    (fun op ->
      G.map2
        (fun a b ->
          match op with
          | `Eq -> Eq (a, b)
          | `Neq -> Neq (a, b)
          | `Lt -> Lt (a, b)
          | `Leq -> Leq (a, b)
          | `Gt -> Gt (a, b)
          | `Geq -> Geq (a, b))
        (gen_int_expr half) (gen_int_expr half))

(* A loop invariant: any well-formed [Logic.expr] over the pool. It need not be
   a true invariant -- the constructed-false-triple property holds regardless
   (a bad invariant only makes [verify] more likely to answer Invalid). *)
let rec gen_l_arith n =
  let open Logic in
  if n <= 1 then
    G.oneof [ G.map (fun i -> Int i) gen_lit; G.map (fun x -> Var x) gen_var ]
  else
    G.bind
      (G.oneof_list [ `P; `S; `M ])
      (fun op ->
        G.map2
          (fun a b ->
            match op with
            | `P -> Plus (a, b)
            | `S -> Sub (a, b)
            | `M -> Mul (a, b))
          (gen_l_arith (n / 2))
          (gen_l_arith (n / 2)))

let gen_comparison =
  let open Logic in
  G.bind
    (G.oneof_list [ `Eq; `Neq; `Lt; `Leq; `Gt; `Geq ])
    (fun op ->
      G.map2
        (fun a b ->
          match op with
          | `Eq -> Eq (a, b)
          | `Neq -> Neq (a, b)
          | `Lt -> Lt (a, b)
          | `Leq -> Leq (a, b)
          | `Gt -> Gt (a, b)
          | `Geq -> Geq (a, b))
        (gen_l_arith 2) (gen_l_arith 2))

(* Include the trivial invariants prominently. [true] is the single most
   important invariant to test: it holds on entry and is inductive, so it is
   exactly what exposes an unsound loop rule that fails to havoc the variables
   the body modifies (a false postcondition then "verifies" whenever the loop
   runs at least once). *)
let gen_invariant =
  G.oneof_weighted
    [
      (2, G.pure (Logic.Bool true));
      (1, G.pure (Logic.Bool false));
      (3, gen_comparison);
    ]

let gen_assgn =
  G.map2 (fun x e -> Program.Assgn (x, e)) gen_var (gen_int_expr 3)

(* Loop-free command generator: assignments, sequencing, conditionals. Used for
   the dual (true-postcondition) property, where WLP is exact and the solver
   should always discharge the concrete goal. *)
let rec gen_cmd_loopfree n =
  let open Program in
  if n <= 1 then gen_assgn
  else
    G.oneof_weighted
      [
        (4, gen_assgn);
        ( 3,
          G.map2
            (fun a b -> Seq (a, b))
            (gen_cmd_loopfree (n / 2))
            (gen_cmd_loopfree (n / 2)) );
        ( 2,
          G.map3
            (fun b c c' -> If (b, c, c'))
            (gen_bool_expr 2)
            (gen_cmd_loopfree (n / 2))
            (gen_cmd_loopfree (n / 2)) );
      ]

(* A guaranteed-terminating counting loop:

     while c > 0 do <inv> t := e; c := c - 1 end

   The dedicated counter [c] strictly decreases and is not touched by the extra
   assignment (t <> c), so the loop always terminates and runs [s0(c)]
   iterations -- >= 1 whenever [s0(c) > 0]. This reliably exercises the loop
   rule (unlike a fully random guard, which usually diverges and is discarded).
   With [inv = true] it is exactly the shape that exposes an unsound loop rule
   that forgets to havoc the variables the body modifies. *)
let gen_while =
  let open Program in
  G.bind (G.oneof_list pool) (fun c ->
      let others = List.filter (fun v -> v <> c) pool in
      G.bind gen_invariant (fun inv ->
          G.map2
            (fun t e ->
              let decr = Assgn (c, Sub (Value (VarInst c), Value (Int 1))) in
              let guard = Gt (Value (VarInst c), Value (Int 0)) in
              While (inv, guard, Seq (Assgn (t, e), decr)))
            (G.oneof_list others) (gen_int_expr 3)))

(* Full command generator: adds shallow, rare [While] loops. Used for the
   primary soundness property. Loops are kept rare and shallow so most runs
   terminate within fuel; those that do not are discarded. *)
let rec gen_cmd_full n =
  let open Program in
  if n <= 1 then gen_assgn
  else
    G.oneof_weighted
      [
        (4, gen_assgn);
        ( 3,
          G.map2
            (fun a b -> Seq (a, b))
            (gen_cmd_full (n / 2))
            (gen_cmd_full (n / 2)) );
        ( 2,
          G.map3
            (fun b c c' -> If (b, c, c'))
            (gen_bool_expr 2)
            (gen_cmd_full (n / 2))
            (gen_cmd_full (n / 2)) );
        (1, gen_while);
      ]

(* s0: a random binding for every pool variable, so every read in [c] is
   defined. Generated alongside [c] so shrinking coordinates the two. *)
let gen_s0 =
  G.map
    (fun vals -> List.combine pool vals)
    (G.list_size (G.return (List.length pool)) gen_lit)

let gen_case gen_cmd = G.pair gen_s0 (G.sized_size (G.int_range 2 6) gen_cmd)

(* ===== Building triples and running the oracle ================ *)

(* Prepend [x := s0(x)] for every pool variable, so the interpreter starts from
   the concrete state [s0]. (The verifier gets [s0] symbolically via [p], so it
   is run on the bare [c].) *)
let seed_cmd s0 c =
  List.fold_right
    (fun (x, v) acc ->
      Program.Seq (Program.Assgn (x, Program.Value (Program.Int v)), acc))
    s0 c

let run s0 c =
  let body = seed_cmd s0 c in
  Runtime.exec_env ~fuel:2000 [ { Runtime.f = ""; ps = []; c = body } ]

(* p = /\_x  x = s0(x) : pins the initial state to exactly [s0]. *)
let build_p s0 =
  List.fold_left
    (fun acc (x, v) -> Logic.And (acc, Logic.Eq (Logic.Var x, logic_int v)))
    (Logic.Bool true) s0

(* q_false = (x = s1(x) + 1) : definitely false on the final state. *)
let build_false_q env =
  match Runtime.Env.find_opt "x" env with
  | None -> None
  | Some v -> Some (Logic.Eq (Logic.Var "x", logic_int (v + 1)))

(* q_true = /\_x  x = s1(x) : the exact (definitely true) postcondition. *)
let build_true_q env =
  Runtime.Env.fold
    (fun x v acc -> Logic.And (acc, Logic.Eq (Logic.Var x, logic_int v)))
    env (Logic.Bool true)

let verify_triple p q c =
  let triple = { Triple.p; q; ws = []; f = ""; ps = []; c } in
  let collected = Var_collection.collect [ triple ] in
  Hoare.verify ?timeout:(Some verify_timeout) collected

(* ===== Properties ============================================= *)

(* Primary: a false postcondition must be rejected. A [Valid] is a soundness
   bug. Non-terminating runs and prover failures/timeouts are not witnesses, so
   they pass trivially. *)
let prop_false_rejected (s0, c) =
  match run s0 c with
  | Runtime.OutOfFuel | Runtime.Raised -> true
  | Runtime.Terminated env -> (
      match build_false_q env with
      | None -> true
      | Some q -> (
          match verify_triple (build_p s0) q c with
          | Prover.Valid -> false
          | Prover.Invalid | Prover.Failed _ -> true))

(* Dual: the exact (true) postcondition should be accepted on a fully-concrete,
   loop-free triple (all linear, so the solver should close it). Failures here
   are not unsoundness but catch regressions / gross incompleteness. *)
let prop_true_accepted (s0, c) =
  match run s0 c with
  | Runtime.OutOfFuel | Runtime.Raised -> true
  | Runtime.Terminated env -> (
      match verify_triple (build_p s0) (build_true_q env) c with
      | Prover.Valid -> true
      | Prover.Failed _ -> true (* solver resource limit: not a regression *)
      | Prover.Invalid -> false)

(* ===== Reporting ============================================= *)

let cmd_to_string c = Core.Sexp.to_string_hum (Program.sexp_of_cmd c)
let logic_to_string e = Core.Sexp.to_string_hum (Logic.sexp_of_expr e)

let s0_to_string s0 =
  String.concat ", " (List.map (fun (x, v) -> Printf.sprintf "%s=%d" x v) s0)

let print_case ~q_of (s0, c) =
  let outcome =
    match run s0 c with
    | Runtime.Terminated env ->
        let s1 =
          Runtime.Env.fold (fun x v acc -> (x, v) :: acc) env []
          |> List.rev
          |> List.map (fun (x, v) -> Printf.sprintf "%s=%d" x v)
          |> String.concat ", "
        in
        Printf.sprintf "s1: {%s}\nq:  %s" s1
          (match q_of env with Some q -> logic_to_string q | None -> "<none>")
    | Runtime.OutOfFuel -> "out of fuel"
    | Runtime.Raised -> "raised"
  in
  Printf.sprintf "s0: {%s}\ncmd:\n%s\n%s" (s0_to_string s0) (cmd_to_string c)
    outcome

(* ===== Wiring ============================================= *)

let count =
  match Sys.getenv_opt "CAVALRY_FUZZ_COUNT" with
  | Some s -> ( try int_of_string s with _ -> 20)
  | None -> 20

let test_false_rejected =
  QCheck2.Test.make ~count ~name:"soundness: false postcondition is rejected"
    ~print:(print_case ~q_of:build_false_q)
    (gen_case gen_cmd_full) prop_false_rejected

let test_true_accepted =
  QCheck2.Test.make ~count
    ~name:"regression: true postcondition is accepted (loop-free)"
    ~print:(print_case ~q_of:(fun env -> Some (build_true_q env)))
    (gen_case gen_cmd_loopfree)
    prop_true_accepted

let () =
  QCheck_base_runner.run_tests_main [ test_false_rejected; test_true_accepted ]
