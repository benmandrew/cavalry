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
              While (inv, None, guard, Seq (Assgn (t, e), decr)))
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

let main_triple ~p ~q ~c : Triple.t = { p; q; ws = []; f = "main"; ps = []; c }

let verify_program (triples : Triple.t list) =
  Hoare.verify ?timeout:(Some verify_timeout) (Var_collection.collect triples)

(* ===== Procedure generation (framing) ========================= *)

(* Each non-hidden pool variable plays one of three roles in the generated
   procedure [f]: it is either not written, written and declared in [writes],
   or written but *not* declared (an incomplete writes clause). This directly
   generates "random writes clauses, including deliberately incomplete ones". *)
type role = Skip | Declared of int | Undeclared of int

let gen_role =
  G.oneof_weighted
    [
      (2, G.pure Skip);
      (2, G.map (fun k -> Declared k) (G.int_range 1 8));
      (2, G.map (fun k -> Undeclared k) (G.int_range 1 8));
    ]

(* A framing case: an initial state, a distinguished [hidden] global that [f]
   always writes (by [+kh], so it definitely changes) but *never* declares, and
   a role for every other pool variable. [hidden] is the guaranteed incomplete
   write that gives this property teeth against a missing [writes] check. *)
let gen_framing =
  G.bind gen_s0 (fun s0 ->
      G.bind (G.oneof_list pool) (fun hidden ->
          G.bind (G.int_range 1 8) (fun kh ->
              let others = List.filter (fun v -> v <> hidden) pool in
              G.map
                (fun roles -> (s0, hidden, kh, List.combine others roles))
                (G.flatten_list (List.map (fun _ -> gen_role) others)))))

let framing_body hidden kh roles =
  let open Program in
  let assign v k = Assgn (v, Plus (Value (VarInst v), Value (Int k))) in
  let stmts =
    assign hidden kh
    :: List.filter_map
         (fun (v, role) ->
           match role with
           | Skip -> None
           | Declared k | Undeclared k -> Some (assign v k))
         roles
  in
  List.fold_left (fun acc st -> Seq (acc, st)) (List.hd stmts) (List.tl stmts)

let framing_writes roles =
  List.filter_map
    (fun (v, role) -> match role with Declared _ -> Some v | _ -> None)
    roles

(* Triples for a framing case:

     procedure f () = requires { true } ensures { true } writes { <declared> }
       hidden := hidden + kh; <other writes>
     end
     { P }  f()  { hidden = s0(hidden) }

   [f]'s postcondition tells the caller nothing, and [hidden] is written but
   undeclared. A correct [writes_are_declared] check rejects [f] outright
   (Invalid). Drop the check and the caller's WLP no longer havocs [hidden], so
   it "proves" it is unchanged (Valid) -- while the interpreter changed it. This
   is the minimized shape of [verify_false_writes_undeclared.cav]. *)
let framing_triples (s0, hidden, kh, roles) : Triple.t list =
  let f : Triple.t =
    {
      p = Logic.Bool true;
      q = Logic.Bool true;
      ws = framing_writes roles;
      f = "f";
      ps = [];
      c = framing_body hidden kh roles;
    }
  in
  let q = Logic.Eq (Logic.Var hidden, logic_int (List.assoc hidden s0)) in
  [ f; main_triple ~p:(build_p s0) ~q ~c:(Program.Proc ("f", [])) ]

(* Execute a framing case through the interpreter: main seeds [s0] then calls
   [f]. [f]'s global writes persist to main, so the final environment shows the
   real effect of the (partially undeclared) writes. *)
let run_framing (s0, hidden, kh, roles) =
  let f_proc = { Runtime.f = "f"; ps = []; c = framing_body hidden kh roles } in
  let main_proc =
    { Runtime.f = "main"; ps = []; c = seed_cmd s0 (Program.Proc ("f", [])) }
  in
  Runtime.exec_env ~fuel:2000 [ f_proc; main_proc ]

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
          match verify_program [ main_triple ~p:(build_p s0) ~q ~c ] with
          | Prover.Valid -> false
          | Prover.Invalid | Prover.Failed _ -> true))

(* Dual: the exact (true) postcondition should be accepted on a fully-concrete,
   loop-free triple (all linear, so the solver should close it). Failures here
   are not unsoundness but catch regressions / gross incompleteness. *)
let prop_true_accepted (s0, c) =
  match run s0 c with
  | Runtime.OutOfFuel | Runtime.Raised -> true
  | Runtime.Terminated env -> (
      let q = build_true_q env in
      match verify_program [ main_triple ~p:(build_p s0) ~q ~c ] with
      | Prover.Valid -> true
      | Prover.Failed _ -> true (* solver resource limit: not a regression *)
      | Prover.Invalid -> false)

(* Framing: a procedure with an incomplete [writes] clause must never let a
   caller "prove" a false triple. The interpreter confirms [hidden] really
   changed (differential sanity); then [verify] must answer Invalid. A [Valid]
   is a framing soundness bug. *)
let prop_framing ((s0, hidden, _, _) as case) =
  match run_framing case with
  | Runtime.OutOfFuel | Runtime.Raised -> true
  | Runtime.Terminated env -> (
      match Runtime.Env.find_opt hidden env with
      | Some v when v = List.assoc hidden s0 -> true (* unchanged: skip *)
      | _ -> (
          match verify_program (framing_triples case) with
          | Prover.Valid -> false
          | Prover.Invalid | Prover.Failed _ -> true))

(* ===== .cav pretty-printer =================================== *)

(* Render a failing case as surface-syntax Cavalry, so a counterexample can be
   dropped straight into test/ as a regression fixture. Binary operators are
   fully parenthesised to sidestep precedence; negatives are written [0 - n]. *)

let indent n s =
  let pad = String.make n ' ' in
  String.split_on_char '\n' s
  |> List.map (fun l -> pad ^ l)
  |> String.concat "\n"

let value_to_cav : type a. a Program.value -> string = function
  | Program.Int i -> string_of_int i
  | Program.Bool b -> string_of_bool b
  | Program.VarInst x -> x
  | Program.Unit () -> "()"

let rec expr_to_cav : type a. a Program.expr -> string =
 fun e ->
  let bin op a b =
    Printf.sprintf "(%s %s %s)" (expr_to_cav a) op (expr_to_cav b)
  in
  match e with
  | Program.Value v -> value_to_cav v
  | Program.Plus (a, b) -> bin "+" a b
  | Program.Sub (a, b) -> bin "-" a b
  | Program.Mul (a, b) -> bin "*" a b
  | Program.Div (a, b) -> bin "/" a b
  | Program.Mod (a, b) -> bin "%" a b
  | Program.Eq (a, b) -> bin "=" a b
  | Program.Neq (a, b) -> bin "!=" a b
  | Program.Lt (a, b) -> bin "<" a b
  | Program.Leq (a, b) -> bin "<=" a b
  | Program.Gt (a, b) -> bin ">" a b
  | Program.Geq (a, b) -> bin ">=" a b
  | Program.Get (a, i) -> Printf.sprintf "%s[%s]" a (expr_to_cav i)
  | Program.Len a -> Printf.sprintf "len(%s)" a

let rec arith_to_cav = function
  | Logic.Int i -> string_of_int i
  | Logic.Var x -> x
  | Logic.Plus (a, b) ->
      Printf.sprintf "(%s + %s)" (arith_to_cav a) (arith_to_cav b)
  | Logic.Sub (a, b) ->
      Printf.sprintf "(%s - %s)" (arith_to_cav a) (arith_to_cav b)
  | Logic.Mul (a, b) ->
      Printf.sprintf "(%s * %s)" (arith_to_cav a) (arith_to_cav b)
  | Logic.Div (a, b) ->
      Printf.sprintf "(%s / %s)" (arith_to_cav a) (arith_to_cav b)
  | Logic.Mod (a, b) ->
      Printf.sprintf "(%s %% %s)" (arith_to_cav a) (arith_to_cav b)
  | Logic.Get (a, i) -> Printf.sprintf "%s[%s]" a (arith_to_cav i)
  | Logic.Len a -> Printf.sprintf "len(%s)" a

(* The grammar has no parenthesised-[logic_expr] rule (only [arith_expr] can be
   parenthesised), so boolean connectives and comparisons are emitted bare and
   rely on the declared precedence: [&&] is left-associative and comparisons
   bind tighter, which faithfully reproduces the left-nested [And] of
   comparisons this harness generates for P and Q. *)
let rec logic_to_cav = function
  | Logic.Bool b -> string_of_bool b
  | Logic.Not e -> Printf.sprintf "!%s" (logic_to_cav e)
  | Logic.And (a, b) ->
      Printf.sprintf "%s && %s" (logic_to_cav a) (logic_to_cav b)
  | Logic.Or (a, b) ->
      Printf.sprintf "%s || %s" (logic_to_cav a) (logic_to_cav b)
  | Logic.Impl (a, b) ->
      Printf.sprintf "%s -> %s" (logic_to_cav a) (logic_to_cav b)
  | Logic.Eq (a, b) ->
      Printf.sprintf "%s = %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Neq (a, b) ->
      Printf.sprintf "%s != %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Lt (a, b) ->
      Printf.sprintf "%s < %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Leq (a, b) ->
      Printf.sprintf "%s <= %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Gt (a, b) ->
      Printf.sprintf "%s > %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Geq (a, b) ->
      Printf.sprintf "%s >= %s" (arith_to_cav a) (arith_to_cav b)
  | Logic.Forall (x, e) -> Printf.sprintf "forall %s . %s" x (logic_to_cav e)
  | Logic.Exists (x, e) -> Printf.sprintf "exists %s . %s" x (logic_to_cav e)

let rec cmd_to_cav c =
  match c with
  | Program.Assgn (x, e) -> Printf.sprintf "%s <- %s" x (expr_to_cav e)
  | Program.Let (x, e) -> Printf.sprintf "%s <- %s" x (expr_to_cav e)
  | Program.Seq (a, b) -> Printf.sprintf "%s;\n%s" (cmd_to_cav a) (cmd_to_cav b)
  | Program.If (b, c0, c1) ->
      Printf.sprintf "if %s then\n%s\nelse\n%s\nend" (expr_to_cav b)
        (indent 2 (cmd_to_cav c0))
        (indent 2 (cmd_to_cav c1))
  | Program.While (inv, variant, b, body) ->
      let variant_clause =
        match variant with
        | None -> ""
        | Some m -> Printf.sprintf "  variant { %s }\n" (arith_to_cav m)
      in
      Printf.sprintf "while %s do\n  invariant { %s }\n%s%s\nend"
        (expr_to_cav b) (logic_to_cav inv) variant_clause
        (indent 2 (cmd_to_cav body))
  | Program.Proc (f, ps) ->
      Printf.sprintf "%s(%s)" f (String.concat ", " (List.map expr_to_cav ps))
  | Program.IntExpr e -> expr_to_cav e
  | Program.Print e -> Printf.sprintf "print %s" (expr_to_cav e)
  | Program.ArrMake (a, n) -> Printf.sprintf "%s <- array(%s)" a (expr_to_cav n)
  | Program.ArrAssgn (a, i, e) ->
      Printf.sprintf "%s[%s] <- %s" a (expr_to_cav i) (expr_to_cav e)

let proc_to_cav (t : Triple.t) =
  Printf.sprintf
    "procedure %s (%s) =\n\
    \  requires { %s }\n\
    \  ensures { %s }\n\
    \  writes { %s }\n\
     %s\n\
     end"
    t.f (String.concat ", " t.ps) (logic_to_cav t.p) (logic_to_cav t.q)
    (String.concat ", " t.ws)
    (indent 2 (cmd_to_cav t.c))

let main_to_cav (t : Triple.t) =
  Printf.sprintf "{ %s }\n%s\n{ %s }" (logic_to_cav t.p) (cmd_to_cav t.c)
    (logic_to_cav t.q)

let emit_cav (triples : Triple.t list) =
  let procs, main =
    match List.rev triples with
    | last :: rev_procs -> (List.rev rev_procs, last)
    | [] ->
        ( [],
          main_triple ~p:(Logic.Bool true) ~q:(Logic.Bool true)
            ~c:(Program.IntExpr (Program.Value (Program.Int 0))) )
  in
  String.concat "\n\n" (List.map proc_to_cav procs @ [ main_to_cav main ])

(* ===== Reporting ============================================= *)

let s0_to_string s0 =
  String.concat ", " (List.map (fun (x, v) -> Printf.sprintf "%s=%d" x v) s0)

let env_to_string env =
  Runtime.Env.fold (fun x v acc -> (x, v) :: acc) env []
  |> List.rev
  |> List.map (fun (x, v) -> Printf.sprintf "%s=%d" x v)
  |> String.concat ", "

let outcome_to_string = function
  | Runtime.Terminated env -> Printf.sprintf "s1: {%s}" (env_to_string env)
  | Runtime.OutOfFuel -> "out of fuel"
  | Runtime.Raised -> "raised"

let print_case ~triples_of (s0, c) =
  let triples = triples_of (s0, c) in
  Printf.sprintf "s0: {%s}\n%s\n\n.cav:\n%s" (s0_to_string s0)
    (outcome_to_string (run s0 c))
    (match triples with Some t -> emit_cav t | None -> "<no triple>")

let print_framing ((s0, hidden, kh, _) as case) =
  Printf.sprintf
    "s0: {%s}\nhidden (undeclared write): %s (+%d)\n%s\n\n.cav:\n%s"
    (s0_to_string s0) hidden kh
    (outcome_to_string (run_framing case))
    (emit_cav (framing_triples case))

(* ===== Wiring ============================================= *)

let count =
  match Sys.getenv_opt "CAVALRY_FUZZ_COUNT" with
  | Some s -> ( try int_of_string s with _ -> 20)
  | None -> 20

let test_false_rejected =
  let triples_of (s0, c) =
    match run s0 c with
    | Runtime.Terminated env ->
        Option.map
          (fun q -> [ main_triple ~p:(build_p s0) ~q ~c ])
          (build_false_q env)
    | Runtime.OutOfFuel | Runtime.Raised -> None
  in
  QCheck2.Test.make ~count ~name:"soundness: false postcondition is rejected"
    ~print:(print_case ~triples_of) (gen_case gen_cmd_full) prop_false_rejected

let test_true_accepted =
  let triples_of (s0, c) =
    match run s0 c with
    | Runtime.Terminated env ->
        Some [ main_triple ~p:(build_p s0) ~q:(build_true_q env) ~c ]
    | Runtime.OutOfFuel | Runtime.Raised -> None
  in
  QCheck2.Test.make ~count
    ~name:"regression: true postcondition is accepted (loop-free)"
    ~print:(print_case ~triples_of)
    (gen_case gen_cmd_loopfree)
    prop_true_accepted

let test_framing =
  QCheck2.Test.make ~count
    ~name:"soundness: incomplete writes clause is rejected" ~print:print_framing
    gen_framing prop_framing

let () =
  QCheck_base_runner.run_tests_main
    [ test_false_rejected; test_true_accepted; test_framing ]
