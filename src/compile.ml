(* Ahead-of-time backend: transpile the typed AST to OCaml source, which is
   then built into a native binary by [ocamlfind ocamlopt] (CLI wiring lands in
   a later milestone). See PLAN.md (Option B).

   Correctness note: Cavalry integers are Why3's *unbounded* mathematical
   integers, and that is the model [Hoare.verify] proves against. OCaml native
   [int] is 63-bit and wraps (see the [check_overflow] discussion in
   [Ast.Runtime]), so a naive transpile to [+]/[-]/[*] could produce results the
   proof never sanctioned. We therefore emit arbitrary-precision [Zarith]
   arithmetic so the runtime integer model coincides with the verified one.

   Milestone 3: the whole language, including procedures. *)

open Ast
open Program

exception Unsupported of string

module Str_set = Set.Make (String)

(* The interpreter keeps two disjoint stores: a persistent global environment
   and a shadowing per-scope local environment ([Runtime.{global,local}_vars]).
   We reproduce that split with three prefixed OCaml namespaces so a program
   name can never collide with an OCaml keyword or with a name in the other
   store (a formal [a] and a global [a] are genuinely different cells in the
   interpreter, and become [l_a] / [g_a] here):
   - [g_x]: a global, compiled to a top-level mutable [ref]. Read [!g_x],
     written [g_x := _]. Every [<-] ([Assgn]) target is a global, in main and
     in procedures alike, because the interpreter's [Assgn] always writes the
     global store.
   - [l_x]: a local -- a procedure formal or a [let]-bound name. Read [l_x].
   - [p_f]: a procedure, compiled to an OCaml function.

   Cavalry identifiers are [(a-z|A-Z|_)+], all valid OCaml identifier tails, so
   the prefixes are enough. *)
let gref x = "g_" ^ x
let lname x = "l_" ^ x
let pname f = "p_" ^ f

(* Globals to declare = every [Assgn] target anywhere in the program. *)
let rec assigned = function
  | Seq (c, c') -> Str_set.union (assigned c) (assigned c')
  | Assgn (x, _) -> Str_set.singleton x
  | If (_, c, c') -> Str_set.union (assigned c) (assigned c')
  | While (_, _, c) -> assigned c
  | Let _ | Print _ | IntExpr _ | Proc _ -> Str_set.empty

(* [locals] is the set of program names currently bound in the local store
   (formals + in-scope [let]s). A read resolves local-first then global, the
   same precedence as [Runtime.find_var]. A name in neither is a read of an
   uninitialised variable -- the interpreter raises [Runtime.UnboundError]; we
   emit [!g_x] with no matching declaration so [ocamlopt] rejects it too. *)
let emit_read ~locals x = if Str_set.mem x locals then lname x else "!" ^ gref x

let emit_int_value ~locals : int value -> string = function
  | Int i -> Printf.sprintf "(Z.of_int (%d))" i
  | VarInst x -> emit_read ~locals x

let rec emit_int ~locals (e : int expr) : string =
  let bin op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~locals a) (emit_int ~locals b)
  in
  match e with
  | Value v -> emit_int_value ~locals v
  | Plus (a, b) -> bin "Z.add" a b
  | Sub (a, b) -> bin "Z.sub" a b
  | Mul (a, b) -> bin "Z.mul" a b

let emit_bool ~locals (e : bool expr) : string =
  let cmp op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~locals a) (emit_int ~locals b)
  in
  match e with
  | Value (Bool b) -> string_of_bool b
  | Eq (a, b) -> cmp "Z.equal" a b
  | Neq (a, b) -> Printf.sprintf "(not %s)" (cmp "Z.equal" a b)
  | Lt (a, b) -> cmp "Z.lt" a b
  | Leq (a, b) -> cmp "Z.leq" a b
  | Gt (a, b) -> cmp "Z.gt" a b
  | Geq (a, b) -> cmp "Z.geq" a b

(* Flatten the (possibly unbalanced) [Seq] tree into left-to-right statement
   order, so a [Let] can scope over everything sequenced after it. *)
let rec flatten = function Seq (c, c') -> flatten c @ flatten c' | c -> [ c ]

(* Each command compiles to an OCaml expression of type [Z.t] whose evaluation
   performs its effects and yields its *value* -- the int the interpreter
   threads through and [cav run] prints for main. Value of each form, matching
   [Runtime.exec_cmd]: [Assgn]/[Let] -> the RHS; [Print] -> 0; [If] -> the taken
   branch; [While] -> 0; [Proc] -> the callee's body value; [Seq] -> its last. *)
let rec emit_cmd ~locals c : string =
  match c with
  | Seq _ -> emit_block ~locals (flatten c)
  | Assgn (x, e) ->
      Printf.sprintf "(%s := %s; %s)" (gref x) (emit_int ~locals e)
        (emit_read ~locals x)
  | Let (_, e) -> emit_int ~locals e (* trailing/standalone: value only *)
  | Print e ->
      Printf.sprintf
        "(print_string (Z.to_string (%s)); print_newline (); Z.zero)"
        (emit_int ~locals e)
  | IntExpr e -> emit_int ~locals e
  | If (b, c, c') ->
      Printf.sprintf "(if %s then %s else %s)" (emit_bool ~locals b)
        (emit_cmd ~locals c) (emit_cmd ~locals c')
  | While (_, b, c) ->
      Printf.sprintf "(while %s do ignore (%s : Z.t) done; Z.zero)"
        (emit_bool ~locals b) (emit_cmd ~locals c)
  | Proc (f, args) ->
      let args =
        match args with
        | [] -> " ()"
        | _ ->
            List.map (fun a -> Printf.sprintf " (%s)" (emit_int ~locals a)) args
            |> String.concat ""
      in
      Printf.sprintf "(%s%s)" (pname f) args

and emit_block ~locals : cmd list -> string = function
  | [] -> "Z.zero"
  | [ s ] -> emit_cmd ~locals s
  | Let (x, e) :: rest ->
      Printf.sprintf "(let %s = %s in\n%s)" (lname x) (emit_int ~locals e)
        (emit_block ~locals:(Str_set.add x locals) rest)
  | s :: rest ->
      Printf.sprintf "(ignore (%s : Z.t);\n%s)" (emit_cmd ~locals s)
        (emit_block ~locals rest)

(* A procedure becomes an OCaml function; its formals are the initial local
   scope. Zero-formal procedures take [unit] so calls read [p_f ()]. *)
let emit_proc ((t : Triple.t), _vars) : string =
  let params =
    match t.ps with [] -> "()" | ps -> List.map lname ps |> String.concat " "
  in
  let locals = Str_set.of_list t.ps in
  Printf.sprintf "let %s %s =\n%s" (pname t.f) params (emit_cmd ~locals t.c)

let emit (program : (Triple.t * Vars.t) list) : string =
  let procs, main =
    match List.rev program with
    | (main, _) :: rev_procs -> (List.rev rev_procs, main)
    | [] -> failwith "empty program"
  in
  (* Emitting procedures in source order is valid OCaml: [verify] accepts a
     program only when every callee is defined before its callers (bottom-up
     fold over [split_last]), which is also legal definition order here. *)
  let refs =
    List.fold_left
      (fun acc (t, _) -> Str_set.union acc (assigned t.Triple.c))
      Str_set.empty program
  in
  let decls =
    Str_set.elements refs
    |> List.map (fun x -> Printf.sprintf "let %s = ref Z.zero" (gref x))
    |> String.concat "\n"
  in
  let proc_defs = List.map emit_proc procs |> String.concat "\n\n" in
  let main_body = emit_block ~locals:Str_set.empty (flatten main.Triple.c) in
  let sections = List.filter (fun s -> s <> "") [ decls; proc_defs ] in
  Printf.sprintf
    "%s\n\n\
     let () =\n\
    \  let _result = %s in\n\
    \  print_string (Z.to_string _result); print_newline ()\n"
    (String.concat "\n\n" sections)
    main_body
