(* Ahead-of-time backend: [emit] transpiles the typed AST to OCaml source and
   [to_native] builds it into a native binary via [ocamlfind ocamlopt]. Driven
   by [cav compile] (see [Main.compile] and [bin/main.ml]). See PLAN.md
   (Option B).

   Correctness note: Cavalry integers are Why3's *unbounded* mathematical
   integers, and that is the model [Hoare.verify] proves against. OCaml native
   [int] is 63-bit and wraps (see the [check_overflow] discussion in
   [Ast.Runtime]), so a naive transpile to [+]/[-]/[*] could produce results the
   proof never sanctioned. We therefore emit arbitrary-precision [Zarith]
   arithmetic so the runtime integer model coincides with the verified one. *)

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

(* The integer backend the emitted program computes in. Each op is a string
   usable as a prefix function, so arithmetic and comparisons emit uniformly as
   [(op a b)].
   - [zarith]: Why3's *unbounded* integers, the model [Hoare.verify] proves
     against -- the default, so the binary agrees with what was verified.
   - [native]: OCaml 63-bit [int], which *wraps*, and matches the tree-walking
     interpreter (also native ints). Faster. It can diverge from the proof only
     when a value overflows, so [cav compile --native-int] runs the verification
     gate in machine-integer mode ([Hoare.verify ~machine_int:true]), which
     proves overflow cannot happen -- making the native-int binary sound. It is
     unsound only if that gate is skipped with --no-verify. *)
type ops = {
  ty : string; (* the OCaml type of a program integer *)
  zero : string; (* its zero literal *)
  lit : int -> string; (* an int literal *)
  add : string;
  sub : string;
  mul : string;
  div : string; (* truncated division, matching the interpreter's native [/] *)
  mod_ : string; (* remainder, matching the interpreter's native [mod] *)
  eq : string;
  lt : string;
  leq : string;
  gt : string;
  geq : string;
  to_string : string; (* int -> string, for printing *)
}

let zarith =
  {
    ty = "Z.t";
    zero = "Z.zero";
    lit = (fun i -> Printf.sprintf "(Z.of_int (%d))" i);
    add = "Z.add";
    sub = "Z.sub";
    mul = "Z.mul";
    (* [Z.div]/[Z.rem] truncate towards zero (sign of dividend), matching OCaml
       native [/]/[mod] and Why3's [ComputerDivision]. [Z.ediv]/[Z.erem] would
       be Euclidean and disagree on negatives. *)
    div = "Z.div";
    mod_ = "Z.rem";
    eq = "Z.equal";
    lt = "Z.lt";
    leq = "Z.leq";
    gt = "Z.gt";
    geq = "Z.geq";
    to_string = "Z.to_string";
  }

let native =
  {
    ty = "int";
    zero = "0";
    lit = (fun i -> Printf.sprintf "(%d)" i);
    add = "( + )";
    sub = "( - )";
    mul = "( * )";
    div = "( / )";
    mod_ = "( mod )";
    eq = "( = )";
    lt = "( < )";
    leq = "( <= )";
    gt = "( > )";
    geq = "( >= )";
    to_string = "string_of_int";
  }

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

let emit_int_value ~ops ~locals : int value -> string = function
  | Int i -> ops.lit i
  | VarInst x -> emit_read ~locals x

let rec emit_int ~ops ~locals (e : int expr) : string =
  let bin op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~ops ~locals a)
      (emit_int ~ops ~locals b)
  in
  match e with
  | Value v -> emit_int_value ~ops ~locals v
  | Plus (a, b) -> bin ops.add a b
  | Sub (a, b) -> bin ops.sub a b
  | Mul (a, b) -> bin ops.mul a b
  | Div (a, b) -> bin ops.div a b
  | Mod (a, b) -> bin ops.mod_ a b

let emit_bool ~ops ~locals (e : bool expr) : string =
  let cmp op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~ops ~locals a)
      (emit_int ~ops ~locals b)
  in
  match e with
  | Value (Bool b) -> string_of_bool b
  | Eq (a, b) -> cmp ops.eq a b
  | Neq (a, b) -> Printf.sprintf "(not %s)" (cmp ops.eq a b)
  | Lt (a, b) -> cmp ops.lt a b
  | Leq (a, b) -> cmp ops.leq a b
  | Gt (a, b) -> cmp ops.gt a b
  | Geq (a, b) -> cmp ops.geq a b

(* Flatten the (possibly unbalanced) [Seq] tree into left-to-right statement
   order, so a [Let] can scope over everything sequenced after it. *)
let rec flatten = function Seq (c, c') -> flatten c @ flatten c' | c -> [ c ]

(* Each command compiles to an OCaml expression of the backend int type whose
   evaluation performs its effects and yields its *value* -- the int the
   interpreter threads through and [cav run] prints for main. Value of each
   form, matching [Runtime.exec_cmd]: [Assgn]/[Let] -> the RHS; [Print] -> 0;
   [If] -> the taken branch; [While] -> 0; [Proc] -> the callee's body value;
   [Seq] -> its last. *)
let rec emit_cmd ~ops ~locals c : string =
  match c with
  | Seq _ -> emit_block ~ops ~locals (flatten c)
  | Assgn (x, e) ->
      Printf.sprintf "(%s := %s; %s)" (gref x) (emit_int ~ops ~locals e)
        (emit_read ~locals x)
  | Let (_, e) -> emit_int ~ops ~locals e (* trailing/standalone: value only *)
  | Print e ->
      Printf.sprintf "(print_string (%s (%s)); print_newline (); %s)"
        ops.to_string (emit_int ~ops ~locals e) ops.zero
  | IntExpr e -> emit_int ~ops ~locals e
  | If (b, c, c') ->
      Printf.sprintf "(if %s then %s else %s)" (emit_bool ~ops ~locals b)
        (emit_cmd ~ops ~locals c) (emit_cmd ~ops ~locals c')
  | While (_, b, c) ->
      Printf.sprintf "(while %s do ignore (%s : %s) done; %s)"
        (emit_bool ~ops ~locals b) (emit_cmd ~ops ~locals c) ops.ty ops.zero
  | Proc (f, args) ->
      let args =
        match args with
        | [] -> " ()"
        | _ ->
            List.map
              (fun a -> Printf.sprintf " (%s)" (emit_int ~ops ~locals a))
              args
            |> String.concat ""
      in
      Printf.sprintf "(%s%s)" (pname f) args

and emit_block ~ops ~locals : cmd list -> string = function
  | [] -> ops.zero
  | [ s ] -> emit_cmd ~ops ~locals s
  | Let (x, e) :: rest ->
      Printf.sprintf "(let %s = %s in\n%s)" (lname x) (emit_int ~ops ~locals e)
        (emit_block ~ops ~locals:(Str_set.add x locals) rest)
  | s :: rest ->
      Printf.sprintf "(ignore (%s : %s);\n%s)" (emit_cmd ~ops ~locals s) ops.ty
        (emit_block ~ops ~locals rest)

(* A procedure becomes an OCaml function; its formals are the initial local
   scope. Zero-formal procedures take [unit] so calls read [p_f ()]. *)
let emit_proc ~ops ((t : Triple.t), _vars) : string =
  let params =
    match t.ps with [] -> "()" | ps -> List.map lname ps |> String.concat " "
  in
  let locals = Str_set.of_list t.ps in
  Printf.sprintf "let %s %s =\n%s" (pname t.f) params
    (emit_cmd ~ops ~locals t.c)

let emit ?(native_int = false) (program : (Triple.t * Vars.t) list) : string =
  let ops = if native_int then native else zarith in
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
    |> List.map (fun x -> Printf.sprintf "let %s = ref %s" (gref x) ops.zero)
    |> String.concat "\n"
  in
  let proc_defs = List.map (emit_proc ~ops) procs |> String.concat "\n\n" in
  let main_body =
    emit_block ~ops ~locals:Str_set.empty (flatten main.Triple.c)
  in
  let sections = List.filter (fun s -> s <> "") [ decls; proc_defs ] in
  Printf.sprintf
    "%s\n\n\
     let () =\n\
    \  let _result = %s in\n\
    \  print_string (%s _result); print_newline ()\n"
    (String.concat "\n\n" sections)
    main_body ops.to_string

(* Raised when the external OCaml toolchain is missing or rejects the emitted
   source. Kept distinct from [Unsupported] (a Cavalry feature we cannot yet
   compile) so the CLI can phrase each differently. *)
exception Toolchain_error of string

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc contents)

(* Build [ocaml_source] into a native executable at [output] by shelling out to
   [ocamlfind ocamlopt]. Zarith is linked unless [native_int] (which must match
   the backend [emit] used, so the source and the link line agree). The source
   and ocamlopt's intermediate artifacts ([.cmi]/[.cmx]/[.o]) go in a temp file
   that is removed afterwards; only the executable at [output] survives. *)
let to_native ?(native_int = false) ~output ocaml_source =
  let ml = Filename.temp_file "cavalry_out_" ".ml" in
  let base = Filename.remove_extension ml in
  write_file ml ocaml_source;
  let cleanup () =
    List.iter
      (fun ext -> try Sys.remove (base ^ ext) with Sys_error _ -> ())
      [ ".ml"; ".cmi"; ".cmo"; ".cmx"; ".o" ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      let packages = if native_int then "" else "-package zarith -linkpkg " in
      let cmd =
        Printf.sprintf "ocamlfind ocamlopt %s%s -o %s" packages
          (Filename.quote ml) (Filename.quote output)
      in
      match Sys.command cmd with
      | 0 -> ()
      | 127 ->
          raise
            (Toolchain_error
               "'ocamlfind'/'ocamlopt' not found; install the OCaml toolchain \
                and the 'zarith' package")
      | n ->
          raise
            (Toolchain_error (Printf.sprintf "ocamlopt exited with code %d" n)))
