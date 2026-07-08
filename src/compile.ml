(* Ahead-of-time backend: transpile the typed AST to OCaml source, which is
   then built into a native binary by [ocamlfind ocamlopt] (CLI wiring lands in
   a later milestone). See PLAN.md (Option B).

   Correctness note: Cavalry integers are Why3's *unbounded* mathematical
   integers, and that is the model [Hoare.verify] proves against. OCaml native
   [int] is 63-bit and wraps (see the [check_overflow] discussion in
   [Ast.Runtime]), so a naive transpile to [+]/[-]/[*] could produce results the
   proof never sanctioned. We therefore emit arbitrary-precision [Zarith]
   arithmetic so the runtime integer model coincides with the verified one.

   Milestone 2: straight-line, arithmetic, [if] and [while]. Procedures raise
   [Unsupported] and are added in the next milestone. *)

open Ast
open Program

exception Unsupported of string

module Str_set = Set.Make (String)

(* Two categories of program variable, mirroring the interpreter's
   global/local split:
   - [refs]: targets of [<-] ([Assgn]). The interpreter writes these into its
     persistent global environment, so we compile them to top-level mutable
     [ref]s; a read is [!x] and an assignment is [x := ...].
   - lexical locals: [let]-bound names ([Let]). The interpreter shadows globals
     with these for the remainder of the enclosing sequence, so we compile them
     to ordinary [let ... in] bindings and a read is just [x].

   A read resolves against the currently in-scope locals first, then the refs --
   the same precedence as [Runtime.find_var] (locals before globals) -- so a
   name used as both is compiled correctly on each side of the [let]. *)
let rec assigned = function
  | Seq (c, c') -> Str_set.union (assigned c) (assigned c')
  | Assgn (x, _) -> Str_set.singleton x
  | If (_, c, c') -> Str_set.union (assigned c) (assigned c')
  | While (_, _, c) -> assigned c
  | Let _ | Print _ | IntExpr _ | Proc _ -> Str_set.empty

let emit_read ~refs ~locals x =
  if Str_set.mem x locals then x
  else if Str_set.mem x refs then "!" ^ x
  else
    (* A read that is neither an in-scope local nor an assigned global is a
       read of an uninitialised variable; the interpreter raises
       [Runtime.UnboundError] on it. Emit the bare name so the OCaml compiler
       likewise rejects the program rather than inventing a value. *)
    x

let emit_int_value ~refs ~locals : int value -> string = function
  | Int i -> Printf.sprintf "(Z.of_int (%d))" i
  | VarInst x -> emit_read ~refs ~locals x

let rec emit_int ~refs ~locals (e : int expr) : string =
  let bin op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~refs ~locals a)
      (emit_int ~refs ~locals b)
  in
  match e with
  | Value v -> emit_int_value ~refs ~locals v
  | Plus (a, b) -> bin "Z.add" a b
  | Sub (a, b) -> bin "Z.sub" a b
  | Mul (a, b) -> bin "Z.mul" a b

let emit_bool ~refs ~locals (e : bool expr) : string =
  let cmp op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int ~refs ~locals a)
      (emit_int ~refs ~locals b)
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

(* Every command has an int *value* (the interpreter threads it through and
   [cav run] prints main's). We mirror that: each command compiles to an OCaml
   expression of type [Z.t] whose evaluation performs the command's effects and
   yields that value.

   Value of each form, matching [Runtime.exec_cmd]: [Assgn]/[Let] -> the RHS;
   [Print] -> 0; [If] -> the taken branch; [While] -> 0; [Seq] -> its last. *)
let rec emit_cmd ~refs ~locals c : string =
  match c with
  | Seq _ -> emit_block ~refs ~locals (flatten c)
  | Assgn (x, e) ->
      Printf.sprintf "(%s := %s; !%s)" x (emit_int ~refs ~locals e) x
  | Let (_, e) -> emit_int ~refs ~locals e (* trailing/standalone: value only *)
  | Print e ->
      Printf.sprintf
        "(print_string (Z.to_string (%s)); print_newline (); Z.zero)"
        (emit_int ~refs ~locals e)
  | IntExpr e -> emit_int ~refs ~locals e
  | If (b, c, c') ->
      Printf.sprintf "(if %s then %s else %s)"
        (emit_bool ~refs ~locals b)
        (emit_cmd ~refs ~locals c)
        (emit_cmd ~refs ~locals c')
  | While (_, b, c) ->
      Printf.sprintf "(while %s do ignore (%s : Z.t) done; Z.zero)"
        (emit_bool ~refs ~locals b)
        (emit_cmd ~refs ~locals c)
  | Proc _ -> raise (Unsupported "procedure call")

and emit_block ~refs ~locals : cmd list -> string = function
  | [] -> "Z.zero"
  | [ s ] -> emit_cmd ~refs ~locals s
  | Let (x, e) :: rest ->
      Printf.sprintf "(let %s = %s in\n%s)" x (emit_int ~refs ~locals e)
        (emit_block ~refs ~locals:(Str_set.add x locals) rest)
  | s :: rest ->
      Printf.sprintf "(ignore (%s : Z.t);\n%s)" (emit_cmd ~refs ~locals s)
        (emit_block ~refs ~locals rest)

(* Emit a full OCaml program. Milestone 2: [main] only. *)
let emit (program : (Triple.t * Vars.t) list) : string =
  let main =
    match List.rev program with
    | (main, _) :: [] -> main
    | _ :: _ :: _ -> raise (Unsupported "procedures")
    | [] -> failwith "empty program"
  in
  let body = main.Triple.c in
  let refs = assigned body in
  let decls =
    Str_set.elements refs
    |> List.map (fun x -> Printf.sprintf "let %s = ref Z.zero" x)
    |> String.concat "\n"
  in
  Printf.sprintf
    "%s\n\n\
     let () =\n\
    \  let _result = %s in\n\
    \  print_string (Z.to_string _result); print_newline ()\n"
    decls
    (emit_block ~refs ~locals:Str_set.empty (flatten body))
