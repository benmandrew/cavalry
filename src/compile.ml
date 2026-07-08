(* Ahead-of-time backend: transpile the typed AST to OCaml source, which is
   then built into a native binary by [ocamlfind ocamlopt] (CLI wiring lands in
   a later milestone). See PLAN.md (Option B).

   Correctness note: Cavalry integers are Why3's *unbounded* mathematical
   integers, and that is the model [Hoare.verify] proves against. OCaml native
   [int] is 63-bit and wraps (see the [check_overflow] discussion in
   [Ast.Runtime]), so a naive transpile to [+]/[-]/[*] could produce results the
   proof never sanctioned. We therefore emit arbitrary-precision [Zarith]
   arithmetic so the runtime integer model coincides with the verified one.

   Milestone 1: straight-line + arithmetic only. Procedures, [if] and [while]
   raise [Unsupported] and are added in later milestones. *)

open Ast
open Program

exception Unsupported of string

module Str_set = Set.Make (String)

(* Variables that are targets of [<-] ([Assgn]) become mutable OCaml [ref]s;
   [let]-bound names stay immutable. A read ([VarInst]) of a mutable is emitted
   as [!x], otherwise as [x]. *)
let rec assigned = function
  | Seq (c, c') -> Str_set.union (assigned c) (assigned c')
  | Assgn (x, _) -> Str_set.singleton x
  | If (_, c, c') -> Str_set.union (assigned c) (assigned c')
  | While (_, _, c) -> assigned c
  | Let _ | Print _ | IntExpr _ | Proc _ -> Str_set.empty

let emit_int_value mut : int value -> string = function
  | Int i -> Printf.sprintf "(Z.of_int (%d))" i
  | VarInst x -> if Str_set.mem x mut then "!" ^ x else x

let rec emit_int mut (e : int expr) : string =
  let bin op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int mut a) (emit_int mut b)
  in
  match e with
  | Value v -> emit_int_value mut v
  | Plus (a, b) -> bin "Z.add" a b
  | Sub (a, b) -> bin "Z.sub" a b
  | Mul (a, b) -> bin "Z.mul" a b

let emit_bool mut (e : bool expr) : string =
  let cmp op a b =
    Printf.sprintf "(%s %s %s)" op (emit_int mut a) (emit_int mut b)
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
   yields that value. *)
let rec emit_cmd mut c : string =
  match c with
  | Seq _ -> emit_block mut (flatten c)
  | Assgn (x, e) -> Printf.sprintf "(%s := %s; !%s)" x (emit_int mut e) x
  | Let (_, e) -> emit_int mut e (* trailing/standalone let: value only *)
  | Print e ->
      Printf.sprintf
        "(print_string (Z.to_string (%s)); print_newline (); Z.zero)"
        (emit_int mut e)
  | IntExpr e -> emit_int mut e
  | If _ -> raise (Unsupported "if")
  | While _ -> raise (Unsupported "while")
  | Proc _ -> raise (Unsupported "procedure call")

and emit_block mut : cmd list -> string = function
  | [] -> "Z.zero"
  | [ s ] -> emit_cmd mut s
  | Let (x, e) :: rest ->
      Printf.sprintf "(let %s = %s in\n%s)" x (emit_int mut e)
        (emit_block mut rest)
  | s :: rest ->
      Printf.sprintf "(ignore (%s : Z.t);\n%s)" (emit_cmd mut s)
        (emit_block mut rest)

(* Emit a full OCaml program. Milestone 1: [main] only. *)
let emit (program : (Triple.t * Vars.t) list) : string =
  let main =
    match List.rev program with
    | (main, _) :: [] -> main
    | _ :: _ :: _ -> raise (Unsupported "procedures")
    | [] -> failwith "empty program"
  in
  let body = main.Triple.c in
  let mut = assigned body in
  let refs =
    Str_set.elements mut
    |> List.map (fun x -> Printf.sprintf "let %s = ref Z.zero" x)
    |> String.concat "\n"
  in
  Printf.sprintf
    "%s\n\n\
     let () =\n\
    \  let _result = %s in\n\
    \  print_string (Z.to_string _result); print_newline ()\n"
    refs
    (emit_block mut (flatten body))
