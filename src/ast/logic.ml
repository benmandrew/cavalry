open Core
module T = Why3.Term

type arith_expr =
  | Int of int
  | Var of string
  | Plus of arith_expr * arith_expr
  | Sub of arith_expr * arith_expr
  | Mul of arith_expr * arith_expr
  | Div of arith_expr * arith_expr
  | Mod of arith_expr * arith_expr
  | Get of string * arith_expr (* array element a[i] *)
  | Len of string (* array length len(a) *)
[@@deriving sexp_of, show]

type logic_expr =
  | Bool of bool
  | BoolVar of string (* a boolean program variable as a proposition *)
  | Not of logic_expr
  | And of logic_expr * logic_expr
  | Or of logic_expr * logic_expr
  | Impl of logic_expr * logic_expr
  | Eq of arith_expr * arith_expr
  | Neq of arith_expr * arith_expr
  | Lt of arith_expr * arith_expr
  | Leq of arith_expr * arith_expr
  | Gt of arith_expr * arith_expr
  | Geq of arith_expr * arith_expr
  | Forall of string * logic_expr (* forall x. body *)
  | Exists of string * logic_expr (* exists x. body *)
[@@deriving sexp_of, show]

type expr = logic_expr [@@deriving sexp_of, show]

(* Resolve a source name to its Why3 variable. Quantifier-bound names ([bound])
   shadow program variables; among program variables a global shadows a local
   (the language's existing convention). *)
let resolve ~g_vars ?l_vars ?(bound = Vars.empty) x =
  match Vars.find_opt x bound with
  | Some symbol -> symbol
  | None -> (
      match l_vars with
      | Some l_vars -> (
          match Vars.find_opt x g_vars with
          | Some symbol -> symbol
          | None -> Vars.find x l_vars)
      | None -> Vars.find x g_vars)

let rec translate_arith_term ~g_vars ?l_vars ?(bound = Vars.empty) e =
  let f = translate_arith_term ~g_vars ?l_vars ~bound in
  let var x = T.t_var (resolve ~g_vars ?l_vars ~bound x) in
  e |> function
  | Int v -> T.t_nat_const v
  | Var x -> var x
  | Plus (e0, e1) -> Arith.plus (f e0) (f e1)
  | Sub (e0, e1) -> Arith.sub (f e0) (f e1)
  | Mul (e0, e1) -> Arith.mul (f e0) (f e1)
  | Div (e0, e1) -> Arith.div (f e0) (f e1)
  | Mod (e0, e1) -> Arith.modulo (f e0) (f e1)
  | Get (a, i) -> Arith.aget (var a) (f i)
  | Len a -> var (Vars.len_key a)

let rec translate_term ~g_vars ?l_vars ?(bound = Vars.empty) e =
  let f_t = translate_term ~g_vars ?l_vars ~bound in
  let f_a = translate_arith_term ~g_vars ?l_vars ~bound in
  (* Bind a fresh quantifier variable [x] and translate [body] under it. *)
  let quant close x body =
    let vs = Vars.create_fresh x in
    close [ vs ] []
      (translate_term ~g_vars ?l_vars ~bound:(Vars.add x vs bound) body)
  in
  (* Whether a translated term is boolean-sorted (a boolean variable), so [=]/
     [!=] over it use Why3's polymorphic equality rather than the integer
     comparison predicate. *)
  let is_bool_term t =
    match t.T.t_ty with
    | Some ty -> Why3.Ty.ty_equal ty Why3.Ty.ty_bool
    | None -> false
  in
  e |> function
  | Bool b -> if b then T.t_true else T.t_false
  (* A boolean program variable asserted as a proposition: [v = True]. *)
  | BoolVar x ->
      T.t_equ (T.t_var (resolve ~g_vars ?l_vars ~bound x)) T.t_bool_true
  | Not e -> T.t_not (f_t e)
  | And (e0, e1) -> T.t_and (f_t e0) (f_t e1)
  | Or (e0, e1) -> T.t_or (f_t e0) (f_t e1)
  | Impl (e0, e1) -> T.t_implies (f_t e0) (f_t e1)
  | Eq (e0, e1) ->
      let a = f_a e0 and b = f_a e1 in
      if is_bool_term a then T.t_equ a b else Arith.eq a b
  | Neq (e0, e1) ->
      let a = f_a e0 and b = f_a e1 in
      if is_bool_term a then T.t_not (T.t_equ a b) else Arith.neq a b
  | Lt (e0, e1) -> Arith.lt (f_a e0) (f_a e1)
  | Leq (e0, e1) -> Arith.leq (f_a e0) (f_a e1)
  | Gt (e0, e1) -> Arith.gt (f_a e0) (f_a e1)
  | Geq (e0, e1) -> Arith.geq (f_a e0) (f_a e1)
  | Forall (x, body) -> quant T.t_forall_close x body
  | Exists (x, body) -> quant T.t_exists_close x body

let fmt = Format.formatter_of_out_channel stdout

let print_expr p =
  Core.Sexp.pp_hum fmt (sexp_of_logic_expr p);
  Format.pp_print_flush fmt ()

let print_term t =
  Why3.Pretty.print_term fmt t;
  Format.pp_print_flush fmt ()
