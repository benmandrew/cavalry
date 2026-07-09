open Core
open Ast
open Ast.Program

let g_vars =
  List.fold [ "x"; "y"; "z" ] ~init:Vars.empty ~f:(fun vs x ->
      Vars.add x (Vars.create_fresh x) vs)

let term_to_string t = Stdlib.Format.asprintf "%a" Why3.Pretty.print_term t

let is_true e =
  Why3.Term.t_equal (Cavalry.Hoare.safe ~g_vars e) Why3.Term.t_true

(* One machine bound appears per emitted [in_bounds], so counting occurrences of
   max_int's magnitude counts the arithmetic results [safe] guarded. *)
let bound = "4611686018427387903"

let bound_count e =
  term_to_string (Cavalry.Hoare.safe ~g_vars e)
  |> String.substr_index_all ~may_overlap:false ~pattern:bound
  |> List.length

let var x = Value (VarInst x)
let int n = Value (Int n)

let%test_unit "safe - literals, variables and comparisons carry no obligation" =
  [%test_result: bool] ~expect:true (is_true (int 5));
  [%test_result: bool] ~expect:true (is_true (var "x"));
  [%test_result: bool] ~expect:true (is_true (Lt (var "x", var "y")))

let%test_unit "safe - one bound per arithmetic result" =
  (* x + y : one result *)
  [%test_result: int] ~expect:1 (bound_count (Plus (var "x", var "y")));
  (* x - y : subtraction is guarded too (underflow) *)
  [%test_result: int] ~expect:1 (bound_count (Sub (var "x", var "y")));
  (* (x + y) * z : two results, both guarded *)
  [%test_result: int] ~expect:2
    (bound_count (Mul (Plus (var "x", var "y"), var "z")));
  (* a comparison adds no bound of its own, only its operands' *)
  [%test_result: int] ~expect:1
    (bound_count (Lt (Plus (var "x", var "y"), var "z")))
