open Core
open Ast

let term_to_string t = Stdlib.Format.asprintf "%a" Why3.Pretty.print_term t
let contains ~substring s = String.is_substring s ~substring

(* 63-bit machine range: max_int = 2^62 - 1, min_int = -(2^62). *)
let max_int_str = "4611686018427387903"
let two_pow_62_str = "4611686018427387904"

let%test_unit "Arith.max_int / min_int carry the 63-bit magnitudes" =
  [%test_pred: string]
    (contains ~substring:max_int_str)
    (term_to_string Arith.max_int);
  [%test_pred: string]
    (contains ~substring:two_pow_62_str)
    (term_to_string Arith.min_int)

let%test_unit "Arith.in_bounds - conjunction of both bounds" =
  let s = term_to_string (Arith.in_bounds (Why3.Term.t_nat_const 5)) in
  List.iter [ max_int_str; two_pow_62_str; "<="; "/\\" ] ~f:(fun substring ->
      if not (contains ~substring s) then
        raise_s
          [%message
            "in_bounds term missing expected fragment"
              (substring : string)
              (s : string)])
