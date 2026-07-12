(* The types a Cavalry expression or variable can have. Scalars are [Int] and
   [Bool]; [Array] is a bounded array over an element type ([Int] or [Bool]).
   This is the vocabulary the {!Typecheck} pass reasons in. Booleans are
   first-class (see [Program.value]'s [Bool]/[BoolVar]); the interpreter and
   compiler encode them as 0/1 in the same integer store, while the WLP gives
   them a distinct [bool] Why3 sort. *)
type t = Int | Bool | Array of t [@@deriving sexp_of, show]

let rec equal (a : t) (b : t) =
  match (a, b) with
  | Int, Int | Bool, Bool -> true
  | Array a, Array b -> equal a b
  | _ -> false

let rec to_string = function
  | Int -> "int"
  | Bool -> "bool"
  | Array t -> to_string t ^ "[]"
