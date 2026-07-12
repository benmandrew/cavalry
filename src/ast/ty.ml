(* The types a Cavalry expression or variable can have. Scalars are [Int] and
   [Bool]; [Array] is a bounded integer array (its element type is always [Int]
   for now, but the constructor is kept general so a later element-typed array
   needs no change here). This is the vocabulary the {!Typecheck} pass reasons
   in; the runtime representation is still integer-only (see [Program.value]),
   so a well-typed program is additionally required not to store booleans -- a
   restriction {!Typecheck} enforces until first-class boolean variables land. *)
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
