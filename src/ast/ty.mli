(** Cavalry's types: the vocabulary {!Typecheck} validates a program against.
    [Array] carries an element type (always [Int] today). *)
type t = Int | Bool | Array of t [@@deriving sexp_of, show]

val equal : t -> t -> bool

val to_string : t -> string
(** Surface syntax for a type: [int], [bool], [int[]]. *)
