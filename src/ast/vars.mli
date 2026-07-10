module T = Why3.Term

(* module M : sig
     type 'a t
   end

   type t = T.vsymbol M.t *)

type t

val empty : t
val find : string -> t -> T.vsymbol
val find_opt : string -> t -> T.vsymbol option
val find_fallback : string -> t -> t -> T.vsymbol
val add : string -> T.vsymbol -> t -> t
val bindings : t -> (string * T.vsymbol) list

val union : t -> t -> t
(** In [union a b], if there are conflicts between keys we prefer the values
    from [a] *)

val fold : (string -> T.vsymbol -> 'a -> 'a) -> t -> 'a -> 'a
val create_fresh : string -> Why3.Term.vsymbol

val create_fresh_array : string -> Why3.Term.vsymbol
(** A fresh array-valued ([map int int]) variable. *)

val fresh_like : T.vsymbol -> Why3.Term.vsymbol
(** A fresh variable of the same type as the argument, for havoc'ing a variable
    without assuming it is an integer. *)

val len_key : string -> string
(** [len_key a] is the internal key under which array [a]'s length is stored. It
    cannot clash with a source identifier. *)
