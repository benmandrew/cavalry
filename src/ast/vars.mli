module T = Why3.Term

(* module M : sig
     type 'a t
   end

   type t = T.vsymbol M.t *)

type t

val empty : t
val find : string -> t -> T.vsymbol
val find_opt : string -> t -> T.vsymbol option
val add : string -> T.vsymbol -> t -> t
val bindings : t -> (string * T.vsymbol) list

val union : t -> t -> t
(** In [union a b], if there are conflicts between keys we prefer the values from [a] *)

val fold : (string -> T.vsymbol -> 'a -> 'a) -> t -> 'a -> 'a
val create_fresh : string -> Why3.Term.vsymbol
