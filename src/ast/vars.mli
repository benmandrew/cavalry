module T = Why3.Term

(* module M : sig
     type 'a t
   end

   type t = T.vsymbol M.t *)

type t

val empty : t
val find : string -> t -> T.vsymbol
val add : string -> T.vsymbol -> t -> t
val bindings : t -> (string * T.vsymbol) list
val create_fresh : string -> Why3.Term.vsymbol
