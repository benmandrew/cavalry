(** The variable environment: a map from source names to the Why3 [vsymbol]s
    that stand for them in translated terms. {!Var_collection} builds these (one
    per procedure, one for [main]'s globals); {!Logic} and [Hoare] look names up
    in them. *)

module T = Why3.Term

(* module M : sig
     type 'a t
   end

   type t = T.vsymbol M.t *)

type t
(** A name-to-[vsymbol] environment. *)

val empty : t

val find : string -> t -> T.vsymbol
(** Look a name up in the environment. Raises the internal [Var_not_found] if it
    is unbound; use {!find_opt} to handle absence. *)

val find_opt : string -> t -> T.vsymbol option

val find_fallback : string -> t -> t -> T.vsymbol
(** [find_fallback s a b] looks [s] up in [a], falling back to [b]. Used to
    resolve a name against procedure-locals first, then globals. *)

val add : string -> T.vsymbol -> t -> t
val bindings : t -> (string * T.vsymbol) list

val union : t -> t -> t
(** In [union a b], if there are conflicts between keys we prefer the values
    from [a] *)

val fold : (string -> T.vsymbol -> 'a -> 'a) -> t -> 'a -> 'a

val create_fresh : string -> Why3.Term.vsymbol
(** A fresh integer-valued ([int]) [vsymbol] with a unique name derived from the
    argument. *)

val create_fresh_bool : string -> Why3.Term.vsymbol
(** A fresh boolean-valued ([bool]) [vsymbol]. *)

val create_fresh_array : string -> Why3.Term.vsymbol
(** A fresh array-valued ([map int int]) variable. *)

val create_fresh_bool_array : string -> Why3.Term.vsymbol
(** A fresh boolean-array ([map int bool]) variable. *)

val fresh_like : T.vsymbol -> Why3.Term.vsymbol
(** A fresh variable of the same type as the argument, for havoc'ing a variable
    without assuming it is an integer. *)

val len_key : string -> string
(** [len_key a] is the internal key under which array [a]'s length is stored. It
    cannot clash with a source identifier. *)

val is_len_key : string -> bool
(** Whether a name is a {!len_key} (used to hide it from user-facing output). *)
