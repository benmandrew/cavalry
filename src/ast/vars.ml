module T = Why3.Term
module M = Map.Make (String)

type t = T.vsymbol M.t

exception Var_not_found of string

let empty : t = M.empty
let find s (m : t) = try M.find s m with Not_found -> raise (Var_not_found s)
let find_opt s (m : t) = M.find_opt s m
let add s x (m : t) = M.add s x m
let bindings (m : t) = M.bindings m
let union (a : t) (b : t) = M.union (fun _ v _ -> Some v) a b
let create_fresh x = Why3.(Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int)
