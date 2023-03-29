module T = Why3.Term
module M = Map.Make (String)

type t = T.vsymbol M.t

let empty : t = M.empty
let find s (m : t) = M.find s m
let find_opt s (m : t) = M.find_opt s m
let add s x (m : t) = M.add s x m
let bindings (m : t) = M.bindings m
let create_fresh x = Why3.(Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int)

let filter_ghost_vars (m : t) =
  M.filter (fun s _ -> String.starts_with ~prefix:"_" s) m
