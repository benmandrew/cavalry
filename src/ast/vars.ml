module T = Why3.Term
module M = Map.Make (String)

type t = T.vsymbol M.t

let find s (m : t) = M.find s m
let add x s (m : t) = M.add x s m
let empty : t = M.empty
let bindings (m : t) = M.bindings m
