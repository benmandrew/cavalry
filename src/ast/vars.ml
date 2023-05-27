module T = Why3.Term
module M = Map.Make (String)

type t = T.vsymbol M.t

exception Var_not_found of string

let empty : t = M.empty
let find s (m : t) = try M.find s m with Not_found -> raise (Var_not_found s)
let find_opt = M.find_opt
let add = M.add
let bindings = M.bindings
let union (a : t) (b : t) = M.union (fun _ v _ -> Some v) a b
let fold = M.fold
let create_fresh x = Why3.(Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int)

let find_fallback s m0 m1 =
  match find_opt s m0 with Some v -> v | None -> find s m1

(* type state = {
     globals : t;
     locals : t;
   }

   let find_s s (m : state) =
         match m.locals with
         | Some l_vars -> (
             match Vars.find_opt x g_vars with
             | Some symbol -> T.t_var symbol
             | None -> T.t_var @@ Vars.find x l_vars)
         | None -> T.t_var @@ Vars.find x g_vars) *)
