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

(* An array-valued variable ([map int int]); its element store. *)
let create_fresh_array x =
  Why3.Term.create_vsymbol (Why3.Ident.id_fresh x) Arith.ty_int_map

(* A fresh variable of the *same type* as [vs]. Havoc'ing a variable must
   introduce a replacement of matching type, so an array (map) havocs to a fresh
   map, not a fresh integer. *)
let fresh_like (vs : T.vsymbol) =
  Why3.(Term.create_vsymbol (Ident.id_fresh "y") vs.T.vs_ty)

(* Key under which an array's length is stored, kept alongside the element map.
   [#] cannot appear in a source identifier ([a-zA-Z_]+), so this never collides
   with a user variable. *)
let len_key x = "#len#" ^ x
let is_len_key s = String.starts_with ~prefix:"#len#" s

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
