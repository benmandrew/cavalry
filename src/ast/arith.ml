open Why3
module T = Term

let int_theory : Theory.theory = Env.read_theory Smt.Prover.env [ "int" ] "Int"

(* Truncated (round-towards-zero) division and remainder, matching OCaml's
   native [/] and [mod], the interpreter ([Ast.Runtime]), and both compile
   backends ([Z.div]/[Z.rem] and [( / )]/[( mod )]). Euclidean division would
   disagree on negative operands, so we deliberately use [ComputerDivision]. *)
let comp_div_theory : Theory.theory =
  Env.read_theory Smt.Prover.env [ "int" ] "ComputerDivision"

let find_symbol theory s = Theory.ns_find_ls theory.Theory.th_export [ s ]
let plus_symbol = find_symbol int_theory "infix +"
let sub_symbol = find_symbol int_theory "infix -"
let mul_symbol = find_symbol int_theory "infix *"
let div_symbol = find_symbol comp_div_theory "div"
let mod_symbol = find_symbol comp_div_theory "mod"

(* Arrays are Why3 [map.Map]s ([map int int]) for their elements, with each
   array's length tracked separately as an ordinary integer variable (see
   [Vars]). [map.Const] supplies the all-zeros map an [array(n)] starts from.
   Element updates ([set]) leave the length variable untouched, so length is
   preserved across writes for free. *)
let map_theory : Theory.theory = Env.read_theory Smt.Prover.env [ "map" ] "Map"

let const_theory : Theory.theory =
  Env.read_theory Smt.Prover.env [ "map" ] "Const"

let map_ts = Theory.ns_find_ts map_theory.Theory.th_export [ "map" ]
let ty_int_map = Ty.ty_app map_ts [ Ty.ty_int; Ty.ty_int ]
let get_symbol = find_symbol map_theory "get"
let set_symbol = find_symbol map_theory "set"
let const_symbol = find_symbol const_theory "const"
let aget a i = T.t_app get_symbol [ a; i ] (Some Ty.ty_int)
let aset a i v = T.t_app set_symbol [ a; i; v ] (Some ty_int_map)
let azero = T.t_app const_symbol [ T.t_nat_const 0 ] (Some ty_int_map)
let eq_symbol = find_symbol int_theory "infix ="
let lt_symbol = find_symbol int_theory "infix <"
let leq_symbol = find_symbol int_theory "infix <="
let gt_symbol = find_symbol int_theory "infix >"
let geq_symbol = find_symbol int_theory "infix >="
let plus a b = T.t_app plus_symbol [ a; b ] (Some Ty.ty_int)
let sub a b = T.t_app sub_symbol [ a; b ] (Some Ty.ty_int)
let mul a b = T.t_app mul_symbol [ a; b ] (Some Ty.ty_int)
let div a b = T.t_app div_symbol [ a; b ] (Some Ty.ty_int)
let modulo a b = T.t_app mod_symbol [ a; b ] (Some Ty.ty_int)
let eq a b = T.ps_app eq_symbol [ a; b ]
let neq a b = T.t_not (eq a b)
let lt a b = T.ps_app lt_symbol [ a; b ]
let leq a b = T.ps_app leq_symbol [ a; b ]
let gt a b = T.ps_app gt_symbol [ a; b ]
let geq a b = T.ps_app geq_symbol [ a; b ]

(* Machine-integer range. OCaml native [int] is 63-bit ([Sys.int_size = 63]),
   so a representable value lies in [\[-2^62, 2^62-1\]] -- the same bound the
   interpreter enforces in [Ast.Runtime.{add,sub,mul}_ovf]. These magnitudes
   exceed [max_int] on a 63-bit host (2^62 does not fit in an OCaml [int]), so
   they are built as Why3 big-integer constants rather than literals. [min_int]
   is expressed as [0 - 2^62] to avoid relying on negative-literal construction. *)
let big s = T.t_int_const (BigInt.of_string s)
let max_int = big "4611686018427387903" (* 2^62 - 1 *)
let min_int = sub (T.t_nat_const 0) (big "4611686018427387904") (* -(2^62) *)

(* [in_bounds t] asserts that [t] is a representable machine integer, i.e.
   [min_int <= t <= max_int]. This is the per-operation overflow-freedom
   obligation the WLP conjoins for each arithmetic result. *)
let in_bounds t = T.t_and (leq min_int t) (leq t max_int)

let base_task =
  let task = Task.add_ty_decl None Ty.ts_int in
  List.fold_left
    (fun task theory -> Task.use_export task theory)
    task [ int_theory ]

(* [ComputerDivision]'s axioms are quantified. The former Alt-Ergo backend's
   matching loop could diverge on them (ignoring the time limit) even for goals
   that never mention [div]/[mod], so they are kept out of the shared [base_task]
   and spliced in only for goals that actually use division (see [task_for]).
   The split is retained under Z3: it costs nothing, and the per-obligation
   proving in [Hoare] already picks a minimal task per subgoal. *)
let base_task_div = Task.use_export base_task comp_div_theory

let base_task_map =
  List.fold_left Task.use_export base_task [ map_theory; const_theory ]

(* Whether [t] applies any of [syms] anywhere in its tree. *)
let uses_any (syms : Term.lsymbol list) (t : T.term) : bool =
  let rec check acc (t : T.term) =
    let acc =
      match t.T.t_node with
      | T.Tapp (ls, _) -> acc || List.exists (T.ls_equal ls) syms
      | _ -> acc
    in
    T.t_fold check acc t
  in
  check false t

let uses_div (t : T.term) : bool = uses_any [ div_symbol; mod_symbol ] t

let uses_map (t : T.term) : bool =
  uses_any [ get_symbol; set_symbol; const_symbol ] t

(* The base task extended with [ComputerDivision] and/or the map theories,
   included only when actually needed. Withholding unused theories keeps
   Alt-Ergo from looping on axioms it does not need (see [base_task_div]). The
   map theories must also be present whenever a [map]-typed *variable* occurs --
   even with no [get]/[set] applied -- because [map] unfolds to the arrow type,
   which the task must declare. *)
let task ~div ~map : Task.task =
  match (div, map) with
  | false, false -> base_task
  | true, false -> base_task_div
  | false, true -> base_task_map
  | true, true -> Task.use_export base_task_map comp_div_theory

let task_for (t : T.term) : Task.task = task ~div:(uses_div t) ~map:(uses_map t)
