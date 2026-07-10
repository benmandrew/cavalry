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

(* [ComputerDivision]'s axioms are quantified, and Alt-Ergo's matching loop can
   diverge on them (ignoring the time limit) even for goals that never mention
   [div]/[mod]. So we keep them out of the shared [base_task] and splice them in
   only for goals that actually use division -- see [task_for]. *)
let base_task_div = Task.use_export base_task comp_div_theory

(* Whether [t] applies the truncated [div]/[mod] symbols anywhere in its tree. *)
let uses_div_mod (t : T.term) : bool =
  let rec check acc (t : T.term) =
    let acc =
      match t.T.t_node with
      | T.Tapp (ls, _) ->
          acc || T.ls_equal ls div_symbol || T.ls_equal ls mod_symbol
      | _ -> acc
    in
    T.t_fold check acc t
  in
  check false t

(* Task to discharge [t] against: the plain integer theory, plus
   [ComputerDivision] only when [t] needs its axioms. *)
let task_for (t : T.term) : Task.task =
  if uses_div_mod t then base_task_div else base_task
