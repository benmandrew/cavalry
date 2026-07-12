open Why3
module T = Term

(* Every Why3 theory read below (and, through [Smt.Prover.env], the Z3 driver
   load it forces) is deferred behind a single [lazy] record. OCaml evaluates a
   linked module's top-level bindings at process startup, so reading the
   standard-library theories eagerly made [cav run] -- which never builds a
   proof term -- pay for them. The term combinators force [defs] on first use,
   keeping that cost on the verify/compile paths that actually need it. *)
type defs = {
  int_theory : Theory.theory;
  comp_div_theory : Theory.theory;
  map_theory : Theory.theory;
  const_theory : Theory.theory;
  plus_symbol : Term.lsymbol;
  sub_symbol : Term.lsymbol;
  mul_symbol : Term.lsymbol;
  div_symbol : Term.lsymbol;
  mod_symbol : Term.lsymbol;
  eq_symbol : Term.lsymbol;
  lt_symbol : Term.lsymbol;
  leq_symbol : Term.lsymbol;
  gt_symbol : Term.lsymbol;
  geq_symbol : Term.lsymbol;
  get_symbol : Term.lsymbol;
  set_symbol : Term.lsymbol;
  const_symbol : Term.lsymbol;
  ty_int_map : Ty.ty;
  azero : Term.term;
  ty_int_bool_map : Ty.ty;
  bfalse : Term.term;
  min_int : Term.term;
  max_int : Term.term;
  base_task : Task.task;
  base_task_div : Task.task;
  base_task_map : Task.task;
}

let defs =
  lazy
    (let env = Lazy.force Smt.Prover.env in
     let find_symbol theory s =
       Theory.ns_find_ls theory.Theory.th_export [ s ]
     in
     let int_theory = Env.read_theory env [ "int" ] "Int" in
     (* Truncated (round-towards-zero) division and remainder, matching OCaml's
        native [/] and [mod], the interpreter ([Ast.Runtime]), and both compile
        backends ([Z.div]/[Z.rem] and [( / )]/[( mod )]). Euclidean division
        would disagree on negative operands, so we deliberately use
        [ComputerDivision]. *)
     let comp_div_theory = Env.read_theory env [ "int" ] "ComputerDivision" in
     (* Arrays are Why3 [map.Map]s ([map int int]) for their elements, with each
        array's length tracked separately as an ordinary integer variable (see
        [Vars]). [map.Const] supplies the all-zeros map an [array(n)] starts
        from. Element updates ([set]) leave the length variable untouched, so
        length is preserved across writes for free. *)
     let map_theory = Env.read_theory env [ "map" ] "Map" in
     let const_theory = Env.read_theory env [ "map" ] "Const" in
     let map_ts = Theory.ns_find_ts map_theory.Theory.th_export [ "map" ] in
     let ty_int_map = Ty.ty_app map_ts [ Ty.ty_int; Ty.ty_int ] in
     let get_symbol = find_symbol map_theory "get" in
     let const_symbol = find_symbol const_theory "const" in
     let azero = T.t_app const_symbol [ T.t_nat_const 0 ] (Some ty_int_map) in
     (* Boolean arrays are [map int bool]; an [array(n)] of booleans starts from
        the all-[False] map. The [map.Map]/[map.Const] symbols are polymorphic,
        so the same [get]/[set]/[const] serve both element types. *)
     let ty_int_bool_map = Ty.ty_app map_ts [ Ty.ty_int; Ty.ty_bool ] in
     let bfalse =
       T.t_app const_symbol [ T.t_bool_false ] (Some ty_int_bool_map)
     in
     let sub_symbol = find_symbol int_theory "infix -" in
     let sub a b = T.t_app sub_symbol [ a; b ] (Some Ty.ty_int) in
     (* Machine-integer range. OCaml native [int] is 63-bit ([Sys.int_size =
        63]), so a representable value lies in [\[-2^62, 2^62-1\]] -- the same
        bound the interpreter enforces in [Ast.Runtime.{add,sub,mul}_ovf].
        These magnitudes exceed [max_int] on a 63-bit host (2^62 does not fit in
        an OCaml [int]), so they are built as Why3 big-integer constants rather
        than literals. [min_int] is expressed as [0 - 2^62] to avoid relying on
        negative-literal construction. *)
     let big s = T.t_int_const (BigInt.of_string s) in
     let max_int =
       big "4611686018427387903"
       (* 2^62 - 1 *)
     in
     let min_int = sub (T.t_nat_const 0) (big "4611686018427387904") in
     let base_task =
       let task = Task.add_ty_decl None Ty.ts_int in
       List.fold_left
         (fun task theory -> Task.use_export task theory)
         task [ int_theory ]
     in
     (* [ComputerDivision]'s axioms are quantified. The former Alt-Ergo backend's
        matching loop could diverge on them (ignoring the time limit) even for
        goals that never mention [div]/[mod], so they are kept out of the shared
        [base_task] and spliced in only for goals that actually use division (see
        [task_for]). The split is retained under Z3: it costs nothing, and the
        per-obligation proving in [Hoare] already picks a minimal task per
        subgoal. *)
     let base_task_div = Task.use_export base_task comp_div_theory in
     let base_task_map =
       List.fold_left Task.use_export base_task [ map_theory; const_theory ]
     in
     {
       int_theory;
       comp_div_theory;
       map_theory;
       const_theory;
       plus_symbol = find_symbol int_theory "infix +";
       sub_symbol;
       mul_symbol = find_symbol int_theory "infix *";
       div_symbol = find_symbol comp_div_theory "div";
       mod_symbol = find_symbol comp_div_theory "mod";
       eq_symbol = find_symbol int_theory "infix =";
       lt_symbol = find_symbol int_theory "infix <";
       leq_symbol = find_symbol int_theory "infix <=";
       gt_symbol = find_symbol int_theory "infix >";
       geq_symbol = find_symbol int_theory "infix >=";
       get_symbol;
       set_symbol = find_symbol map_theory "set";
       const_symbol;
       ty_int_map;
       azero;
       ty_int_bool_map;
       bfalse;
       min_int;
       max_int;
       base_task;
       base_task_div;
       base_task_map;
     })

let d () = Lazy.force defs
let plus a b = T.t_app (d ()).plus_symbol [ a; b ] (Some Ty.ty_int)
let sub a b = T.t_app (d ()).sub_symbol [ a; b ] (Some Ty.ty_int)
let mul a b = T.t_app (d ()).mul_symbol [ a; b ] (Some Ty.ty_int)
let div a b = T.t_app (d ()).div_symbol [ a; b ] (Some Ty.ty_int)
let modulo a b = T.t_app (d ()).mod_symbol [ a; b ] (Some Ty.ty_int)
let eq a b = T.ps_app (d ()).eq_symbol [ a; b ]
let neq a b = T.t_not (eq a b)
let lt a b = T.ps_app (d ()).lt_symbol [ a; b ]
let leq a b = T.ps_app (d ()).leq_symbol [ a; b ]
let gt a b = T.ps_app (d ()).gt_symbol [ a; b ]
let geq a b = T.ps_app (d ()).geq_symbol [ a; b ]
let aget a i = T.t_app (d ()).get_symbol [ a; i ] (Some Ty.ty_int)
let aset a i v = T.t_app (d ()).set_symbol [ a; i; v ] (Some (d ()).ty_int_map)

(* The boolean-array counterparts: [a[i]] as a [bool] term and [set a i v] over a
   [map int bool]. *)
let aget_bool a i = T.t_app (d ()).get_symbol [ a; i ] (Some Ty.ty_bool)

let aset_bool a i v =
  T.t_app (d ()).set_symbol [ a; i; v ] (Some (d ()).ty_int_bool_map)

(* Exposed as [Lazy.t] rather than plain values: a consumer that bound one of
   these at its own top level would re-force the whole Why3 layer at startup,
   the very cost the laziness above avoids. Callers force at use. *)
let ty_int_map = lazy (d ()).ty_int_map
let azero = lazy (d ()).azero
let ty_int_bool_map = lazy (d ()).ty_int_bool_map
let bfalse = lazy (d ()).bfalse
let min_int = lazy (d ()).min_int
let max_int = lazy (d ()).max_int

(* [in_bounds t] asserts that [t] is a representable machine integer, i.e.
   [min_int <= t <= max_int]. This is the per-operation overflow-freedom
   obligation the WLP conjoins for each arithmetic result. *)
let in_bounds t =
  T.t_and (leq (Lazy.force min_int) t) (leq t (Lazy.force max_int))

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

let uses_div (t : T.term) : bool =
  uses_any [ (d ()).div_symbol; (d ()).mod_symbol ] t

let uses_map (t : T.term) : bool =
  let d = d () in
  uses_any [ d.get_symbol; d.set_symbol; d.const_symbol ] t

(* The base task extended with [ComputerDivision] and/or the map theories,
   included only when actually needed. Withholding unused theories keeps
   Alt-Ergo from looping on axioms it does not need (see [base_task_div]). The
   map theories must also be present whenever a [map]-typed *variable* occurs --
   even with no [get]/[set] applied -- because [map] unfolds to the arrow type,
   which the task must declare. *)
let task ~div ~map : Task.task =
  let d = d () in
  match (div, map) with
  | false, false -> d.base_task
  | true, false -> d.base_task_div
  | false, true -> d.base_task_map
  | true, true -> Task.use_export d.base_task_map d.comp_div_theory

let task_for (t : T.term) : Task.task = task ~div:(uses_div t) ~map:(uses_map t)
