open Why3
module T = Term

(* Operator binding strength, lowest to highest, mirroring the Cavalry grammar
   (`src/ast/parse/tokens.mly`): a quantifier binds looser than everything, then
   [->]/[<->], [||], [&&], [!], the comparisons, and finally the arithmetic
   operators. A subterm is parenthesised when its own operator binds looser than
   the context it sits in. [->] etc. are printed left-associatively, matching the
   parser's [%left]. *)
let p_quant = 0
let p_impl = 1
let p_or = 2
let p_and = 3
let p_not = 4
let p_cmp = 5
let p_add = 6
let p_sub = 7
let p_mul = 8
let p_atom = 9
let is_bool_true t = T.t_equal t T.t_bool_true
let is_bool_false t = T.t_equal t T.t_bool_false

(* A fresh identifier printer per {!to_string} call, so the many WLP-introduced
   havoc variables -- all created from the base name ["y"] and thus sharing an
   [id_string] -- render as distinct [y], [y1], [y2], ... . The bare [id_string]
   would collapse them all to [y] and make the output unreadable. *)
let printer = ref (Why3.Ident.create_ident_printer [])

(* A source variable's display name. Two internal naming conventions are undone
   here: [#len#a] (an array's tracked length, see {!Vars.len_key}) prints as
   [len(a)], and an [_x] snapshot that survived {!Wlp.sub_old_vars} prints as
   [old(x)] -- both name distinct globals, so they need no disambiguation.
   Everything else is disambiguated through the shared {!printer}. *)
let var_name (vs : T.vsymbol) =
  let n = vs.T.vs_name.Ident.id_string in
  if Vars.is_len_key n then "len(" ^ String.sub n 5 (String.length n - 5) ^ ")"
  else if String.length n > 0 && Char.equal n.[0] '_' then
    "old(" ^ String.sub n 1 (String.length n - 1) ^ ")"
  else Why3.Ident.id_unique !printer vs.T.vs_name

let const_string = function
  | Constant.ConstInt ic -> BigInt.to_string ic.Number.il_int
  | c -> Format.asprintf "%a" Constant.print_def c

let rec render (t : T.term) : int * string =
  match t.T.t_node with
  | _ when is_bool_true t -> (p_atom, "true")
  | _ when is_bool_false t -> (p_atom, "false")
  | T.Tvar vs -> (p_atom, var_name vs)
  | T.Tconst c -> (p_atom, const_string c)
  | T.Ttrue -> (p_atom, "true")
  | T.Tfalse -> (p_atom, "false")
  | T.Tapp (ls, args) -> render_app ls args
  | T.Tif (c, a, b) when is_bool_true a && is_bool_false b ->
      (* Boolean coercion [if b then True else False] -- the encoding a boolean
         right-hand side takes to become a [bool] term -- is just [b]. *)
      render c
  | T.Tif (c, a, b) ->
      ( p_quant,
        Printf.sprintf "if %s then %s else %s" (pp p_quant c) (pp p_quant a)
          (pp p_quant b) )
  | T.Tbinop (op, a, b) ->
      let sym, p =
        match op with
        | T.Tand -> ("&&", p_and)
        | T.Tor -> ("||", p_or)
        | T.Timplies -> ("->", p_impl)
        | T.Tiff -> ("<->", p_impl)
      in
      (p, Printf.sprintf "%s %s %s" (pp p a) sym (pp (p + 1) b))
  | T.Tnot a -> render_not a
  | T.Tquant (q, tq) ->
      let vs, _, body = T.t_open_quant tq in
      let kw = match q with T.Tforall -> "forall" | T.Texists -> "exists" in
      let names = String.concat ", " (List.map var_name vs) in
      (p_quant, Printf.sprintf "%s %s. %s" kw names (pp p_quant body))
  (* [Tlet]/[Tcase]/[Teps] have no Cavalry surface form and should not appear in
     a WLP assertion; fall back to Why3's printer, parenthesised. *)
  | _ -> (p_atom, "(" ^ Format.asprintf "%a" Pretty.print_term t ^ ")")

and render_app ls args =
  match args with
  | [ a; b ] when Arith.is_plus ls -> binop p_add "+" a b
  | [ a; b ] when Arith.is_sub ls -> binop p_sub "-" a b
  | [ a; b ] when Arith.is_mul ls -> binop p_mul "*" a b
  | [ a; b ] when Arith.is_div ls -> binop p_mul "/" a b
  | [ a; b ] when Arith.is_mod ls -> binop p_mul "%" a b
  | [ a; b ] when Arith.is_lt ls -> binop p_cmp "<" a b
  | [ a; b ] when Arith.is_leq ls -> binop p_cmp "<=" a b
  | [ a; b ] when Arith.is_gt ls -> binop p_cmp ">" a b
  | [ a; b ] when Arith.is_geq ls -> binop p_cmp ">=" a b
  | [ a; b ] when Arith.is_eq ls || T.ls_equal ls T.ps_equ ->
      (* Equality against a [bool] literal is a coercion, not a comparison:
         [x = True] asserts [x], [x = False] asserts [!x]. *)
      if is_bool_true b then render a
      else if is_bool_true a then render b
      else if is_bool_false b then (p_not, "!" ^ pp p_atom a)
      else if is_bool_false a then (p_not, "!" ^ pp p_atom b)
      else binop p_cmp "=" a b
  | [ a; i ] when Arith.is_get ls ->
      (p_atom, Printf.sprintf "%s[%s]" (pp p_atom a) (pp p_quant i))
  | [ a; i; v ] when Arith.is_set ls ->
      ( p_atom,
        Printf.sprintf "%s[%s := %s]" (pp p_atom a) (pp p_quant i)
          (pp p_quant v) )
  | [ x ] when Arith.is_const ls ->
      (p_atom, Printf.sprintf "const(%s)" (pp p_quant x))
  | [] -> (p_atom, ls.T.ls_name.Ident.id_string)
  | _ ->
      ( p_atom,
        Printf.sprintf "%s(%s)" ls.T.ls_name.Ident.id_string
          (String.concat ", " (List.map (pp p_quant) args)) )

(* [!] of an equality reads better as [!=]; otherwise negate the operand,
   parenthesising anything that is not already atomic. *)
and render_not a =
  match a.T.t_node with
  | T.Tapp (ls, [ x; y ]) when Arith.is_eq ls || T.ls_equal ls T.ps_equ ->
      if is_bool_true y then (p_not, "!" ^ pp p_atom x)
      else if is_bool_true x then (p_not, "!" ^ pp p_atom y)
      else
        (p_cmp, Printf.sprintf "%s != %s" (pp (p_cmp + 1) x) (pp (p_cmp + 1) y))
  | _ -> (p_not, "!" ^ pp p_atom a)

and binop p sym a b = (p, Printf.sprintf "%s %s %s" (pp p a) sym (pp (p + 1) b))

(* Render [t] in a context that binds with strength [ctx], adding parentheses
   when [t]'s own operator binds looser. *)
and pp ctx t =
  let p, s = render t in
  if p < ctx then "(" ^ s ^ ")" else s

(* One-point rule: [forall y. y = t -> P] is logically equivalent to [P[y := t]]
   whenever [y] does not occur in [t]. The WLP encodes an assignment [x := e] as
   exactly this shape ([forall y. y = e -> q[x := y]]), and a loop variant's
   measure snapshot likewise, so eliminating it recovers the substituted
   assertion a reader expects in a proof outline ([q[x := e]]) instead of a nest
   of quantifiers. A genuine loop-havoc quantifier binds several variables with
   no [y = t] guard, so it never matches and is left intact -- a loop *should*
   show its quantifier. Applied bottom-up, re-simplifying after each rewrite so
   chained assignments collapse. *)
let rec simplify (t : T.term) : T.term =
  let t = T.t_map simplify t in
  match t.T.t_node with
  | T.Tquant (_, tq) when vacuous tq ->
      (* A quantifier binding no variable that occurs in its body (e.g. the
         [forall y. true] a trivial assignment leaves behind) is just the body. *)
      let _, _, body = T.t_open_quant tq in
      body
  | T.Tquant (T.Tforall, tq) -> (
      let vs, _, body = T.t_open_quant tq in
      match (vs, body.T.t_node) with
      | [ y ], T.Tbinop (T.Timplies, cond, rest) -> (
          match one_point y cond rest with Some t' -> simplify t' | None -> t)
      | _ -> t)
  | _ -> t

(* Whether no variable [tq] binds occurs free in its body. *)
and vacuous tq =
  let vs, _, body = T.t_open_quant tq in
  let fv = T.t_vars body in
  not (List.exists (fun v -> T.Mvs.mem v fv) vs)

(* If [cond] is [y = t] (either orientation) with [y] absent from [t], return
   [rest] with [t] substituted for [y]; otherwise [None]. *)
and one_point y cond rest =
  match cond.T.t_node with
  | T.Tapp (ls, [ a; b ]) when Arith.is_eq ls || T.ls_equal ls T.ps_equ ->
      let is_y u =
        match u.T.t_node with T.Tvar v -> T.vs_equal v y | _ -> false
      in
      let absent u = not (T.Mvs.mem y (T.t_vars u)) in
      if is_y a && absent b then Some (T.t_subst_single y b rest)
      else if is_y b && absent a then Some (T.t_subst_single y a rest)
      else None
  | _ -> None

let to_string t =
  let p = Why3.Ident.create_ident_printer [] in
  printer := p;
  (* Reserve the real names of the free (source) variables first, so a program
     variable literally named [y] keeps its name and only the WLP's *bound*
     havoc variables (also based on ["y"]) are pushed to [y1], [y2], ... . *)
  T.t_v_fold (fun () vs -> ignore (Why3.Ident.id_unique p vs.T.vs_name)) () t;
  snd (render t)
