(* M0 -- de-risk the js_of_ocaml link.

   Runs the *entire* OCaml verification pipeline (parse -> typecheck -> translate
   -> variable collection -> weakest-liberal-precondition -> Why3 task -> SMT-LIB2
   printing) on a hardcoded program and dumps the SMT-LIB2 that a Z3-wasm worker
   would solve. No prover is spawned. If this runs as JavaScript and prints
   SMT-LIB2, the make-or-break question -- does Why3 + Core + zarith run under
   js_of_ocaml at all -- is answered yes. *)

(* A provable program (postcondition follows) and an unprovable one (it does
   not). Z3 should return [unsat] on the first and [sat] on the second, proving
   the printed SMT-LIB2 both is well-formed and discriminates. *)
let programs =
  [
    ("valid", {|{ x >= 0 }
y := x + 1
{ y > x }|});
    ("invalid", {|{ x >= 0 }
y := x + 1
{ y > x + 5 }|});
    ( "euclidean",
      {|procedure euclidean_div () =
  requires { x >= 0 }
  ensures { x = q * y + r && 0 <= r && r < y }
  writes { q, r }
  q := 0;
  r := x;
  while r >= y do
    invariant { x = q * y + r && 0 <= r }
    r := r - y;
    q := q + 1
  end
end

{ true }
x := 42;
y := 17;
q := 0;
r := 0;
euclidean_div()
{ q = 2 && r = 8 }|}
    );
  ]

let () =
  List.iter
    (fun (label, program) ->
      let ast = Cavalry.Main.get_ast_string ~fname:"m0.cav" program in
      let procs = Cavalry.Main.obligations_smtlib ast in
      List.iter
        (fun (name, obligations) ->
          let name = if String.equal name "" then "<main>" else name in
          List.iteri
            (fun i (expl, _loc, smtlib, _ce) ->
              Printf.printf "@@@ %s / %s / obligation %d: %s\n%s\n@@@END\n"
                label name i expl smtlib)
            obligations)
        procs)
    programs
