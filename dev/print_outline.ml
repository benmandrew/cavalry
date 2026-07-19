(* Dev harness (Phase 1 spike): print the WLP proof outline of a [.cav] file to
   stdout. For each procedure it shows, per statement boundary, the propagated
   assertion with the safety side-obligations stripped (what [proof_outline]
   returns) and, for comparison, the raw assertion with them still glued in.

     dune exec -- dev/print_outline.exe <file.cav> [--machine-int]  *)

open Cavalry

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: print_outline <file.cav> [--machine-int]";
    exit 2);
  let path = Sys.argv.(1) in
  let machine_int = Array.exists (String.equal "--machine-int") Sys.argv in
  let ast = Main.get_ast path in
  let outline = Hoare.proof_outline_debug ~machine_int ast in
  Printf.printf "########## %s%s ##########\n" path
    (if machine_int then " (machine-int)" else "");
  List.iter
    (fun (name, rows) ->
      Printf.printf "\n===== procedure %s =====\n" name;
      List.iter
        (fun (loc, raw, stripped) ->
          let l =
            match loc with
            | Some (l : Ast.Loc.t) -> Printf.sprintf "%d:%d" l.line l.col
            | None -> "?"
          in
          Printf.printf "  [%-7s]\n" l;
          Printf.printf "    stripped: %s\n" stripped;
          Printf.printf "    raw:      %s\n" raw)
        rows)
    outline
