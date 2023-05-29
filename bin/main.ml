open Cavalry

let exec source_file = Printf.printf "%d\n" (Main.exec source_file)

let verify debug source_file =
  let program = Main.get_ast source_file in
  let open Smt.Prover in
  match Main.verify ~debug program with
  | Valid -> Printf.printf "verification successful\n"
  | Invalid ->
      Printf.printf
        "verification unsuccessful: precondition does not imply postcondition\n"
  | Failed s -> Printf.printf "internal failure: %s\n" s

(* Command-line parsing *)
open Cmdliner

let source_file =
  Arg.required
  @@ Arg.pos 0 Arg.(some file) None
  @@ Arg.info ~doc:"The input cavalry source file" ~docv:"SOURCE" []

let debug =
  Arg.value @@ Arg.flag
  @@ Arg.info ~doc:"Emit debug information" ~docv:"DEBUG" [ "d"; "debug" ]

let run_cmd =
  let doc = "Run a Cavalry program in an interpreter" in
  let info = Cmd.info "run" ~doc in
  let cmd_t = Term.(const exec $ source_file) in
  Cmd.v info cmd_t

let verify_cmd =
  let doc = "Verify that a Cavalry program satisfy its specification" in
  let info = Cmd.info "verify" ~doc in
  let cmd_t = Term.(const verify $ debug $ source_file) in
  Cmd.v info cmd_t

let () =
  let open Cmd in
  let doc = "Run and verify Cavalry programs" in
  let info = info "cavalry" ~doc in
  exit @@ eval @@ group info [ verify_cmd; run_cmd ]
