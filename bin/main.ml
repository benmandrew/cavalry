open Cavalry

let exec source_file = Printf.printf "%d\n" (Main.exec source_file)

let verify debug machine_int source_file =
  let program = Main.get_ast source_file in
  let open Smt.Prover in
  match Main.verify_report ~debug ~machine_int program with
  | { result = Valid; _ } -> Printf.printf "verification successful\n"
  | { result = Invalid; failing_proc } ->
      let where =
        match failing_proc with
        | Some p -> Printf.sprintf " in procedure '%s'" p
        | None -> ""
      in
      Printf.printf
        "verification unsuccessful%s: precondition does not imply postcondition\n"
        where
  | { result = Failed s; _ } -> Printf.printf "internal failure: %s\n" s

let compile debug no_verify native_int output source_file =
  let output =
    match output with
    | Some o -> o
    | None -> Filename.remove_extension (Filename.basename source_file)
  in
  match
    Main.compile ~debug ~verify:(not no_verify) ~native_int ~output source_file
  with
  | () -> Printf.printf "compiled to %s\n" output
  | exception Main.Verification_failed msg ->
      Printf.eprintf "refusing to compile: verification failed: %s\n" msg;
      exit 1
  | exception Compile.Unsupported what ->
      Printf.eprintf "cannot compile: %s is not supported\n" what;
      exit 1
  | exception Compile.Toolchain_error msg ->
      Printf.eprintf "%s\n" msg;
      exit 1

(* Command-line parsing *)
open Cmdliner

let source_file =
  Arg.required
  @@ Arg.pos 0 Arg.(some file) None
  @@ Arg.info ~doc:"The input cavalry source file" ~docv:"SOURCE" []

let debug =
  Arg.value @@ Arg.flag
  @@ Arg.info ~doc:"Emit debug information" ~docv:"DEBUG" [ "d"; "debug" ]

let machine_int =
  Arg.value @@ Arg.flag
  @@ Arg.info
       ~doc:
         "Verify against OCaml's 63-bit machine integers: every arithmetic \
          operation must be proven not to overflow. The default reasons over \
          unbounded integers."
       [ "machine-int" ]

let run_cmd =
  let doc = "Run a Cavalry program in an interpreter" in
  let info = Cmd.info "run" ~doc in
  let cmd_t = Term.(const exec $ source_file) in
  Cmd.v info cmd_t

let verify_cmd =
  let doc = "Verify that a Cavalry program satisfy its specification" in
  let info = Cmd.info "verify" ~doc in
  let cmd_t = Term.(const verify $ debug $ machine_int $ source_file) in
  Cmd.v info cmd_t

let output =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info
       ~doc:"Output executable path (default: source basename without .cav)"
       ~docv:"OUTPUT" [ "o"; "output" ]

let no_verify =
  Arg.value @@ Arg.flag
  @@ Arg.info
       ~doc:
         "Skip the verification gate and compile even if the program does not \
          verify"
       [ "no-verify" ]

let native_int =
  Arg.value @@ Arg.flag
  @@ Arg.info
       ~doc:
         "Compile to 63-bit machine int instead of unbounded integers. The \
          verification gate then proves overflow-freedom, so the (faster) \
          binary is still sound; --no-verify skips that check and native-int \
          arithmetic may then wrap."
       [ "native-int" ]

let compile_cmd =
  let doc = "Compile a Cavalry program to a native executable" in
  let info = Cmd.info "compile" ~doc in
  let cmd_t =
    Term.(const compile $ debug $ no_verify $ native_int $ output $ source_file)
  in
  Cmd.v info cmd_t

let () =
  let open Cmd in
  let doc = "Run, verify and compile Cavalry programs" in
  let info = info "cav" ~doc in
  exit @@ eval @@ group info [ verify_cmd; run_cmd; compile_cmd ]
