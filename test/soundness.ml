open Core
open Cavalry
open Ast

(* Run a program under the interpreter's overflow-checking, fuel-bounded
   evaluator (the same oracle the soundness fuzzer uses). It reports a detected
   63-bit overflow as [OutOfFuel]; these fixtures are loop-free, so [OutOfFuel]
   can only mean overflow, not fuel exhaustion. *)
let run_env path =
  Main.get_ast path
  |> List.map ~f:(fun p -> fst p |> Runtime.to_proc_t)
  |> Runtime.exec_env

let overflowed = function Runtime.OutOfFuel -> true | _ -> false
let terminated = function Runtime.Terminated _ -> true | _ -> false

let mverify path =
  Hoare.verify ~machine_int:true ?timeout:(Some 5.) (Main.get_ast path)

let certified = function
  | Smt.Prover.Valid -> true
  | Invalid | Failed _ -> false

(* Cross-check the verifier against the interpreter's runtime overflow oracle:
   a program that actually overflows a 63-bit integer at runtime must NOT be
   certified under machine-integer verification, and one that runs cleanly may.
   This couples the two implementations, so a hole in the WLP's overflow
   obligations would surface here as "overflows at runtime, yet verifies". *)
let%test_unit "soundness: a run that overflows is not certified" =
  [%test_result: bool] ~expect:true
    (overflowed (run_env "overflow_concrete.cav"));
  [%test_result: bool] ~expect:false
    (certified (mverify "overflow_concrete.cav"))

let%test_unit "soundness: a clean run is certified" =
  [%test_result: bool] ~expect:true
    (terminated (run_env "no_overflow_concrete.cav"));
  [%test_result: bool] ~expect:true
    (certified (mverify "no_overflow_concrete.cav"))
