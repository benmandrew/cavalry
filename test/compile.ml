open Core
open Ast
open Ast.Program

(* Build a [(Triple.t * Vars.t) list] the way [Main.get_ast] would, from a
   hand-written [main] body, and emit OCaml for it. *)
let triple ?(f = "main") ?(ps = []) ?(ws = []) (c : cmd) : Triple.t =
  { p = Logic.Bool true; q = Logic.Bool true; ws; f; ps; c }

let emit_main (c : cmd) : string =
  Var_collection.collect [ triple c ] |> Cavalry.Compile.emit

let contains ~substring s = String.is_substring s ~substring

let%test_unit "Compile.emit - straight-line arithmetic" =
  (* x <- 3; y <- x + 4; print(y); x *)
  let body =
    Seq
      ( Assgn ("x", Value (Int 3)),
        Seq
          ( Assgn ("y", Plus (Value (VarInst "x"), Value (Int 4))),
            Seq (Print (Value (VarInst "y")), IntExpr (Value (VarInst "x"))) )
      )
  in
  let out = emit_main body in
  let want =
    [
      (* assignment targets become prefixed global refs *)
      "let g_x = ref Z.zero";
      "let g_y = ref Z.zero";
      (* unbounded arithmetic, not native + *)
      "Z.add";
      "(Z.of_int (3))";
      "(Z.of_int (4))";
      (* reads of a global are dereferenced *)
      "!g_x";
      (* print keeps the interpreter's decimal-per-line format *)
      "print_string (Z.to_string";
    ]
  in
  List.iter want ~f:(fun substring ->
      if not (contains ~substring out) then
        raise_s
          [%message
            "emitted OCaml missing expected fragment"
              (substring : string)
              (out : string)])

let%test_unit "Compile.emit - result epilogue prints main's value" =
  let body = Seq (Assgn ("x", Value (Int 1)), IntExpr (Value (VarInst "x"))) in
  let out = emit_main body in
  [%test_pred: string] (contains ~substring:"let _result =") out

let%test_unit "Compile.emit - if compiles both branches" =
  (* if x < 5 then x else 0 end *)
  let body =
    If
      ( Lt (Value (VarInst "x"), Value (Int 5)),
        IntExpr (Value (VarInst "x")),
        IntExpr (Value (Int 0)) )
  in
  let out = emit_main body in
  List.iter [ "(if "; "Z.lt"; "then"; "else" ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - while emits a Zarith-guarded loop" =
  (* while i < 10 do invariant { true } i <- i + 1 end *)
  let body =
    While
      ( Logic.Bool true,
        Lt (Value (VarInst "i"), Value (Int 10)),
        Assgn ("i", Plus (Value (VarInst "i"), Value (Int 1))) )
  in
  let out = emit_main body in
  List.iter [ "while "; "Z.lt"; "do"; "done"; "g_i := " ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - procedure: formals are locals, Assgn hits global"
    =
  (* procedure f (a) = ... writes { x } x <- x + a end
     { true } x <- 2; y <- 5; f(y); x { true } *)
  let proc =
    triple ~f:"f" ~ps:[ "a" ] ~ws:[ "x" ]
      (Assgn ("x", Plus (Value (VarInst "x"), Value (VarInst "a"))))
  in
  let main =
    triple
      (Seq
         ( Assgn ("x", Value (Int 2)),
           Seq
             ( Assgn ("y", Value (Int 5)),
               Seq
                 ( Proc ("f", [ Value (VarInst "y") ]),
                   IntExpr (Value (VarInst "x")) ) ) ))
  in
  let out = Var_collection.collect [ proc; main ] |> Cavalry.Compile.emit in
  List.iter
    [
      "let p_f l_a =";
      (* formal [a] read as a local *)
      "l_a";
      (* the [<-] target is the shared global, not the formal *)
      "g_x :=";
      (* call passes the actual through the caller's global [y] *)
      "(p_f (!g_y))";
    ] ~f:(fun substring ->
      if not (contains ~substring out) then
        raise_s
          [%message "missing fragment" (substring : string) (out : string)])

let%test_unit "Compile.emit - zero-arg procedure takes unit" =
  let proc = triple ~f:"f" (Assgn ("y", Value (Int 1))) in
  let main = triple (Proc ("f", [])) in
  let out = Var_collection.collect [ proc; main ] |> Cavalry.Compile.emit in
  List.iter [ "let p_f () ="; "(p_f ())" ] ~f:(fun substring ->
      [%test_pred: string] (contains ~substring) out)

let%test_unit "Compile.emit - arrays: array ref, make, and indexed get/set" =
  (* a <- array(3); a[1] <- 5; len(a) + a[1] *)
  let body =
    Seq
      ( ArrMake ("a", Value (Int 3)),
        Seq
          ( ArrAssgn ("a", Value (Int 1), Value (Int 5)),
            IntExpr (Plus (Len "a", Get ("a", Value (Int 1)))) ) )
  in
  let out = emit_main body in
  List.iter
    [
      (* an array global is an [array ref], not an [int ref] *)
      "let g_a = ref [||]";
      "Array.make";
      (* element access dereferences the ref and indexes with a native int *)
      "(!g_a).(";
      "Z.to_int";
      (* [len] reads [Array.length] back into a program integer *)
      "Array.length !g_a";
      "Z.of_int";
    ] ~f:(fun substring ->
      if not (contains ~substring out) then
        raise_s
          [%message "missing fragment" (substring : string) (out : string)])

(* --- end-to-end: actually build the binary and run it --- *)

(* Compile [fixture] to a native binary (skipping the verification gate, which
   is exercised separately), run it, and return its trimmed stdout. *)
let compile_and_run ?(native_int = false) fixture : string =
  let ocaml =
    Cavalry.Main.get_ast fixture |> Cavalry.Compile.emit ~native_int
  in
  let bin = Stdlib.Filename.temp_file "cav_it_" ".bin" in
  let out = Stdlib.Filename.temp_file "cav_it_" ".out" in
  Cavalry.Compile.to_native ~native_int ~output:bin ocaml;
  let rc =
    Stdlib.Sys.command
      (Printf.sprintf "%s > %s"
         (Stdlib.Filename.quote bin)
         (Stdlib.Filename.quote out))
  in
  let stdout = In_channel.read_all out |> String.strip in
  (try Stdlib.Sys.remove bin with _ -> ());
  (try Stdlib.Sys.remove out with _ -> ());
  if rc <> 0 then failwithf "%s: compiled binary exited %d" fixture rc ();
  stdout

(* The compiled (Zarith) binary must reproduce the interpreter's output. These
   fixtures stay within 63 bits, so all three semantics -- verifier, cav run,
   and the Zarith binary -- agree; see the overflow test for where they part. *)
let%test_unit "Compile e2e - Zarith binary matches interpreter" =
  List.iter
    [
      "exec_negative.cav";
      "verify_true_variable.cav";
      "exec_if.cav";
      "exec_while.cav";
      "exec_nested_while.cav";
      "exec_proc.cav";
      "verify_true_fib_proc.cav";
    ] ~f:(fun fixture ->
      let expect = Int.to_string (Cavalry.Main.exec fixture) in
      [%test_result: string] ~expect (compile_and_run fixture))

(* Arrays compile and run identically to the interpreter under both backends
   (these fixtures stay in bounds and never overflow, so all three agree). This
   covers array creation, element read/write, [len], and -- via
   [exec_array_proc] -- a procedure writing an array global. *)
let%test_unit "Compile e2e - arrays match interpreter, both backends" =
  List.iter [ "exec_array.cav"; "exec_array_len.cav"; "exec_array_proc.cav" ]
    ~f:(fun fixture ->
      let expect = Int.to_string (Cavalry.Main.exec fixture) in
      [%test_result: string] ~expect (compile_and_run fixture);
      [%test_result: string] ~expect (compile_and_run ~native_int:true fixture))

(* On overflow the three semantics diverge, which is the whole reason Zarith is
   the default: the Zarith binary yields the true unbounded value (what the
   verifier reasons about), while --native-int wraps -- and its wrap matches the
   tree-walking interpreter, which also uses native ints. *)
let%test_unit "Compile e2e - Zarith is unbounded, native-int wraps" =
  let fixture = "compile_overflow.cav" in
  let zarith = compile_and_run fixture in
  let native = compile_and_run ~native_int:true fixture in
  let interp = Int.to_string (Cavalry.Main.exec fixture) in
  [%test_result: string] ~expect:"1000000000000000000000000000" zarith;
  [%test_result: string] ~expect:interp native;
  if String.equal zarith native then
    failwithf "backends should diverge on overflow but both gave %s" zarith ()

(* The verification gate refuses to emit a binary for a program that does not
   meet its spec, unless verification is skipped. *)
let%test_unit "Compile - verification gate rejects a false spec" =
  let fixture = "compile_false_spec.cav" in
  (match
     Cavalry.Main.compile ~verify:true ~output:"/dev/null/never" fixture
   with
  | () -> failwith "expected verification to reject the program"
  | exception Cavalry.Main.Verification_failed _ -> ());
  (* with the gate off it compiles and runs the (spec-violating) program *)
  [%test_result: string] ~expect:"1" (compile_and_run ~native_int:false fixture)

(* Native-int codegen is sound because its verification gate proves
   overflow-freedom: a program that can overflow is rejected under --native-int
   (63-bit gate) but accepted under the default Zarith backend (unbounded gate,
   where overflow is impossible). A bounded program compiles either way. *)
let%test_unit "Compile - native-int is gated by machine-int verification" =
  let out = Stdlib.Filename.temp_file "cav_ni_" ".bin" in
  let cleanup () = try Stdlib.Sys.remove out with _ -> () in
  Exn.protect ~finally:cleanup ~f:(fun () ->
      (match
         Cavalry.Main.compile ~native_int:true ~verify:true ~output:out
           "compile_overflow.cav"
       with
      | () -> failwith "native-int gate should reject an overflowing program"
      | exception Cavalry.Main.Verification_failed _ -> ());
      Cavalry.Main.compile ~native_int:false ~verify:true ~output:out
        "compile_overflow.cav";
      Cavalry.Main.compile ~native_int:true ~verify:true ~output:out
        "verify_bounded_succ.cav")
