open Core
open Cavalry.Main

let%test_unit "Main.exec" =
  let result = exec "test.cvl" in
  [%test_result: int] result ~expect:182
