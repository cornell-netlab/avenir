open Avenir
open Ast
open Alcotest


(* Testing equality smart constructor *)
let eq_test _ =
  let exp = Var ("x",8) %=%  Expr.value (7,8) in
  let got = Expr.value (7,8) %=% Var ("x",8) in
  Equality.same_test exp got


let smart_constructors : unit test_case list =
   [test_case "equality (Var, Value)" `Quick eq_test]
