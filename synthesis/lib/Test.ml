open Ast
open Manip
open Prover
   
let test1 = string_of_expr (
    Seq(
      Assign("h",Var("Ingress")),
      While(Neg(Eq(Var "h",Var("Egress"))),
	    combineSelects
              (mkIf (Eq(Var "pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
              (mkIf (Eq(Var "pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
	   )
    )
  )
    
let test2 = wp (Assign("h",Var("Ingress"))) True 

let%test _ = Printf.printf "%s\n" test1; true
let%test _ = Printf.printf "%s\n" (string_of_test test2);
             test2 = True
let%test _ = checkSMT SATISFIABLE (Eq(Var "x", Int 6))
