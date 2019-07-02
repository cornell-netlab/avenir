open Ast

let test1 = string_of_expr (
    Seq(
      Assign("h",Var("Ingress")),
      While(Neg(Eq("h",Var("Egress"))),
	    combineSelects
              (mkIf (Eq("pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
              (mkIf (Eq("pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
	   )
    )
  )
    
let () = Printf.printf "%s\n" test1
