open Ast

let test1 = string_of_expr (
	Seq(
		Assign("h",Var("Ingress")),
		While(Neg(Eq("h",Var("Egress"))),
		  mkIf (Eq("pkt",Int(3))) (Assign("pkt",Int(6))) (Assign("loc",Int(10)))
		)
	)
)
let () = Printf.printf "%s\n" test1