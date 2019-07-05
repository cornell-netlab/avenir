open Ast
open Manip
open Prover
   
let loop_body =
  combineSelects
    (mkIf (Eq(Var "pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
    (mkIf (Eq(Var "pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
   
let simple_test =
  Seq(
      Assign("h",Var("Ingress")),
      While(Neg(Eq(Var "h",Var("Egress"))), loop_body))
   
let test1 = string_of_expr simple_test
                
let test2 = wp (Assign("h",Var("Ingress"))) True 

let%test _ = Printf.printf "%s\n" test1; true
let%test _ = Printf.printf "%s\n" (string_of_test test2);
             test2 = True
let%test _ = checkSMT SATISFIABLE (Eq(Var "x", Int 6))



let%test _ = (* Testing unrolling *)
  unroll 0 simple_test = simple_test


let%test _ = (* One unroll works *)
  let cond = Neg(Eq(Var "h",Var("Egress"))) in
  let loop_body =
    combineSelects
      (mkIf (Eq(Var "pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
      (mkIf (Eq(Var "pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
  in
  let input = Seq(Assign("h", Var "Ingress"), While(cond, loop_body)) in
  unroll 1 input
  = Seq(Assign("h", Var "Ingress"),
        mkIf cond (Seq(loop_body, While(cond, loop_body))))
  

let%test _ = (*Sequencing unrolls works*)
  let cond = Neg(Eq(Var "h",Var("Egress"))) in
  let loop_body =
    combineSelects
      (mkIf (Eq(Var "pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
      (mkIf (Eq(Var "pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
  in
  unroll 1 (Seq(While(cond, loop_body), While(cond, loop_body)))
  = Seq(unroll 1 (While(cond, loop_body)), unroll 1 (While(cond, loop_body)))

let%test _ = (*Selection unrolls works*)
  let cond = Neg(Eq(Var "h",Var("Egress"))) in
  let selectCond = Eq(Var "h", Int 5) in
  let loop_body =
    combineSelects
      (mkIf (Eq(Var "pkt",Int(3))) (Seq(Assign("pkt",Int(6)), Assign("loc", Int(10)))))
      (mkIf (Eq(Var "pkt",Int(4))) (Seq(Assign("pkt",Int(2)), Assign("loc", Int(11)))))
  in
  unroll 1 (SelectFrom [ selectCond, While(cond, loop_body);
                         True, While(cond, loop_body)])
  = SelectFrom [selectCond, unroll 1 (While(cond, loop_body));
                True, unroll 1 (While(cond, loop_body)) ]
      
      

(* Weakest precondition testing *)
let%test _ = (*Skip behaves well *)
  wp Skip True = True
  && wp Skip (Eq(Var "x", Var "y")) = (Eq (Var "x", Var "y"))

let%test _ = (* Assign behaves well with integers *)
  let prog = Assign ("h", Int 7) in
  Eq(Int 7, Int 7) = wp prog (Eq(Var "h", Int 7))
  && Eq(Int 7, Var "g") = wp prog (Eq(Var "h", Var "g"))

let%test _ = (* Assign behaves well with variables *)
  let prog = Assign ("h", Var "hgets") in
  let wphEQ x = wp prog (Eq(Var "h", x)) in  
  Eq(Var "hgets", Int 7) = wphEQ (Int 7) 
  && Eq(Var "hgets", Var "g") = wphEQ (Var "g")


let%test _ = (* wp behaves well with selects *)
  let prog =  SelectFrom[
                  Eq(Var "h", Var "g")    , Assign ("g", Int 8)
                ; Eq(Var "h", Int 99)     , Assign ("h", Int 4)
                ; Neg(Eq(Var "h", Int 2)) , Assign ("h", Var "g")
                ] in
  let comp = wp prog (Eq(Var "g", Int 8)) in
  let all_conds =
    Or(Or(Or(False
            , Eq(Var "h", Var "g"))
         , Eq(Var "h", Int 99))
      , Neg(Eq(Var "h", Int 2))) in
  let all_imps = And(And(And(True,
                             mkImplies (Eq (Var "h", Var "g")) (Eq(Int 8, Int 8)))
                       , mkImplies (Eq(Var "h", Int 99)) (Eq(Var "g", Int 8))),
                     mkImplies (Neg (Eq (Var "h", Int 2))) (Eq(Var "g", Int 8))) in 
  let exp = And( all_conds, all_imps ) in
  comp = exp

           
let%test _ = (* wp behaves well with sequecnce *)
  let prog = Seq(Assign ("h", Int 10), Assign("h", Int 80)) in
  let cond = Eq(Var "h", Var "g") in
  Eq(Int 10, Var "g") = wp prog cond
