open Ast
open Manip
open Prover
   
let loop_body =
  combineSelects
    (mkIf (Var "pkt" %=% Int 3) (("pkt" %<-% Int 6) %:% ("loc" %<-% Int 10)))
    (mkIf (Var "pkt" %=% Int 4) (("pkt" %<-% Int 2) %:% ("loc" %<-% Int 11)))
       
   
let simple_test =
  "h" %<-% Var "Ingress" %:%
    mkWhile (Var "h" %<>% Var "Egress") loop_body
   
let test1 = string_of_expr simple_test
                
let test2 = wp ("h" %<-% Var "Ingress") True 

let%test _ = Printf.printf "%s\n" test1; true
let%test _ = Printf.printf "%s\n" (string_of_test test2);
             test2 = True
let%test _ = checkSMT SATISFIABLE ((Var "x" %=% Int 6))

let%test _ = (* Testing unrolling *)
  unroll 1 simple_test = "h" %<-% Var "Ingress" %:%
                          (Var "h" %<>% Var "Egress" %?% loop_body)

let%test _= (* testing loop removal when n = 0*)
  unroll 0 simple_test = Seq(Assign("h",Var("Ingress")), Skip)

let%test _ = (* One unroll works *)
  let cond = Var "h" %<>% Var "Egress" in
  let input = ("h" %<-% Var "Ingress") %:% mkWhile cond loop_body in
  unroll 1 input
  = ("h" %<-% Var "Ingress") %:%  mkIf cond (loop_body %:% Skip)
  

let%test _ = (*Sequencing unrolls works*)
  let cond = Var "h" %<>% Var "Egress" in
  unroll 1 (mkWhile cond loop_body %:% mkWhile cond loop_body)
  = unroll 1 (mkWhile cond loop_body)
    %:% unroll 1 (mkWhile cond loop_body)

let%test _ = (*Selection unrolls works*)
  let cond = Var "h" %<>% Var "Egress" in
  let selectCond = Var "h" %=% Int 5 in
  unroll 1 (SelectFrom [ selectCond, mkWhile cond loop_body;
                         True, mkWhile cond loop_body])
  = SelectFrom [selectCond, unroll 1 (mkWhile cond loop_body);
                True, unroll 1 (mkWhile cond loop_body)]
      
      

(* Weakest precondition testing *)
let%test _ = (*Skip behaves well *)
  wp Skip True = True
  && wp Skip (Var "x" %=% Var "y") = Var "x" %=% Var "y"

let%test _ = (* Assign behaves well with integers *)
  let prog = "h" %<-% Int 7 in
  Printf.printf "%s\n" (string_of_test (wp prog (Var "h" %=% Int 7)));
  Int 7 %=% Int 7 = wp prog (Var "h" %=% Int 7)
  && Int 7 %=% Var "g" = wp prog (Var "h" %=% Var "g")

let%test _ = (* Assign behaves well with variables *)
  let prog = "h" %<-% Var "hgets" in
  let wphEQ x = wp prog (Var "h" %=% x) in
  Var "hgets" %=% Int 7 = wphEQ (Int 7) 
  && Var "hgets" %=% Var "g" = wphEQ (Var "g")

let%test _ = (* wp behaves well with selects *)
  let prog =  SelectFrom[
                  Var "h" %=%  Var "g" , "g" %<-% Int 8
                ; Var "h" %=%  Int 99  , "h" %<-% Int 4
                ; Var "h" %<>% Int 2   , "h" %<-% Var "g"
                ] in
  let comp = wp prog (Var "g" %=% Int 8) in
  let all_conds =
    False
    %+% (Var "h" %=%  Var "g")
    %+% (Var "h" %=%  Int 99)
    %+% (Var "h" %<>% Int 2)
  in
  let all_imps =
    True
    %&% ((Var "h" %=%  Var "g") %=>% (Int  8  %=% Int 8))
    %&% ((Var "h" %=%  Int 99)  %=>% (Var "g" %=% Int 8))
    %&% ((Var "h" %<>% Int 2)   %=>% (Var "g" %=% Int 8))
  in
  let exp = all_conds %&% all_imps in
  comp = exp

           
let%test _ = (* wp behaves well with sequence *)
  let prog = ("h" %<-% Int 10) %:% ("h" %<-% Int 80) in
  let cond = Var "h" %=% Var "g" in
  Int 80 %=% Var "g" = wp prog cond

let%test _ = (* wp behaves well with assertions *)
  let asst = (Var "h" %<>% Int 10) %&% (Var "h" %<>% Int 15) in
  let prog = Assert(asst) in
  let phi =  Var "h" %=% Var "g" in
  wp prog phi = asst %&% phi
  
