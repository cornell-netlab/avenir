open Core
open Ast
open Manip
open Graph
open Prover
open Semantics
open Synthesis

let parse s = Parser.main Lexer.tokens (Lexing.from_string s)

let alphanum = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"

let rec generate_random_string length =
  if length = 0 then "" else
    let max = String.length alphanum in
    String.sub alphanum ~pos:(Random.int max) ~len:1
    ^ generate_random_string (length - 1)
    
            
let generate_random_value size =
  match Random.int 3 with
  | 0 -> Int (Random.int 256)
  | 1 -> Var (generate_random_string size)
  | 2 -> Hole ("?" ^ generate_random_string size)
  | _ -> failwith "generated random integer larger than 2"
          
let rec generate_random_test size =
  if size = 0 then if Random.int 1 = 1 then False else True else 
  let size' = size - 1 in
  match Random.int 7 with
  | 0 -> True
  | 1 -> False
  | 2 -> Eq (generate_random_value 5, generate_random_value 5)
  | 3 -> Lt (generate_random_value 6, generate_random_value 7)
  | 4 -> And (generate_random_test size', generate_random_test size')
  | 5 -> Or (generate_random_test size', generate_random_test size')
  | 6 -> Neg (generate_random_test size')
  | _ -> failwith "generated random integer larger than 6"
            
let rec generate_random_expr size =
  if size = 0 then Skip else 
  let size' = size - 1 in
  match Random.int 6 with
  | 0 -> Skip
  | 1 -> Assign (generate_random_string 3, generate_random_value 5)
  | 2 -> Assert (generate_random_test size')
  | 3 -> Assume (generate_random_test size')
  | 4 -> Seq (generate_random_expr size', generate_random_expr size')
  | 5 -> While (generate_random_test size', generate_random_expr size')
  | 6 -> let rec loop n =
           if n = 0 then [] else 
           (generate_random_test size', generate_random_expr size') :: loop (n-1)
         in
         SelectFrom (loop (1 + Random.int 4 ))
  | _ -> failwith "Should Not generate number larger than 5"
    

            
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
let%test _ = test2 = True
             
let%test _ = checkSMT SATISFIABLE ((Var "x" %=% Int 6) %+% (Var "x" %<>% Int 6))
let%test _ = checkSMT SATISFIABLE (Hole "?x" %=% Int 6)

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
  (* Printf.printf "%s\n" (string_of_test (wp prog (Var "h" %=% Int 7))); *)
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


                           (* TEST PARSING *)

let%test _ =
  let rec loop n =
    if n = 0 then true else
      let e = generate_random_expr 3 in
      let s = string_of_expr e in
      let s' = parse s |> string_of_expr in
      if s = s' then loop (n-1)
      else
        (Printf.printf "[PARSER ROUND TRIP] FAILED for:\n %s\n got %s" s  s';
         false)
  in
  loop 100


  


                           (* TEST GRAPH GENERATION *)
let%test _ =
  let e =  parse "if loc = 0 && x = 5 -> loc := 1 []  loc = 0 && ~(x = 5) -> loc := 2 []  loc = 1 -> y := 0; loc := 6     []  loc = 2 -> y := 1; loc := 6 fi " in
  Printf.printf "%s\n" (make_graph e |> string_of_graph); true





  
                           (* TESTING SEMANTICS *)

let test_trace p_string expected_trace =
  let p = parse p_string in
  let pkt = Packet.(set_field (set_field empty "loc" 0) "pkt" 100) in
  let _, tr = trace_eval p pkt in
  tr = expected_trace
  

let%test _ = test_trace
               "loc:=0; loc:=0"
               [0]
let%test _ = test_trace
               "loc:=0; loc := 1"
               [0; 1]
let%test _ = test_trace
               "loc:=0; if loc = 0 -> loc := 1 fi; loc := 2"
               [0; 1; 2]
let%test _ = test_trace
               "loc:=0; while (~ loc = 1) { loc := 1 }"
               [0; 1]
let%test _ = test_trace
               "loc := 0; while (~ loc = 1) { if loc = 0 && pkt = 100 -> pkt := 101; loc := 1 fi }"
               [0;1]
  


                           (* TESTING FOR CEGIS PROCEDURE *)

let%test _ =
  let pkt = Packet.(set_field (set_field empty "loc" 0) "pkt" 100) in
  let log  = parse "loc := 0; while (~ loc = 1) { if loc = 0 && pkt = 100 -> pkt := 101; loc := 1 fi } " in
  let real = parse "loc := 0 ; while (~ loc = 1) { if loc = 0 && pkt = ?_hole0 -> pkt := ?_hole1; loc := 1 fi } " in
  let model = get_one_model pkt log real in
  let _ = model in
  true
  
  
