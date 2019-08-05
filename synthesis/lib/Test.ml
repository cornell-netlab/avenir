open Core
open Ast
open Manip
(* open Graph *)
open Semantics
open Synthesis
open Prover

let parse s = Parser.main Lexer.tokens (Lexing.from_string s)

let print_test_neq ~got:got ~exp:exp =
  Printf.printf "TEST FAILED ------\n";
  Printf.printf "EXPECTED: \n%s\n" (string_of_test exp);
  Printf.printf "GOT: \n%s\n" (string_of_test got);
  Printf.printf "------------------\n"

            
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
  | 2 -> Hole (generate_random_string size)
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
         TotalSelect (loop (1 + Random.int 4 ))
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
  unroll 1 (TotalSelect [ selectCond, mkWhile cond loop_body;
                         True, mkWhile cond loop_body])
  = TotalSelect [selectCond, unroll 1 (mkWhile cond loop_body);
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



  
let%test _ =
  let prog = TotalSelect [
                 Var "h" %=% Var "g", "g" %<-% Int 8
               ] in
  let prec = wp prog (Var "g" %=% Int 8) in
  let exp  = Var "h" %=% Var "g" in
  (if prec <> exp then print_test_neq ~got:prec ~exp:exp);
  prec = exp
  
let%test _ = (* wp behaves well with selects *)
  let prog =  TotalSelect[
                  Var "h" %=%  Var "g" , "g" %<-% Int 8
                ; Var "h" %=%  Int 99  , "h" %<-% Int 4
                ; Var "h" %<>% Int 2   , "h" %<-% Var "g"
                ] in
  let comp = wp prog (Var "g" %=% Int 8) in
  let all_conds =
    (Var "h" %=%  Var "g")
    %+% (Var "h" %=%  Int 99)
    %+% (Var "h" %<>% Int 2)
  in
  let all_imps =
    ((Var "h" %=%  Var "g") %=>% (Int  8  %=% Int 8))
    %&% ((Var "h" %=%  Int 99)  %=>% (Var "g" %=% Int 8))
    %&% ((Var "h" %<>% Int 2)   %=>% (Var "g" %=% Int 8))
  in
  let exp = all_conds %&% all_imps in
  (if comp <> exp then print_test_neq ~got:comp ~exp:exp );
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

let%test _ = (* wp behaves well with partials *)
  let cond = Var "pkt" %=% Int 101 in
  let prog = PartialSelect [ Var "pkt" %=% Hole "_hole0", "pkt" %<-% Hole "_hole1" ] in
  let expected = Var "pkt" %=% Hole "_hole0" %=>% (Hole "_hole1" %=% Int 101) in
  let pre = wp prog cond in
  (* Printf.printf "EXPECTED:\n%s\n\nGOT:\n%s\n" (sexp_string_of_test expected) (sexp_string_of_test pre); *)
  expected = pre
  

                           (* TEST PARSING *)

let%test _ =
  let rec loop n =
    if n = 0 then true else
      let e = generate_random_expr 3 in
      let s = string_of_expr e in
      try
        let s' = parse s |> string_of_expr in
        if s = s' then loop (n-1)
        else
          (Printf.printf "[PARSER ROUND TRIP] FAILED for:\n %s\n got %s" s  s';
           false)
      with _ ->
        (Printf.printf "[PARSING FAILED] for expression:\n%s\n\n%s\n" s (sexp_string_of_expr e);
        false)
  in
  loop 100
  


                           (* TEST GRAPH GENERATION *)
(* let%test _ =
 *   let e =  parse "if loc = 0 && x = 5 -> loc := 1 []  loc = 0 && ~(x = 5) -> loc := 2 []  loc = 1 -> y := 0; loc := 6     []  loc = 2 -> y := 1; loc := 6 fi " in
 *   Printf.printf "%s\n" (make_graph e |> string_of_graph); true *)





  
                           (* TESTING SEMANTICS *)

let test_trace p_string expected_trace =
  let p = parse p_string in
  let pkt = Packet.(set_field empty "pkt" 100) in
  let loc = Some 0 in
  match trace_eval p (pkt, loc) with
  | None -> false
  | Some (_, tr) -> tr = expected_trace
  

let%test _ = test_trace
               "loc:=0; loc:=0"
               [0; 0]
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

                            (* TESTING Formula Construction *)

let%test _ =
  let ctx = context in
  let t = (!%( (Var "x" %=% Int 5) %+% ((Var "x" %=% Int 3) %&% (Var "z" %=% Int 6)))
           %+% !%( (Var "x" %=% Hole "hole0") %+% (Var "y" %=% Hole "hole1"))) in
  let exp_fvs = ["x"; "z"; "y"] in
  let indices = mk_deBruijn (free_vars_of_test t) in
  let get = StringMap.find indices in
  let z3test = mkZ3Test t ctx indices in
  let expz3string = "(let ((a!1 (not (or (= (:var 2) 5) (and (= (:var 2) 3) (= (:var 1) 6))))))\n  (or a!1 (not (or (= (:var 2) hole0) (= (:var 0) hole1)))))" in
  let qform = bind_vars `Sat ctx exp_fvs z3test in
  let exp_qform_string ="(forall ((x Int) (z Int) (y Int))\n  (let ((a!1 (not (or (= x 5) (and (= x 3) (= z 6))))))\n    (or a!1 (not (or (= x hole0) (= y hole1))))))" in
  free_vars_of_test t = exp_fvs
  && get "x" = Some 2 && get "y" = Some 0 && get "z" = Some 1
  && Z3.Expr.to_string z3test = expz3string
  && Z3.Expr.to_string qform = exp_qform_string
    
let%test _ =
  let t = (!%( (Var "x" %=% Int 5) %+% ((Var "x" %=% Int 3) %&% (Var "z" %=% Int 6)))
           %+% !%( (Var "x" %=% Hole "hole0") %+% (Var "y" %=% Hole "hole1"))) in
  let r = check `Sat t in
  let r' = check `Sat t in
  r = None (* i.e. is unsat *)
  && r = r'
           
let%test _ = (* Test deBruijn Indices*)
  let vars = ["x"; "y"; "z"] in
  let indices = mk_deBruijn vars in
  let get = StringMap.find indices in
  match get "x", get "y", get "z" with
  | Some xi, Some yi, Some zi -> xi = 2 && yi = 1 && zi = 0
  | _, _, _ -> false
           
           


                           (* TESTING FOR CEGIS PROCEDURE *)

let%test _ =
  let pkt = Packet.(set_field empty "pkt" 100) in
  let log  = parse "loc := 0; while (~ loc = 1) { if loc = 0 && pkt = 100 -> pkt := 101; loc := 1 fi } " in
  let real = parse "loc := 0 ; while (~ loc = 1) { if loc = 0 && pkt = ?_hole0 -> pkt := ?_hole1; loc := 1 fi } " in
  let model = get_one_model pkt log real in
  let _ = model in
  true


let%test _ =
  let x = "x" in
  let y = "y" in
  let vx = Var x in
  let vy = Var y in
  let pkt = Packet.(set_field (set_field empty "x" 3) "y" 1) in
  let logical = SetLoc 0 %:%
                  While(!%(LocEq 6),
                        PartialSelect [
                            LocEq 0 %&% (vx %=% Int 5), SetLoc 1 ;
                            LocEq 0 %&% !%(vx %=% Int 5), SetLoc 2 ;
                            LocEq 1 , y %<-% Int 0 %:% (SetLoc 6) ;
                            LocEq 2 , y %<-% Int 1 %:% (SetLoc 6) ;
                          ]
                       )
  in
  let real = SetLoc 0 %:%
               While ( !%(LocEq 6),
                       PartialSelect [
                           LocEq 0 %&% (vx %=% Int 5 ), SetLoc 1 ;
                           LocEq 1 %&% (vy %=% Hole "_6"), SetLoc 2 ;
                           LocEq 2 , y %<-% Int 1 %:% (SetLoc 6) ;
                           LocEq 5 , y %<-% Int 0 %:% (SetLoc 6) ;
                           LocEq 0 %&% (vx %=% Hole "_0"), SetLoc 3 ;
                           LocEq 3 %&% (vx %=% Hole "_1" %&% (vy %=% Hole "_2")), SetLoc 5 ;
                           LocEq 3 %&% (vx %=% Hole "_3" %&% (vy %=% Hole "_4")), SetLoc 4 ;
                           LocEq 4 , (y %<-% Int 1 %:% (SetLoc 6))
                 ])
  in
  let _ = Printf.printf "\n----- Testing Running Example----\n\n" in
  let model = get_one_model pkt logical real in
  Printf.printf "PACKET:\n%s\n%!\nLOGICAL PROGRAM:\n%s\n%!\nREAL PROGRAM:\n%s\n%!\n MODEL:\n%s\n%!\nNEWREAL:\n%s\n%!"
    (Packet.string_of_packet pkt)
    (string_of_expr logical)
    (string_of_expr real)
    (string_of_map model)
    (string_of_expr (fixup real model)) ;
  (match cegis ~gas:1 logical real with
   | None -> Printf.printf "FINAL PROGRAM:\nNONE\n%!"
   | Some final_program -> 
     Printf.printf "FINAL PROGRAM:\n%s\n%!" (final_program |> string_of_expr));
  true
  
