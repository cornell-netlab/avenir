open Core
open Ast
open Manip
open Graph
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
    
            
let generate_random_expr1 size =
  match Random.int 3 with
  | 0 -> Value1 (Int (Random.int 256))
  | 1 -> Var1 (generate_random_string size)
  | 2 -> Hole1 (generate_random_string size)
  | _ -> failwith "generated random integer larger than 2"
          
let rec generate_random_test size =
  if size = 0 then if Random.int 1 = 1 then False else True else 
  let size' = size - 1 in
  match Random.int 7 with
  | 0 -> True
  | 1 -> False
  | 2 -> Eq (generate_random_expr1 5, generate_random_expr1 5)
  | 3 -> Lt (generate_random_expr1 6, generate_random_expr1 7)
  | 4 -> And (generate_random_test size', generate_random_test size')
  | 5 -> Or (generate_random_test size', generate_random_test size')
  | 6 -> Neg (generate_random_test size')
  | _ -> failwith "generated random integer larger than 6"

let generate_random_select_type _ =
  match Random.int 3 with
  | 0 -> Total
  | 1 -> Partial
  | 2 -> Ordered
  | _ -> failwith "generated random integer larger than 6"

let rec generate_random_cmd size =
  if size = 0 then Skip else 
  let size' = size - 1 in
  match Random.int 6 with
  | 0 -> Skip
  | 1 -> Assign (generate_random_string 3, generate_random_expr1 5)
  | 2 -> Assert (generate_random_test size')
  | 3 -> Assume (generate_random_test size')
  | 4 -> Seq (generate_random_cmd size', generate_random_cmd size')
  | 5 -> While (generate_random_test size', generate_random_cmd size')
  | 6 -> let rec loop n =
           if n = 0 then [] else 
           (generate_random_test size', generate_random_cmd size') :: loop (n-1)
         in
         mkSelect (generate_random_select_type ()) (loop (1 + Random.int 4 ))
  | _ -> failwith "Should Not generate number larger than 5"
    

let loop_body =
  mkSelect Partial
    [ Var1 "pkt" %=% Value1(Int 3) , ("pkt" %<-% Value1(Int 6)) %:% ("loc" %<-% Value1(Int 10))
    ; Var1 "pkt" %=% Value1(Int 4) , ("pkt" %<-% Value1(Int 2)) %:% ("loc" %<-% Value1(Int 11))]       
    
let simple_test =
  "h" %<-% Var1 "Ingress" %:%
    mkWhile (Var1 "h" %<>% Var1 "Egress") loop_body
   
let test1 = string_of_cmd simple_test
                
let test2 = wp ("h" %<-% Var1 "Ingress") True

let complete_test_with_drop_location_no_holes =
  SetLoc 0 %:%
  While(!%(LocEq 1) %&% !%(LocEq (-1)), 
        mkPartial
          [ LocEq 0 %&% (Var1 "pkt" %=% Value1 (Int 42)) ,  SetLoc 1 %:% ("pkt" %<-% Value1 (Int 47))
          ; LocEq 0 %&% !%(Var1 "pkt" %=% Value1 (Int 42)), SetLoc (-1) ]
       )

let complete_test_with_drop_location_holes =
  let pkt_eq h = Var1 "pkt" %=% Hole1 h in
  let pkt_gets h = "pkt" %<-% Hole1 h in
  SetLoc 0 %:%
  While(!%(LocEq 1) %&% !%(LocEq (-1)),
        mkPartial 
          [ LocEq 0 %&% (pkt_eq "_0")  , SetLoc 1 %:% pkt_gets "_1"
          ; LocEq 0 %&% !%(pkt_eq "_0"), SetLoc (-1)]
       )


             
let%test _ = (* Testing unrolling *)
  unroll 1 simple_test = "h" %<-% Var1 "Ingress" %:%
                          mkSelect Partial [Var1 "h" %<>% Var1 "Egress" , loop_body]

let%test _= (* testing loop removal when n = 0*)
  unroll 0 simple_test = Seq(Assign("h",Var1("Ingress")), Skip)

let%test _ = (* One unroll works *)
  let cond = Var1 "h" %<>% Var1 "Egress" in
  let input = ("h" %<-% Var1 "Ingress") %:% mkWhile cond loop_body in
  unroll 1 input
  = ("h" %<-% Var1 "Ingress") %:%  mkSelect Partial [cond , loop_body %:% Skip]
  

let%test _ = (*Sequencing unrolls works*)
  let cond = Var1 "h" %<>% Var1 "Egress" in
  unroll 1 (mkWhile cond loop_body %:% mkWhile cond loop_body)
  = unroll 1 (mkWhile cond loop_body)
    %:% unroll 1 (mkWhile cond loop_body)

let%test _ = (*Selection unrolls works*)
  let cond = Var1 "h" %<>% Var1 "Egress" in
  let selectCond = Var1 "h" %=% Value1 (Int 5) in
  unroll 1 (mkSelect Total [ selectCond, mkWhile cond loop_body
                           ; True, mkWhile cond loop_body])
  = mkSelect Total [selectCond, unroll 1 (mkWhile cond loop_body)
                   ; True, unroll 1 (mkWhile cond loop_body)]

(* Testing equality smart constructor *)
let%test _ =
  let exp = Var1 "x" %=% Value1(Int 7) in
  let got = Value1 (Int 7) %=% Var1 "x" in
  if exp = got then true else
    (print_test_neq ~got ~exp;false)
  
      
      

(* Weakest precondition testing *)
let%test _ = (*Skip behaves well *)
  wp Skip True = True
  && wp Skip (Var1 "x" %=% Var1 "y") = Var1 "x" %=% Var1 "y"

let%test _ = (* Assign behaves well with integers *)
  let prog = "h" %<-% Value1(Int 7) in
  (* Printf.printf "%s\n" (string_of_test (wp prog (Var1 "h" %=% Int 7))); *)
  (* Int 7 %=% Int 7 = wp prog (Var1 "h" %=% Int 7) *)
  if Value1 (Int 7) %=% Var1 "g" = wp prog (Var1 "h" %=% Var1 "g") then true else
    (print_test_neq
       ~exp:(Value1 (Int 7) %=% Var1 "g")
       ~got:(wp prog (Var1 "h" %=% Var1 "g"))
    ; false)
  
    
    

let%test _ = (* Assign behaves well with variables *)
  let prog = "h" %<-% Var1 "hgets" in
  let wphEQ x = wp prog (Var1 "h" %=% x) in
  Var1 "hgets" %=% mkVInt 7 = wphEQ (mkVInt 7) 
  && Var1 "hgets" %=% Var1 "g" = wphEQ (Var1 "g")



  
let%test _ =
  let prog = mkSelect Total [
                 Var1 "h" %=% Var1 "g", "g" %<-% mkVInt 8
               ] in
  let prec = wp prog (Var1 "g" %=% mkVInt 8) in
  let exp  = Var1 "h" %=% Var1 "g" in
  (if prec <> exp then print_test_neq ~got:prec ~exp:exp);
  prec = exp
  
let%test _ = (* wp behaves well with selects *)
  let prog = mkSelect Total [ Var1 "h" %=%  Var1 "g" , "g" %<-% mkVInt 8
                            ; Var1 "h" %=%  mkVInt 99  , "h" %<-% mkVInt 4
                            ; Var1 "h" %<>% mkVInt 2   , "h" %<-% Var1 "g"
                            ] in
  let comp = wp prog (Var1 "g" %=% mkVInt 8) in
  let all_conds =
    (Var1 "h" %=%  Var1 "g")
    %+% (Var1 "h" %=%  mkVInt 99)
    %+% (Var1 "h" %<>% mkVInt 2)
  in
  let all_imps =
    ((Var1 "h" %=%  Var1 "g") %=>% (mkVInt  8  %=% mkVInt 8))
    %&% ((Var1 "h" %=%  mkVInt 99)  %=>% (Var1 "g" %=% mkVInt 8))
    %&% ((Var1 "h" %<>% mkVInt 2)   %=>% (Var1 "g" %=% mkVInt 8))
  in
  let exp = all_conds %&% all_imps in
  (if comp <> exp then print_test_neq ~got:comp ~exp:exp );
  comp = exp


           
let%test _ = (* wp behaves well with sequence *)
  let prog = ("h" %<-% mkVInt 10) %:% ("h" %<-% mkVInt 80) in
  let cond = Var1 "h" %=% Var1 "g" in
  mkVInt 80 %=% Var1 "g" = wp prog cond

let%test _ = (* wp behaves well with assertions *)
  let asst = (Var1 "h" %<>% mkVInt 10) %&% (Var1 "h" %<>% mkVInt 15) in
  let prog = Assert(asst) in
  let phi =  Var1 "h" %=% Var1 "g" in
  wp prog phi = asst %&% phi

let%test _ = (* wp behaves well with partials *)
  let cond = Var1 "pkt" %=% mkVInt 101 in
  let prog = mkSelect Partial [ Var1 "pkt" %=% Hole1 "_hole0", "pkt" %<-% Hole1 "_hole1" ] in
  let exp = Var1 "pkt" %=% Hole1 "_hole0" %=>% (Hole1 "_hole1" %=% mkVInt 101) in
  let got = wp prog cond in
  if exp = got then true else(
    print_test_neq ~exp ~got;
    false
  )


let%test _ = (* wp behaves well with totals *)
  let cond = Var1 "pkt" %=% mkVInt 101 in
  let prog = mkSelect Total [ Var1 "pkt" %=% Hole1 "_hole0", "pkt" %<-% Hole1 "_hole1" ] in
  let exp = (Var1 "pkt" %=% Hole1 "_hole0") %&% (Var1 "pkt" %=% Hole1 "_hole0" %=>% (Hole1 "_hole1" %=% mkVInt 101)) in
  let got = wp prog cond in
  if exp = got then true else(
    print_test_neq ~exp ~got;
    false
  )

let%test _ = (* wp behaves well with ordereds *)
  let cond = Var1 "pkt" %=% mkVInt 101 in
  let prog = mkSelect Ordered [ Var1 "pkt" %=% Hole1 "_hole0", "pkt" %<-% Hole1 "_hole1"
                              ; Var1 "pkt" %=% mkVInt 99 , "pkt" %<-% mkVInt 101] in
  let exp = (Var1 "pkt" %=% Hole1 "_hole0" %=>% (Hole1 "_hole1" %=% mkVInt 101))
             %&% ((Var1 "pkt" %=% mkVInt 99 %&% (Var1 "pkt" %<>% Hole1 "_hole0")) %=>% (mkVInt 101 %=% mkVInt 101))
  in
  let got = wp prog cond in
  (* Printf.printf "EXPECTED:\n%s\n\nGOT:\n%s\n" (sexp_string_of_test expected) (sexp_string_of_test pre); *)
  if exp = got then true else (
    print_test_neq ~exp ~got;
    false
  )


let%test _ =
  let cond = Var1 "pkt" %=% Var1 "ALPHA" in
  let prog = complete_test_with_drop_location_no_holes in
  let u_prog = unroll (Graph.diameter prog) prog in
  let expected =
    ((Var1 "pkt" %=% mkVInt 42) %=>% (Var1 "ALPHA" %=% mkVInt 47 ))
    %&% ( !%(Var1 "pkt" %=% mkVInt 42) %=>% (Var1 "ALPHA" %=% Var1 "pkt" ))
  in
  let wp_got = wp u_prog cond in
  if wp_got = expected
  then true
  else (print_test_neq ~exp:expected ~got:wp_got; false)


let%test _ =
  let x = Var1 "x" in
  let alpha = Var1 "$0" in
  let y = Var1 "y" in
  let beta = Var1 "$1" in
  let inner =
        mkSelect Ordered
          [ LocEq 0 %&% (x %=% mkVInt 5), SetLoc 1
          ; LocEq 0, SetLoc 3
          ; LocEq 1, SetLoc 2
          ; LocEq 2, "y" %<-% mkVInt 1 %:% SetLoc 6
          ; LocEq 3 %&% (x %=% mkVInt 5), SetLoc 4
          ; LocEq 3, SetLoc 5
          ; LocEq 4, "y" %<-% mkVInt 99 %:% SetLoc 6
          ; LocEq 5, "y" %<-% mkVInt 0  %:% SetLoc 6 ]
  in
  let cond = x %=% alpha %&% (y %=% beta)
             %&% LocEq 6 in
  let exp = (x %<>% mkVInt 5
             %=>% (alpha %=% x %&% (beta %=% mkVInt 0)))
            %&% (x %=% mkVInt 5
              %=>% (alpha %=% x %&% (beta %=% mkVInt 1)))
  in
  let prog = SetLoc 0
             %:% inner %:% inner %:% inner
             %:% Assert (LocEq 6) in
  let got = wp prog cond in
  check_valid (exp %<=>% got) = None
  

                           (* TEST PARSING *)

(* let%test _ =
 *   let rec loop n =
 *     if n = 0 then true else
 *       let e = generate_random_cmd 3 in
 *       let s = string_of_cmd e in
 *       try
 *         let s' = parse s |> string_of_cmd in
 *         if s = s' then loop (n-1)
 *         else
 *           (Printf.printf "[PARSER ROUND TRIP] FAILED for:\n %s\n got %s" s  s';
 *            false)
 *       with _ ->
 *         (Printf.printf "[PARSING FAILED] for cmdession:\n%s\n\n%s\n" s (sexp_string_of_cmd e);
 *         false)
 *   in
 *   loop 100 *)
  


                           (* TEST GRAPH GENERATION *)
(* let%test _ =
 *   let e =  parse "if loc = 0 && x = 5 -> loc := 1 []  loc = 0 && ~(x = 5) -> loc := 2 []  loc = 1 -> y := 0; loc := 6     []  loc = 2 -> y := 1; loc := 6 fi " in
 *   Printf.printf "%s\n" (make_graph e |> string_of_graph); true *)


let%test _ = 1 = Graph.diameter complete_test_with_drop_location_no_holes
let%test _ = 1 = Graph.diameter complete_test_with_drop_location_holes

let%test _ =
  let x = Var1 "x" in
  let y = "y" in
  let hole1 = Hole1 "_1" in
  let hole2 = Hole1 "_2" in
  let hole5 = Hole1 "_5" in
  let selects =
    [ LocEq 0 %&% (x %=% mkVInt 5), SetLoc 1
    ; LocEq 0 , SetLoc 3
    ; LocEq 1 , SetLoc 2
    ; LocEq 2 , y %<-% mkVInt 0 %:% SetLoc 0
    ; LocEq 3 %&% Lt (x, hole1) %&% Lt(x, hole2), SetLoc 4
    ; LocEq 3 , SetLoc 5
    ; LocEq 4 , y %<-% hole5 %:% SetLoc 6
    ; LocEq 5 , y %<-% mkVInt 1 %:% SetLoc 6 ]
  in
  let expected = [ LocEq 0 %&% (x %=% mkVInt 5), SetLoc 1
                 ; LocEq 0 %&% !%(x %=% mkVInt 5), SetLoc 3
                 ; LocEq 1, SetLoc 2
                 ; LocEq 2, y %<-% mkVInt 0 %:% SetLoc 0
                 ; LocEq 3 %&% Lt (x, hole1) %&% Lt(x, hole2), SetLoc 4
                 ; LocEq 3 %&% !%(Lt (x, hole1) %&% Lt(x, hole2)), SetLoc 5
                 ; LocEq 4, y %<-% hole5 %:% SetLoc 6
                 ; LocEq 5, y %<-% mkVInt 1 %:% SetLoc 6 ]
  in
  let got = ordered_selects selects in
  if got = expected then true else begin
    Printf.printf "---- FAILED TEST ------ \n%!";
    Printf.printf "EXPECTED:\n%s\n%!\nGOT:\n%s\n%!\n"
      (string_of_cmd (Select (Partial, expected)))
      (string_of_cmd (Select (Partial, got)));
    Printf.printf "----------------------- \n%!";
    false
  end
    
    
  
                           (* TESTING SEMANTICS *)

let test_trace p_string expected_trace =
  let p = parse p_string in
  let pkt = Packet.(set_field empty "pkt" (mkInt 100)) in
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
               "loc:=0; if total loc = 0 -> loc := 1 fi; loc := 2"
               [0; 1; 2]
let%test _ = test_trace
               "loc:=0; while (~ loc = 1) { loc := 1 }"
               [0; 1]
let%test _ = test_trace
               "loc := 0; while (~ loc = 1) { if partial loc = 0 && pkt = 100 -> pkt := 101; loc := 1 fi }"
               [0;1]

                            (* TESTING Formula Construction *)

let%test _ =
  let ctx = context in
  let t = (!%( (Var1 "x" %=% mkVInt 5) %+% ((Var1 "x" %=% mkVInt 3) %&% (Var1 "z" %=% mkVInt 6)))
           %+% !%( (Var1 "x" %=% Hole1 "hole0") %+% (Var1 "y" %=% Hole1 "hole1"))) in
  let exp_fvs = ["x"; "z"; "y"] in
  let indices = mk_deBruijn (free_vars_of_test t) in
  let get = StringMap.find indices in
  let z3test = mkZ3Test `Sat t ctx indices in
  let expz3string = "(let ((a!1 (not (or (= (:var 2) 5) (and (= (:var 2) 3) (= (:var 1) 6))))))\n  (or a!1 (not (or (= (:var 2) hole0) (= (:var 0) hole1)))))" in
  let qform = bind_vars ctx exp_fvs z3test in
  let exp_qform_string ="(forall ((x mkVInt) (z mkVInt) (y mkVInt))\n  (let ((a!1 (not (or (= x 5) (and (= x 3) (= z 6))))))\n    (or a!1 (not (or (= x hole0) (= y hole1))))))" in
  let success = free_vars_of_test t = exp_fvs
                && get "x" = Some 2 && get "y" = Some 0 && get "z" = Some 1
                && Z3.Expr.to_string z3test = expz3string
                && Z3.Expr.to_string qform = exp_qform_string
  in
  if success then success else (
    Printf.printf "FAILED TEST (deBruijn) -----\n%!";
    Printf.printf "DE_BRUIJN :\n%!";
    StringMap.iteri indices ~f:(fun ~key ~data ->
        Printf.printf "\t %s -> %d\n%!" key data;
      );
    Printf.printf "Z3STRING:\n%s\n\n%!EXPECTED:\n%s\n\n%!" (Z3.Expr.to_string z3test) expz3string;
    Printf.printf "----------------------------\n%!";
    success )

    
let%test _ =
  let t = (!%( (Var1 "x" %=% mkVInt 5) %+% ((Var1 "x" %=% mkVInt 3) %&% (Var1 "z" %=% mkVInt 6)))
           %+% !%( (Var1 "x" %=% Hole1 "hole0") %+% (Var1 "y" %=% Hole1 "hole1"))) in
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
           
           
(* TESTING WELL-FORMEDNESS CONDITIONS *)
let%test _ = (* [no_nesting] accepts programs that have no nesting *)
    [ Skip 
    ; SetLoc 8
    ; Assert (LocEq 9 %&% (Var1 "x" %=% mkVInt 100))
    ; Seq (Skip, Seq(SetLoc 100, "x" %<-% mkVInt 200)) ]
    |> List.map ~f:(fun x -> (True, x))
    |> no_nesting

let%test _ = (* [no_nesting] rejects programs that have nesting *)
  let inj_nesting x =
    [ SetLoc 8
    ; Seq(Assert (LocEq 9), x)
    ; Skip ]
    |> List.map ~f:(fun x -> (True, x))
    |> no_nesting
  in
  not (inj_nesting (While (True, Skip)))
  && not (inj_nesting (mkSelect Total [(True, Skip)]))

let%test _ = (* [instrumented] accepts fully instrumented programs *)
  instrumented
  [ (LocEq 0 %&% Eq(Var1 "x", Hole1 "_9")),
    (Assign ("x", Hole1 "_10") %:% (SetLoc 100))
  ; LocEq 9, SetLoc 99 ]

let%test _ = (* [instrumented] rejects programs with missing instrumentation *)
  not (instrumented
         [ LocEq 9, SetLoc 99
         ; (True, Skip) ])
  && not (instrumented
            [ LocEq 9, SetLoc 99
            ; True, SetLoc 99 ])
  && not (instrumented
            [ LocEq 0, "x" %<-% mkVInt 100 %:% Assert (LocEq 9)])


let%test _ = (* [no_negated_holes] accepts programs with no negated holes*)
  no_negated_holes
    [ (True, Skip)
    ; (True, Assert (Hole1 "_0" %=% Hole1 "_1"))
    ; (Hole1 "_0" %=% mkVInt 100,  Assign ("x", mkVInt 100))
    ; (Neg(Neg(Hole1 "_0" %=% mkVInt 99)), Skip)
    ; (Neg(And(Neg (Hole1 "_0" %=% mkVInt 99), True)), Skip)]


let%test _ = (* [no_negated_holes] rejects programs with negated holes]*)
  not (no_negated_holes [(Neg (Hole1 "_8" %=% mkVInt 100), Skip)])
  && not (no_negated_holes [(True, Assert (Hole1 "_8" %<>% mkVInt 99))])
  && not (no_negated_holes [(Neg (Neg (And (Hole1 "_8" %<>% mkVInt 99, True))), Skip)])
                                         
    
(* TESTING FOR CEGIS PROCEDURE *)

let%test _ =
  let pkt = Packet.(set_field empty "pkt" (mkInt 100)) in
  let log  = parse "loc := 0; while (~ loc = 1) { if partial loc = 0 && pkt = 100 -> pkt := 101; loc := 1 fi } " in
  let real = parse "loc := 0 ; while (~ loc = 1) { if partial loc = 0 && pkt = ?_hole0 -> pkt := ?_hole1; loc := 1 fi } " in
  let model = get_one_model pkt log real in
  let _ = model in
  true


(* let%test _ =
 *   let x = "x" in
 *   let y = "y" in
 *   let vx = Var1 x in
 *   let vy = Var1 y in
 *   let pkt = Packet.(set_field (set_field empty "x" 3) "y" 1) in
 *   let logical = SetLoc 0 %:%
 *                   While(!%(LocEq 6),
 *                         PartialSelect [
 *                             LocEq 0 %&% (vx %=% mkVInt 5), SetLoc 1 ;
 *                             LocEq 0 %&% !%(vx %=% mkVInt 5), SetLoc 2 ;
 *                             LocEq 1 , y %<-% mkVInt 0 %:% (SetLoc 6) ;
 *                             LocEq 2 , y %<-% mkVInt 1 %:% (SetLoc 6) ;
 *                           ]
 *                        )
 *   in
 *   let real = SetLoc 0 %:%
 *                While ( !%(LocEq 6),
 *                        PartialSelect [
 *                            LocEq 0 %&% (vx %=% mkVInt 5 ), SetLoc 1 ;
 *                            LocEq 1 %&% (vy %=% Hole1 "_6"), SetLoc 2 ;
 *                            LocEq 2 , y %<-% mkVInt 1 %:% (SetLoc 6) ;
 *                            LocEq 5 , y %<-% mkVInt 0 %:% (SetLoc 6) ;
 *                            LocEq 0 %&% (vx %=% Hole1 "_0"), SetLoc 3 ;
 *                            LocEq 3 %&% (vx %=% Hole1 "_1" %&% (vy %=% Hole1 "_2")), SetLoc 5 ;
 *                            LocEq 3 %&% (vx %=% Hole1 "_3" %&% (vy %=% Hole1 "_4")), SetLoc 4 ;
 *                            LocEq 4 , (y %<-% mkVInt 1 %:% (SetLoc 6))
 *                  ])
 *   in
 *   let _ = Printf.printf "\n----- Testing Running Example----\n\n" in
 *   let model = get_one_model pkt logical real in
 *   Printf.printf "PACKET:\n%s\n%!\nLOGICAL PROGRAM:\n%s\n%!\nREAL PROGRAM:\n%s\n%!\n MODEL:\n%s\n%!\nNEWREAL:\n%s\n%!"
 *     (Packet.string_of_packet pkt)
 *     (string_of_cmd logical)
 *     (string_of_cmd real)
 *     (string_of_map model)
 *     (string_of_cmd (fixup real model)) ;
 *   true *)
  
