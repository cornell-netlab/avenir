open Core
open Ast
open Manip
open Packet
open Semantics

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
  | 0 -> Value (Int (Random.int 256 |> Bigint.of_int_exn, 8))
  | 1 -> Var (generate_random_string size, 8)
  | 2 -> Hole (generate_random_string size, 8)
  | _ -> failwith "generated random integer larger than 2"

let rec generate_random_test size =
  if size = 0 then if Random.int 1 = 1 then False else True else
  let size' = size - 1 in
  match Random.int 7 with
  | 0 -> True
  | 1 -> False
  | 2 -> Eq (generate_random_expr1 5, generate_random_expr1 5)
  | 3 -> Le (generate_random_expr1 6, generate_random_expr1 7)
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
    [ Var ("pkt",8) %=% mkVInt (3,8) , ("pkt" %<-% mkVInt (6, 8)) %:% ("loc" %<-% mkVInt (10,8))
    ; Var ("pkt",8) %=% mkVInt (4,8) , ("pkt" %<-% mkVInt (2, 8)) %:% ("loc" %<-% mkVInt (11,8))]

let simple_test =
  "h" %<-% Var ("Ingress",8) %:%
    mkWhile (Var ("h",8) %<>% Var ("Egress",8)) loop_body

let test1 = string_of_cmd simple_test

let test2 = wp `Negs ("h" %<-% Var ("Ingress",8)) True

(* let complete_test_with_drop_location_no_holes =
 *   While(!%(LocEq 1) %&% !%(LocEq (-1)),
 *         mkPartial
 *           [ LocEq 0 %&% (Var ("pkt",8) %=% Value (Int (42,8))) ,  SetLoc 1 %:% ("pkt" %<-% Value (Int (47,8)))
 *           ; LocEq 0 %&% !%(Var ("pkt",8) %=% Value (Int (42,8))), SetLoc (-1) ]
 *        ) *)

(* let complete_test_with_drop_location_holes =
 *   let pkt_eq h = Var ("pkt",8) %=% Hole (h,8) in
 *   let pkt_gets h = "pkt" %<-% Hole (h,8) in
 *   SetLoc 0 %:%
 *   While(!%(LocEq 1) %&% !%(LocEq (-1)),
 *         mkPartial
 *           [ LocEq 0 %&% (pkt_eq "_0")  , SetLoc 1 %:% pkt_gets "_1"
 *           ; LocEq 0 %&% !%(pkt_eq "_0"), SetLoc (-1)]
 *        ) *)



let%test _ = (* Testing unrolling *)
  unroll 1 simple_test = "h" %<-% Var ("Ingress",8) %:%
                          mkSelect Partial [Var ("h",8) %<>% Var ("Egress",8) , loop_body]

let%test _= (* testing loop removal when n = 0*)
  unroll 0 simple_test = Seq(Assign("h",Var("Ingress",8)), Skip)

let%test _ = (* One unroll works *)
  let cond = Var ("h",8) %<>% Var ("Egress",8) in
  let input = ("h" %<-% Var ("Ingress",8)) %:% mkWhile cond loop_body in
  unroll 1 input
  = ("h" %<-% Var ("Ingress",8)) %:%  mkSelect Partial [cond , loop_body %:% Skip]


let%test _ = (*Sequencing unrolls works*)
  let cond = Var ("h",8) %<>% Var ("Egress",8) in
  unroll 1 (mkWhile cond loop_body %:% mkWhile cond loop_body)
  = unroll 1 (mkWhile cond loop_body)
    %:% unroll 1 (mkWhile cond loop_body)

let%test _ = (*Selection unrolls works*)
  let cond = Var ("h",8) %<>% Var ("Egress",8) in
  let selectCond = Var ("h",8) %=% mkVInt ((5,8)) in
  unroll 1 (mkSelect Total [ selectCond, mkWhile cond loop_body
                           ; True, mkWhile cond loop_body])
  = mkSelect Total [selectCond, unroll 1 (mkWhile cond loop_body)
                   ; True, unroll 1 (mkWhile cond loop_body)]

(* Testing equality smart constructor *)
let%test _ =
  let exp = Var ("x",8) %=%  mkVInt(7,8) in
  let got = mkVInt (7,8) %=% Var ("x",8) in
  if exp = got then true else
    (print_test_neq ~got ~exp;false)




(* Weakest precondition testing *)
let%test _ = (*Skip behaves well *)
  wp `Negs Skip True = True
  && wp `Negs Skip (Var ("x",8) %=% Var ("y",8)) = Var ("x",8) %=% Var ("y",8)

let%test _ = (* Assign behaves well with integers *)
  let prog = "h" %<-% mkVInt (7,8) in
  (* Printf.printf "%s\n" (string_of_test (wp prog (Var "h" %=% Int 7))); *)
  (* Int 7 %=% Int 7 = wp prog (Var "h" %=% Int 7) *)
  if mkVInt(7,8) %=% Var ("g",8) = wp `Negs prog (Var ("h",8) %=% Var ("g",8)) then true else
    (print_test_neq
       ~exp:(mkVInt (7,8) %=% Var ("g",8))
       ~got:(wp `Negs prog (Var ("h",8) %=% Var ("g",8)))
    ; false)




let%test _ = (* Assign behaves well with variables *)
  let prog = "h" %<-% Var ("hgets",8) in
  let wphEQ x = wp `Negs prog (Var ("h",8) %=% x) in
  Var ("hgets",8) %=% mkVInt (7,8) = wphEQ (mkVInt (7,8))
  && Var ("hgets",8) %=% Var ("g",8) = wphEQ (Var ("g",8))


let%test _ =
  let prog = mkSelect Total [
                 Var ("h",8) %=% Var ("g",8), "g" %<-% mkVInt (8,8)
               ] in
  let prec = wp `Negs prog (Var ("g",8) %=% mkVInt (8,8)) in
  let exp  = Var ("h",8) %=% Var ("g",8) in
  (if prec <> exp then print_test_neq ~got:prec ~exp:exp);
  prec = exp

let%test _ = (* wp behaves well with selects *)
  let prog = mkSelect Total [ Var ("h",8) %=%  Var ("g",8) , "g" %<-% mkVInt (8,8)
                            ; Var ("h",8) %=%  mkVInt (99,8)  , "h" %<-% mkVInt (4,8)
                            ; Var ("h",8) %<>% mkVInt (2,8)   , "h" %<-% Var ("g",8)
                            ] in
  let comp = wp `Negs prog (Var ("g",8) %=% mkVInt (8,8)) in
  let all_conds =
    (Var ("h",8) %=%  Var ("g",8))
    %+% (Var ("h",8) %=%  mkVInt (99,8))
    %+% (Var ("h",8) %<>% mkVInt (2,8))
  in
  let all_imps =
    ((Var ("h",8) %=%  Var ("g",8)) %=>% (mkVInt (8,8)  %=% mkVInt (8,8)))
    %&% ((Var ("h",8) %=%  mkVInt (99,8))  %=>% (Var ("g",8) %=% mkVInt (8,8)))
    %&% ((Var ("h",8) %<>% mkVInt (2,8))   %=>% (Var ("g",8) %=% mkVInt (8,8)))
  in
  let exp = all_conds %&% all_imps in
  (if comp <> exp then print_test_neq ~got:comp ~exp:exp );
  comp = exp



let%test _ = (* wp behaves well with sequence *)
  let prog = ("h" %<-% mkVInt (10,8)) %:% ("h" %<-% mkVInt (80,8)) in
  let cond = Var ("h",8) %=% Var ("g",8) in
  mkVInt (80,8) %=% Var ("g",8) = wp `Negs prog cond

let%test _ = (* wp behaves well with assertions *)
  let asst = (Var ("h",8) %<>% mkVInt (10,8)) %&% (Var ("h",8) %<>% mkVInt (15,8)) in
  let prog = Assert(asst) in
  let phi =  Var ("h",8) %=% Var ("g",8) in
  wp `Negs prog phi = asst %&% phi

let%test _ = (* wp behaves well with partials *)
  let cond = Var ("pkt",8) %=% mkVInt (101,8) in
  let prog = mkSelect Partial [ Var ("pkt",8) %=% Hole ("_hole0",8), "pkt" %<-% Hole ("_hole1",8) ] in
  let exp = Var ("pkt",8) %=% Hole ("_hole0",8) %=>% (Hole ("_hole1",8) %=% mkVInt (101,8)) in
  let got = wp `Negs prog cond in
  if exp = got then true else(
    print_test_neq ~exp ~got;
    false
  )


let%test _ = (* wp behaves well with totals *)
  let cond = Var ("pkt",8) %=% mkVInt (101,8) in
  let prog = mkSelect Total [ Var ("pkt",8) %=% Hole ("_hole0",8), "pkt" %<-% Hole ("_hole1",8) ] in
  let exp = (Var ("pkt",8) %=% Hole ("_hole0",8)) %&% (Var ("pkt",8) %=% Hole ("_hole0",8) %=>% (Hole ("_hole1",8) %=% mkVInt (101,8))) in
  let got = wp `Negs prog cond in
  if exp = got then true else(
    print_test_neq ~exp ~got;
    false
  )

let%test _ = (* wp behaves well with ordereds *)
  let cond = Var ("pkt",8) %=% mkVInt (101,8) in
  let prog = mkSelect Ordered [ Var ("pkt",8) %=% Hole ("_hole0",8), "pkt" %<-% Hole ("_hole1",8)
                              ; Var ("pkt",8) %=% mkVInt (99,8), "pkt" %<-% mkVInt (101,8)] in
  let exp = (Var ("pkt",8) %=% Hole  ("_hole0",8) %=>% (Hole ("_hole1",8) %=% mkVInt (101,8)))
             %&% ((Var ("pkt",8) %=% mkVInt (99,8) %&% (Var ("pkt",8) %<>% Hole ("_hole0",8))) %=>% (mkVInt (101,8) %=% mkVInt (101,8)))
  in
  let got = wp `Negs prog cond in
  (* Printf.printf "EXPECTED:\n%s\n\nGOT:\n%s\n" (sexp_string_of_test expected) (sexp_string_of_test pre); *)
  if exp = got then true else (
    print_test_neq ~exp ~got;
    false
  )

let%test _ =
  let i x = mkVInt (x,2) in
  let v s = Var (s, 2) in
  let s x = v x %=% v (symbolize x) in
  let dsteq x = (Var ("dst", 2) %=% i x) in
  let setOut x = "out" %<-% i x in
  Printf.printf "TEST %s\n"
    (wp `Negs (mkPartial
               [ dsteq 2, setOut 2
               ; dsteq 1, setOut 1
               ; dsteq 3, setOut 3
               ; True, setOut 0 ])
       (s "dst" %&% s "out")
     |> string_of_test);
  true



                           (* TESTING SEMANTICS *)

let test_trace p_string expected_trace =
  let p = parse p_string in
  let pkt = Packet.(set_field empty "pkt" (mkInt (100,8))) in
  let loc = Some 0 in
  match trace_eval_inst p Instance.empty ~wide:StringMap.empty (pkt, loc) with
  | _, tr,_ ,_ -> tr = expected_trace



let%test _ =
  let dst_is x = Var("dst", 2) %=% mkVInt(x,2) in
  let src_is x = Var("src", 2) %=% mkVInt(x,2) in
  let _ : Ast.cmd =
    mkOrdered [
        src_is 1, "dst" %<-% mkVInt(2,2);
        src_is 0, "smac" %<-% mkVInt(1,2);
        True, Skip
      ]
    %:%
      mkOrdered [
          dst_is 0, "out" %<-% mkVInt(1,2);
          dst_is 1, "out" %<-% mkVInt(2,2);
          True, Skip ]
  in
  let _ : Ast.cmd =
    mkOrdered [
       src_is 0 %&% dst_is 0, sequence["dst"%<-% mkVInt(1,2); "out" %<-% mkVInt(1,2)];
        Var("src",2) %=% mkVInt(1,2) %&% (Var("dst",1) %=% mkVInt(0,2)), "dst" %<-% mkVInt(0,2)
      ]
  in
  true
