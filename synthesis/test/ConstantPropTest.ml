open Alcotest
open Avenir
open Ast
open ConstantProp
open Equality



(*Testing Constant Propogation *)
let cp_wikipedia_ex1 _ =
  let cmd =
    sequence [
        "x" %<-% mkVInt(14,32);
        "y" %<-%
          mkMinus (mkTimes (mkVInt(2,32)) (mkVInt(7,32))) (Var("x",32));
        "return" %<-% mkTimes (Var("y",32)) (mkPlus (mkVInt(99,32)) (mkVInt(2,32)))
      ]
  in
  let exp = sequence[
                "x" %<-% mkVInt(14,32);
                "y" %<-% mkVInt(0,32);
                "return" %<-% mkVInt(0,32)
              ] in
  same_cmd exp (propogate_fix cmd)

let cp_wikipedia_ex2 _ =
  let int i = mkVInt(i,32) in
  let var a = Var(a,32) in
  let cmd =
    sequence [
        "a" %<-% int 30;
        "b" %<-% mkMinus (int 9) (mkMinus(var "a") (int 24));
        "c" %<-% mkTimes (var "b") (int 4);
        mkOrdered [
            var "c" %>% int 10, "c" %<-% mkMinus (var "c") (int 10);
            True, Skip
          ];
        "return" %<-% mkTimes (var "c") (int 2)
      ]
  in
  let exp =
    sequence [
        "a" %<-% int 30;
        "b" %<-% int 3;
        "c" %<-% int 12;
        "c" %<-% int 2;
        "return" %<-% int 4
      ]
  in
  same_cmd exp (propogate_fix cmd)


let cp_only_holes_and_constants _ =
  let cmd =
    sequence [
        "ipv4.dst" %<-% mkVInt(9999,32);
        "ipv4.ttl" %<-% mkVInt(12,8);
        mkOrdered [
            (Hole("?Del_0_fwd_table", 1) %=% mkVInt(0,1)) %&%
              (Var("ipv4.dst",32) %=% mkVInt(3333,32)), "out_port" %<-% mkVInt(45,9);
            (Hole("?AddTo_fwd_table",1) %=% mkVInt(1,1))
            %&% (Var("ipv4.dst",32) %=% Hole("?ipv4.dst_fwd_table", 32))
            , "out_port" %<-% Hole("?port_0_fwd_table",32);
            True, "out_port" %<-% mkVInt(0,9)
          ];
        mkOrdered [
            bigand [
                Hole("?Del_0_punt_table",1) %=% mkVInt(0,1);
                Var("ipv4.ttl",8) %=% mkVInt(1,8)
              ], "out_port" %<-% mkVInt(0,9);
            bigand [
                Hole("?AddTo_punt_table",1) %=% mkVInt(1,1);
                Var("ipv4.ttl",8) %=% Hole("?ipv4.ttl_punt_table",8)
              ], "out_port" %<-% Hole("?port_0_punt",9);
            True, Skip
          ]
      ]
  in
  let exp =
    sequence [
        "ipv4.dst" %<-% mkVInt(9999,32);
        "ipv4.ttl" %<-% mkVInt(12,8);
        mkOrdered [
            (Hole("?AddTo_fwd_table",1) %=% mkVInt(1,1))
            %&% (mkVInt(9999,32) %=% Hole("?ipv4.dst_fwd_table", 32))
          , "out_port" %<-% Hole("?port_0_fwd_table",32);
            True, "out_port" %<-% mkVInt(0,9)
          ];
        mkOrdered [
            bigand [
                Hole("?AddTo_punt_table",1) %=% mkVInt(1,1);
                mkVInt(12,8) %=% Hole("?ipv4.ttl_punt_table",8)
              ], "out_port" %<-% Hole("?port_0_punt",9);
            True, Skip
          ]
      ]
  in
  same_cmd exp (propogate_fix cmd)


let cp_also_props_copies _ =
  let var x = Var(x,32) in
  let int i = mkVInt(i,32) in
  let cmd =
    sequence [
        "y" %<-% mkPlus (var "x") (int 1);
        "z" %<-% mkPlus (var "y") (var "x");
      ] in
  let exp =
    sequence [
        "y" %<-% mkPlus (var "x") (int 1);
        "z" %<-% mkPlus (mkPlus (var "x") (int 1)) (var "x");
      ] in
  same_cmd exp (propogate_fix cmd)


let cp_affects_actions_and_keys _ =
  let var x = Var(x,32) in
  let int i = mkVInt(i,32) in
  let cmd =
    sequence [
        "z" %<-% int 99;
        "out" %<-% int 0;
        "meta" %<-% var "addr";
        Apply {name = "tbl";
               keys = [("z",32,None); ("meta",32,None)];
               actions = [ "action", ["port",9], sequence [
                                           "out" %<-% mkPlus (Var("port",9)) (mkCast 9 @@ var "z");
                                           "meta" %<-% mkPlus (var "z") (var "z");
                                         ]
                         ; "action", ["port",9], sequence [
                                           "out" %<-% mkVInt (0,9);
                                           "meta" %<-% int (2*99);
                         ] ];
               default = "meta" %<-% mkTimes (var "z") (int 2);
          };
        "addr" %<-% var "meta";
      ]

  in
  let exp =
    sequence [
        "z" %<-% int 99;
        "out" %<-% int 0;
        "meta" %<-% var "addr";
        Apply {name = "tbl";
               keys = [("z",32,Some (Value.make (99,32))); ("addr",32,None)];
               actions = [ "action",
                           ["port",9],
                           sequence [
                               "out" %<-% mkPlus (Var("port",9)) (mkVInt(99,9));
                               "meta" %<-% int (198);
                             ]
                         ; "action", ["port",9], sequence [
                                           "out" %<-% mkVInt (0,9);
                                           "meta" %<-% int (198);
                         ] ];
               default = "meta" %<-% int 198;
          };
        "addr" %<-% int 198;
      ]
  in
  same_cmd exp @@ propogate cmd

let opt_bcm_example _ =
  let bcm =
    sequence [
        "x" %<-% mkVInt(333,32);
        "o" %<-% mkVInt(0,9);
        mkOrdered [
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(0,2)
              ], "meta" %<-% mkVInt(99,32);
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(1,2)
              ], sequence [
                     "o" %<-% Hole("?outtie_1_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(2,2)
              ], sequence [
                     "o" %<-% Hole("?outtie_2_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            True, Skip
          ];
        mkOrdered [
            Var("o", 9) %=% mkVInt(0,9),
            sequence [
                "o" %<-% mkVInt(0,9);
                "meta" %<-% mkVInt(0,9);
                "x" %<-% mkVInt(0,32);
              ];
            True, Skip
          ]
      ]
  in
  let expected =
    sequence [
        "x" %<-% mkVInt(333,32);
        "o" %<-% mkVInt(0,9);
        mkOrdered [
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% mkVInt(333,32);
                    Hole("?Acttbl",1) %=% mkVInt(0,2)
              ], "meta" %<-% mkVInt(99,32);
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% mkVInt(333,32);
                    Hole("?Acttbl",1) %=% mkVInt(1,2)
              ], sequence [
                     "o" %<-% Hole("?outtie_1_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% mkVInt(333,32);
                    Hole("?Acttbl",1) %=% mkVInt(2,2)
              ], sequence [
                     "o" %<-% Hole("?outtie_2_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            True, Skip
          ];
        mkOrdered [
            Var("o", 9) %=% mkVInt(0,9),
            sequence [
                "o" %<-% mkVInt(0,9);
                "meta" %<-% mkVInt(0,9);
                "x" %<-% mkVInt(0,32);
              ];
            True, Skip
          ]
      ]
  in
  same_cmd expected (propogate bcm)

let passive_propogation_learns_from_tests1 _ =
  let open Util in
  let cmd =
    sequence [
        mkAssume(Hole("?dst_addr_0_ipv4_rewrite",48) %=% mkVInt(0,48));
        mkAssume(Var("standard_metadata.egress_spec$2",9) %=% mkVInt(1,9))
      ]
  in
  let map = StringMap.of_alist_exn ["standard_metadata.egress_spec$2", mkVInt(1,9)] in
  let (map, _) = ConstantProp.passive_propogate_aux `Rev map cmd in
  match StringMap.find map "standard_metadata.egress_spec$2" with
  | None -> Alcotest.(check string) "Should have found" "Some 1" "None"
  | Some e ->
     same_expr (mkVInt(1,9)) e

let passive_propogation_learns_from_tests2 _ =
  let open Util in
  let cmd =
    mkAssume(Var("standard_metadata.egress_spec$2",9) %=% mkVInt(1,9))
  in
  let (map, _) = ConstantProp.passive_propogate_aux `Rev (StringMap.empty) cmd in
  match StringMap.find map "standard_metadata.egress_spec$2" with
  | None -> Alcotest.(check string) "Should have found" "Some 1" "None"
  | Some e ->
     same_expr (mkVInt(1,9)) e


let passive_propogation_learns_disjoint _ =
  let open Util in
  let cmd =
    mkOrdered [
        bigand [
            Hole("?AddRowToipv4_fwd",1) %=% mkVInt(1,1);
            Hole("?hdr.ipv4.dstAddr_ipv4_fwd",32) %=% mkVInt(0,32);
            Hole("?ActInipv4_rewrite",2) %=% mkVInt(0,2)
          ], sequence [
                 mkAssume(Hole("?dst_addr_0_ipv4_rewrite",48) %=% mkVInt(0,48));
                 mkAssume(Var("standard_metadata.egress_spec$2",9) %=% mkVInt(1,9));
               ];
        bigand[
            Hole("?AddRowToipv4_fwd",1) %=% mkVInt(1,1);
            Hole("?hdr.ipv4.dstAddr_ipv4_fwd",32) %=% mkVInt(0,0);
            Hole("?ActInipv4_rewrite",2) %=% mkVInt(1,2)
          ], mkAssume(Var("standard_metadata.egress_spec$2",9) %=% mkVInt(1,9))
      ]
  in
  let (map, _) = ConstantProp.passive_propogate_aux `Rev (StringMap.empty) cmd in
  match StringMap.find map "standard_metadata.egress_spec$2" with
  | None -> Alcotest.(check string) "Should have found" "Some 1" "None"
  | Some e ->
     same_expr (mkVInt(1,9)) e

let meet_combines_facts _ =
  let open Util in
  let map =
    ConstantProp.meet
      (StringMap.of_alist_exn ["standard_metadata.egress_spec$2", mkVInt(1,9)])
      (StringMap.of_alist_exn ["standard_metadata.egress_spec$2", mkVInt(1,9)])
  in
  match StringMap.find map "standard_metadata.egress_spec$2" with
  | None -> Alcotest.(check string) "Should have found" "Some 1" "None"
  | Some e ->
     same_expr (mkVInt(1,9)) e

let test : unit test_case list =
  [test_case "straight line code" `Quick cp_wikipedia_ex1;
   test_case "eliminate if" `Quick cp_wikipedia_ex2;
   test_case "only holes, constants, and output vars remain in tests" `Quick cp_only_holes_and_constants;
   test_case "cp_also_props_copies" `Quick cp_also_props_copies;
   test_case "opt_bcm_example" `Quick opt_bcm_example;
   test_case "cp_affects_actions_and_keys" `Quick cp_affects_actions_and_keys;
  ]


let test_passive_prop : unit test_case list =
  [test_case "learns from tests 1" `Quick passive_propogation_learns_from_tests1;
   test_case "learns from tests 2" `Quick passive_propogation_learns_from_tests2;
   test_case "learns facts common to disjoint paths" `Quick passive_propogation_learns_disjoint]

let test_meet : unit test_case list =
  [test_case "combines facts" `Quick meet_combines_facts]
