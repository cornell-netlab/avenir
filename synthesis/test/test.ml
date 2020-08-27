open Core
open Avenir
open Ast

let testable_string (type a) (f : a -> string) (eq : a -> a -> bool) =
  Alcotest.testable (Fmt.of_to_string f) (eq)

let expr = testable_string string_of_expr Stdlib.(=)
let same_expr = Alcotest.(check expr) "same expr"

let test = testable_string string_of_test Stdlib.(=)
let same_test = Alcotest.(check test) "same test"

let cmd = testable_string string_of_cmd Stdlib.(=)
let same_cmd = Alcotest.(check cmd) "same cmd"

let packet = testable_string Packet.string__packet Packet.equal
let same_packet = Alcotest.(check packet) "same packet"


let model = testable_string string_of_map (Util.StringMap.equal veq)
let same_model = Alcotest.(check model) "same model"

(* Testing equality smart constructor *)
let eq_test _ =
  let exp = Var ("x",8) %=%  mkVInt(7,8) in
  let got = mkVInt (7,8) %=% Var ("x",8) in
  same_test exp got


(*Testing weakest preconditions*)
let wp_skip_eq _ =
  let open Manip in
  let phi = (Var("x",8) %=% Var("y",8)) in
  same_test phi (wp `Negs Skip phi)

let wp_int_assign_eq _ =
  let open Manip in
  let pre = mkVInt(7,8) %=% Var ("g",8) in
  let cmd = "h" %<-% mkVInt(7,8) in
  let post = Var("h",8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_var_assign_eq _ =
  let open Manip in
  let pre = Var("hgets",8) %=% Var ("g",8) in
  let cmd = "h" %<-% Var("hgets",8) in
  let post = Var("h",8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_ordered_eq _ =
  let open Manip in
  let post = Var("g",8) %=% mkVInt(8,8) in
  let pre =
    bigand [
        bigand [
            Var ("h",8) %<>% mkVInt (2,8);
            mkNeg @@ bigor [Var("h",8) %=% mkVInt(99,8);
                            Var("h",8) %=% Var("g",8)] ;
          ] %=>% post;
        bigand [
            Var("h",8) %=%  mkVInt(99,8);
            Var("h",8) %<>% Var("g",8)
        ] %=>% post;
        Var("h",8) %=% Var ("g",9) %=>% (mkVInt(8,8) %=% mkVInt(8,8));
      ]
  in
  let cmd =
    mkOrdered [ Var ("h",8) %=%  Var ("g",8)    , "g" %<-% mkVInt (8,8);
                Var ("h",8) %=%  mkVInt (99,8)  , "h" %<-% mkVInt (4,8);
                Var ("h",8) %<>% mkVInt (2,8)   , "h" %<-% Var    ("g",8)
      ]
  in
  let post = Var("g",8) %=% mkVInt(8,8) in
  same_test pre (wp `Negs cmd post)

let wp_seq_eq _ =
  let open Manip in
  let cmd = ("h" %<-% mkVInt (10,8)) %:% ("h" %<-% mkVInt (80,8)) in
  let post = Var ("h",8) %=% Var ("g",8) in
  let pre = mkVInt (80,8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_assume_eq _ = (* wp behaves well with assertions *)
  let open Manip in
  let phi = bigand [Var ("h",8) %<>% mkVInt (10,8); Var ("h",8) %<>% mkVInt (15,8)] in
  let cmd = Assume(phi) in
  let post =  Var ("h",8) %=% Var ("g",8) in
  same_test (phi %=>% post) (wp `Negs cmd post)

(*Semantics*)
let semantics =
  let open Semantics in
  let dst_is x = Var("dst", 2) %=% mkVInt(x,2) in
  let src_is x = Var("src", 2) %=% mkVInt(x,2) in
  let prog =
    sequence [
    mkOrdered [
        src_is 1, "smac" %<-% mkVInt(2,2);
        src_is 0, "smac" %<-% mkVInt(1,2);
        True, Skip
      ];
      mkOrdered [
          dst_is 1, "out" %<-% mkVInt(2,2);
          dst_is 0, "out" %<-% mkVInt(1,2);
          True, Skip ]
      ]
  in
  let update f j pkt = Packet.set_field pkt f (mkInt(j,2)) in
  let mkPacket i j = Packet.empty |> update "src" i |> update "dst" j in
  let update f j pkt = Packet.set_field pkt f (mkInt(j,2)) in
  let pkt00 = mkPacket 0 0 in
  let pkt01 = mkPacket 0 1 in
  let pkt10 = mkPacket 1 0 in
  let pkt11 = mkPacket 1 1 in
  let run = eval_act prog in
  [ run pkt00, pkt00 |> update "smac" 1 |> update "out" 1;
    run pkt01, pkt01 |> update "smac" 1 |> update "out" 2;
    run pkt10, pkt10 |> update "smac" 2 |> update "out" 1;
    run pkt11, pkt11 |> update "smac" 2 |> update "out" 2;
  ]


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
  same_cmd exp (ConstantProp.propogate_fix cmd)

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
  same_cmd exp (ConstantProp.propogate_fix cmd)


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
  same_cmd exp (ConstantProp.propogate_fix cmd)


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
  same_cmd exp (ConstantProp.propogate_fix cmd)


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
               actions = [ ["port",9], sequence [
                                           "out" %<-% mkPlus (Var("port",9)) (mkCast 9 @@ var "z");
                                           "meta" %<-% mkPlus (var "z") (var "z");
                                         ]
                         ; ["port",9], sequence [
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
               keys = [("z",32,Some (mkInt(99,32))); ("addr",32,None)];
               actions = [ ["port",9],
                           sequence [
                               "out" %<-% mkPlus (Var("port",9)) (mkVInt(99,9));
                               "meta" %<-% int (198);
                             ]
                         ; ["port",9], sequence [
                                           "out" %<-% mkVInt (0,9);
                                           "meta" %<-% int (198);
                         ] ];
               default = "meta" %<-% int 198;
          };
        "addr" %<-% int 198;
      ]
  in
  same_cmd exp @@ ConstantProp.propogate cmd


let dc_remove_table _ =
  let var x = Var(x,32) in
  let int i = mkVInt(i,32) in
  let cmd =
    sequence [
        Apply {name = "tbl";
               keys = [("z",32, None); ("meta",32,None)];
               actions = [ ["port",9], sequence [
                                           "out" %<-% mkPlus (Var("port",9)) (mkCast 9 @@ var "z");
                                           "meta" %<-% mkPlus (var "z") (var "z");
                                         ]
                         ; ["port",9], sequence [
                                           "out" %<-% mkVInt (0,9);
                                           "meta" %<-% int (2*99);
                         ] ];
               default = "meta" %<-% mkTimes (var "z") (int 2);
          };
        "addr" %<-% var "meta";
      ]
  in
  let exp = Skip in
  same_cmd exp @@ DeadCode.elim_vars ["mac",48] cmd



let opts_minimize _ =
  let fvs = ["ipv4.dst", 32; "ipv4.ttl", 8; "out_port",9 ] in
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
  same_cmd exp (CompilerOpts.optimize fvs cmd)


let opt_bcm_example _ =
  let bcm =
    sequence [
        "x" %<-% mkVInt(333,32);
        "o" %<-% mkVInt(0,9);
        mkOrdered [
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(0,1)
              ], "meta" %<-% mkVInt(99,32);
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(1,1)
              ], sequence [
                     "o" %<-% Hole("?outtie_1_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% Var("x",32);
                    Hole("?Acttbl",1) %=% mkVInt(2,1)
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
                    Hole("?Acttbl",1) %=% mkVInt(0,1)
              ], "meta" %<-% mkVInt(99,32);
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% mkVInt(333,32);
                    Hole("?Acttbl",1) %=% mkVInt(1,1)
              ], sequence [
                     "o" %<-% Hole("?outtie_1_tbl", 9);
                     "meta" %<-% mkVInt(44,32)
                   ];
            bigand [Hole("?Addtbl",1) %=% mkVInt(1,1);
                    Hole("?x",32) %=% mkVInt(333,32);
                    Hole("?Acttbl",1) %=% mkVInt(2,1)
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
  same_cmd expected (ConstantProp.propogate bcm)


let rev_propogate_computes_single_path _ =
  let var x = Var (x, 32) in
  let int i = mkVInt(i,32) in
  let vareq x e = var x %=% e in
  let assign x e = Assume (vareq x e) in
  let cmd =
    sequence [
        assign "port$1" (int 0);
        mkOrdered [
            bigand [Hole("?AddRowtoipv4",1) %=% mkVInt(1,1);
                    Hole("?ipv4.dst_ipv4",32) %=% mkVInt(5,32);
                    Hole("?ActInipv4",2) %=% mkVInt(0,2)
              ], assign "port$2" @@ Hole("?port_ipv4_0",32);

            True, assign "port$2" @@ var "port$1";
          ];
        mkOrdered [
            !%(vareq "port$2" @@ mkVInt (0,32)),
            sequence [
                assign "ipv4.dst$1" @@ mkVInt(0,32);
                assign "port$3" @@ mkVInt(0,32)
              ];
            True,
            sequence [
                assign "ipv4.dst$1" @@ mkVInt(5,32);
                assign "port$3" @@ var "port$2"
              ]
          ];
      ]
  in
  let out_packet =
    Util.StringMap.of_alist_exn
      [ "port$3", mkVInt(99,32);
        "ipv4.dst$1", mkVInt(5,32);
      ]
  in
  same_cmd (mkAssume(bigand [
                      Hole("?AddRowtoipv4",1) %=% mkVInt(1,1);
                      Hole("?ipv4.dst_ipv4",32) %=% mkVInt(5,32);
                      Hole("?ActInipv4",2) %=% mkVInt(0,2)])
            %:% mkAssume(Hole("?port_ipv4_0",32) %=% mkVInt(99,32)))
  @@ CompilerOpts.passive_optimize out_packet cmd


let construct_model_query_PA_is_sat1 _ =
  let inpkt = Packet.mk_packet_from_list
                ["ipv4.dst", mkInt(5,32);
                 "port", mkInt(8,9)] in
  let outpkt = Packet.mk_packet_from_list
                 ["ipv4.dst", mkInt(5,32);
                  "port", mkInt(99,9)] in
  let fvs = ["port", 9; "ipv4.dst", 32] in
  let phys =
    sequence [
        "port" %<-% mkVInt(0,9);
        "ipv4_action_run" %<-% mkVInt(0,1);
        "exit" %<-% mkVInt(0,1);
        mkOrdered [
            Var("exit",1) %<>% mkVInt(1,1),
            mkOrdered [
                bigand [
                    Hole("?AddRowtoipv4", 1) %=% mkVInt(1,1);
                    Hole("?ipv4.dst_ipv4", 32) %=% Var("ipv4.dst", 32);
                    Hole("?ActInipv4",1) %=% mkVInt(0,1)
                  ], sequence [
                         "port" %<-% Hole("?port_tbl_0", 9);
                         "ipv4_action_run" %<-% mkVInt(1,1)
                       ];
                True, Skip
              ];
            True, Skip
          ];
        mkOrdered [
            Var("port",9) %=% mkVInt(0,9),
            sequence [
                "ipv4.dst" %<-% mkVInt (0,0);
                "port" %<-% mkVInt(0,0)
              ];
            True, Skip
          ]
      ]
  in
  let query = ModelFinder.construct_model_query `PassiveAggressive fvs inpkt phys outpkt in
  let res = Prover.is_sat Parameters.default query in
  Alcotest.(check bool) "is true" true res


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


let construct_model_query_PA_is_sat_hello _ =
  let set_port e = "standard_metadata.egress_spec" %<-% e in
  let drop = set_port @@ mkVInt(0,9) in
  let ipv4_tbl =
    let add = Hole("?AddRowtoipv4_fwd",1) %=% mkVInt(1,1)in
    let key = Hole("?hdr.ipv4.dstAddr_ipv4_fwd",32)
              %=% Var("hdr.ipv4.dstAddr",32) in
    let act i = Hole("?ActInipv4_fwd",2) %=% mkVInt(i,2) in
    let actrun i = "ipv4_fwd_action_run" %<-% mkVInt(i,3) in
    mkOrdered [
        bigand [add;key;act 0],
        sequence [actrun 1; set_port (Hole("?port_0_ipv4_fwd",9))];
        bigand [add;key;act 1],
        sequence [actrun 2; drop];
        bigand [add;key;act 2],
        sequence [actrun 3];
        True, drop;
      ]
  in
  let ipv4_rewrite_table =
    let add = Hole("?AddRowtoipv4_rewrite",1) %=% mkVInt(1,1)in
    let key = Hole("?hdr.ipv4.dstAddr_ipv4_rewrite",32)
              %=% Var("hdr.ipv4.dstAddr",32) in
    let act i = Hole("?ActInipv4_rewrite",2) %=% mkVInt(i,2) in
    let actrun i = "ipv4_rewrite_action_run" %<-% mkVInt(i,3) in
    mkOrdered [
        bigand [add;key;act 0],
        sequence [actrun 1;
                  "hdr.ethernet.srcAddr"
                  %<-% Var("hdr.ethernet.dstAddr",48);
                  "hdr.ethernet.dstAddr"
                  %<-% Hole("?dstAddr_0_ipv4_rewrite",48);
                  "hdr.ipv4.ttl" %<-% mkMinus(Var("hdr.ipv4.ttl",8))(mkVInt(1,8))
          ];
        bigand [add;key;act 1],
        sequence [actrun 2; drop ];
        bigand [add;key;act 2],
        sequence [actrun 3];
        True, drop;
      ]
  in
  let hello_phys =
    sequence [
        "exit" %<-% mkVInt(0,1);
        "ipv4_fwd_action_run" %<-% mkVInt(0,3);
        "ipv4_fwd_rewrite_action_run" %<-% mkVInt(0,3);
        "return3" %<-% mkVInt(0,1);
        "return4" %<-% mkVInt(0,1);
        "standard_metadata.egress_port" %<-% mkVInt(0,9);
        "return4" %<-% mkVInt(0,1);
        mkOrdered [
            Var("hdr.ipv4_valid",1) %=% mkVInt(1,1),
            sequence["ipv4_fwd_action_run"%<-% mkVInt(0,3);
                     ipv4_tbl;
                     mkOrdered [
                         Var("exit",1) %=% mkVInt(0,1),
                         mkOrdered [
                             Var("return4",1) %=% mkVInt(0,1),
                             sequence [
                                 "ipv4_rewrite_action_run" %<-% mkVInt(0,3);
                                 ipv4_rewrite_table;
                               ];
                             True, Skip
                           ];
                         True, Skip
                       ];
              ];
            True, Skip
          ];
        "standard_metadata.egress_port"
        %<-% Var("standard_metadata.egress_spec",9);
        mkOrdered [
            Var("standard_metadata.egress_spec",9) %<>% mkVInt(0,9),
            "return3" %<-% mkVInt(0,1);
            True, Skip
          ];
        mkOrdered [
            Var("standard_metadata.egress_spec",9) %=% mkVInt(0,9),
            sequence [
                "hdr.ipv4.valid" %<-% mkVInt(0,1);
                "hdr.ethernet.srcAddr" %<-% mkVInt(0,48);
                "hdr.ethernet.dstAddr" %<-% mkVInt(0,48);
                "hdr.ipv4.ttl" %<-% mkVInt(0,8);
                "hdr.ipv4.dstAddr" %<-% mkVInt(0,32);
                "hdr.ipv4.srcAddr" %<-% mkVInt(0,32);
                "standard_metadata.egress_spec" %<-% mkVInt(0,9)
              ];
            True,Skip;
          ]
      ]
  in
  let fvs = [
      "standard_metadata.egress_spec",9;
      "hdr.ipv4_valid",1;
      "hdr.ethernet.srcAddr",48;
      "hdr.ethernet.dstAddr",48;
      "hdr.ipv4.ttl", 8;
      "hdr.ipv4.dstAddr",32;
      "hdr.ipv4.srcAddr",32;
      "standard_metadata.egress_spec",9;
    ]
  in
  let inpkt = Packet.mk_packet_from_list
                [ "hdr.ethernet.dstAddr", Int(Bigint.of_string "0x287bb416626", 48);
                  "hdr.ethernet.srcAddr", Int(Bigint.of_string "0xffffffffffff", 48);
                  "hdr.ipv4.dstAddr", Int(Bigint.of_string "0x0", 32);
                  "hdr.ipv4.srcAddr", Int(Bigint.of_string "0xabd62d8b", 32);
                  "hdr.ipv4.ttl", Int(Bigint.of_string "0x65", 8);
                  "hdr.ipv4_valid", Int(Bigint.of_string "0x1", 1);
                  "standard_metadata.egress_spec", Int(Bigint.of_string "0x1ff",9)] in
  let outpkt =
    Packet.mk_packet_from_list
      ["hdr.ethernet.dstAddr", Int(Bigint.of_string "0x0",48);
       "hdr.ethernet.srcAddr", Int(Bigint.of_string "0x287bb416626",48);
       "hdr.ipv4.dstAddr", Int(Bigint.of_string "0x0",32);
       "hdr.ipv4.srcAddr", Int(Bigint.of_string "0xabd62d8b",32);
       "hdr.ipv4.ttl", Int(Bigint.of_string "0x64",8);
       "hdr.ipv4_valid", Int(Bigint.of_string "0x1",1);
       "ipv4_lpm_action_run", Int(Bigint.of_string "0x1",1);
       "return1", Int(Bigint.of_string "0x0",1);
       "return2", Int(Bigint.of_string "0x0",1);
       "standard_metadata.egress_port", Int(Bigint.of_string "0x1",9);
       "standard_metadata.egress_spec", Int(Bigint.of_string "0x1",9)] in
  let query =
    ModelFinder.construct_model_query `PassiveAggressive fvs inpkt hello_phys outpkt
  in
  let res = Prover.is_sat Parameters.default query in
  Alcotest.(check bool) "is true" true res

let construct_model_query_PA_is_sat_hello_smaller _ =
  let set_port e = "standard_metadata.egress_spec" %<-% e in
  let drop = set_port @@ mkVInt(0,9) in
  let ipv4_tbl =
    let add = Hole("?AddRowtoipv4_fwd",1) %=% mkVInt(1,1)in
    let key = Hole("?hdr.ipv4.dstAddr_ipv4_fwd",32)
              %=% Var("hdr.ipv4.dstAddr",32) in
    let act i = Hole("?ActInipv4_fwd",2) %=% mkVInt(i,2) in
    let actrun i = "ipv4_fwd_action_run" %<-% mkVInt(i,3) in
    mkOrdered [
        bigand [add;key;act 0],
        sequence [actrun 1; set_port (Hole("?port_0_ipv4_fwd",9))];

        bigand [add;key;act 1], sequence [actrun 2; drop];

        True, drop;
      ]
  in
  let ipv4_rewrite_table =
    let add = Hole("?AddRowtoipv4_rewrite",1) %=% mkVInt(1,1)in
    let key = Hole("?hdr.ipv4.dstAddr_ipv4_rewrite",32)
              %=% Var("hdr.ipv4.dstAddr",32) in
    let act i = Hole("?ActInipv4_rewrite",2) %=% mkVInt(i,2) in
    let actrun i = "ipv4_rewrite_action_run" %<-% mkVInt(i,3) in
    mkOrdered [
        bigand [add;key;act 0],
        sequence [actrun 1;
                  "hdr.ethernet.srcAddr"
                  %<-% Var("hdr.ethernet.dstAddr",48);
                  "hdr.ethernet.dstAddr"
                  %<-% Hole("?dstAddr_0_ipv4_rewrite",48)];

        bigand [add;key;act 1], actrun 2;

        True, drop;
      ]
  in
  let hello_phys =
    sequence [
        "ipv4_fwd_action_run" %<-% mkVInt(0,3);
        "ipv4_fwd_rewrite_action_run" %<-% mkVInt(0,3);
        "standard_metadata.egress_port" %<-% mkVInt(0,9);
        ipv4_tbl;
        ipv4_rewrite_table;
        mkOrdered [
            Var("standard_metadata.egress_spec",9) %=% mkVInt(0,9),
            sequence [
                "hdr.ipv4.valid" %<-% mkVInt(0,1);
                "hdr.ethernet.srcAddr" %<-% mkVInt(0,48);
                "hdr.ethernet.dstAddr" %<-% mkVInt(0,48);
                "hdr.ipv4.ttl" %<-% mkVInt(0,8);
                "hdr.ipv4.dstAddr" %<-% mkVInt(0,32);
                "hdr.ipv4.srcAddr" %<-% mkVInt(0,32);
                "standard_metadata.egress_spec" %<-% mkVInt(0,9)
              ];
            True,Skip;
          ]
      ]
  in
  let fvs = [
      "standard_metadata.egress_spec",9;
      "hdr.ipv4_valid",1;
      "hdr.ethernet.srcAddr",48;
      "hdr.ethernet.dstAddr",48;
      "hdr.ipv4.ttl", 8;
      "hdr.ipv4.dstAddr",32;
      "hdr.ipv4.srcAddr",32;
      "standard_metadata.egress_spec",9;
    ]
  in
  let inpkt = Packet.mk_packet_from_list
                [ "hdr.ethernet.dstAddr", Int(Bigint.of_string "0x287bb416626", 48);
                  "hdr.ethernet.srcAddr", Int(Bigint.of_string "0xffffffffffff", 48);
                  "hdr.ipv4.dstAddr", Int(Bigint.of_string "0x0", 32);
                  "hdr.ipv4.srcAddr", Int(Bigint.of_string "0xabd62d8b", 32);
                  "hdr.ipv4.ttl", Int(Bigint.of_string "0x65", 8);
                  "hdr.ipv4_valid", Int(Bigint.of_string "0x1", 1);
                  "standard_metadata.egress_spec", Int(Bigint.of_string "0x1ff",9)] in
  let outpkt =
    Packet.mk_packet_from_list
      ["hdr.ethernet.dstAddr", Int(Bigint.of_string "0x0",48);
       "hdr.ethernet.srcAddr", Int(Bigint.of_string "0x287bb416626",48);
       "hdr.ipv4.dstAddr", Int(Bigint.of_string "0x0",32);
       "hdr.ipv4.srcAddr", Int(Bigint.of_string "0xabd62d8b",32);
       "hdr.ipv4.ttl", Int(Bigint.of_string "0x65",8);
       "hdr.ipv4_valid", Int(Bigint.of_string "0x1",1);
       "ipv4_lpm_action_run", Int(Bigint.of_string "0x1",1);
       "return1", Int(Bigint.of_string "0x0",1);
       "return2", Int(Bigint.of_string "0x0",1);
       "standard_metadata.egress_port", Int(Bigint.of_string "0x1",9);
       "standard_metadata.egress_spec", Int(Bigint.of_string "0x1",9)] in
  let query form =
    ModelFinder.construct_model_query form fvs inpkt hello_phys outpkt
  in
  let exp = Prover.is_sat Parameters.default (query `Passive) in
  let got = Prover.is_sat Parameters.default (query `PassiveAggressive) in
  if exp
  then Alcotest.(check bool) "PassiveAggressive is true" true got
  else Alcotest.(check bool) "Passive Failed!!!" true exp



let hints_injects_keys _ =
  let log = mkApply ("logical", ["x",32; "y", 32; "q", 32], [[], Skip],Skip) in
  let phys =
    sequence [
        mkApply ("p1", ["x",32;"y",32], [[],Skip], Skip);
        mkOrdered [
            Var("x",32) %=% mkVInt(100,32), mkApply ("p2a", ["y", 32; "q",32], [[], Skip], Skip);
            True, mkApply("p2b", ["x",32; "q",32], [[], Skip], Skip);
          ]
      ]
  in
  let edit = Tables.Edit.Add ("logical", (Match.([exact_ "x" (mkInt (5,32));
                                                  mask_ "y" (mkInt(0,32)) (mkInt(0,32));
                                                  exact_ "q" (mkInt(55,32))])
                                         , [], 0)) in
  let model = Hint.(construct log phys edit |> list_to_model phys) in
  let expected = Util.StringMap.of_alist_exn
                   [ "?x_p2b", mkInt(5,32);
                     "?q_p2b", mkInt(55,32) ]
  in
  same_model expected model

  
let () =
  let open Alcotest in
  run "Tests" [

      "smart constructors",
      [test_case "equality (Var, Value)" `Quick eq_test];

      "weakest precondition",
      [test_case "wp(skip,phi) = phi" `Quick wp_skip_eq;
       test_case "int assignment" `Quick wp_int_assign_eq;
       test_case "var assignment" `Quick wp_var_assign_eq;
       test_case "ordered" `Quick wp_ordered_eq;
       test_case "sequence" `Quick wp_seq_eq;
       test_case "assume" `Quick wp_assume_eq;
      ];

      "semantics",
      List.map semantics ~f:(fun (run_pkt, ex_pkt) ->
          test_case "cross product" `Quick @@
            fun _ -> same_packet ex_pkt run_pkt);

      "constant propagation",
      [test_case "straight line code" `Quick cp_wikipedia_ex1;
       test_case "eliminate if" `Quick cp_wikipedia_ex2;
       test_case "only holes, constants, and output vars remain in tests" `Quick cp_only_holes_and_constants;
       test_case "cp_also_props_copies" `Quick cp_also_props_copies;
       test_case "opt_bcm_example" `Quick opt_bcm_example;
       test_case "cp_affects_actions_and_keys" `Quick cp_affects_actions_and_keys;
      ];

      "dead code elimination",
      [test_case "eliminates irrelevant tables" `Quick dc_remove_table];

      "passive propogation",
      [test_case "handwritten example" `Quick rev_propogate_computes_single_path;
       test_case "learns from tests 1" `Quick passive_propogation_learns_from_tests1;
       test_case "learns from tests 2" `Quick passive_propogation_learns_from_tests2;
       test_case "learns facts common to disjoint paths" `Quick
         passive_propogation_learns_disjoint];

      "meet",
      [test_case "combines facts" `Quick meet_combines_facts];

      "quantifier elimination",
      [test_case "handwritten example" `Quick construct_model_query_PA_is_sat1;
       test_case "hello_world example" `Quick construct_model_query_PA_is_sat_hello_smaller];

      "Hints",
      [test_case "injects logical keys into physical table" `Quick hints_injects_keys]


    ]
