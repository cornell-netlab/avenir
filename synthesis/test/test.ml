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
  same_cmd exp (ConstantProp.propogate cmd)

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
  same_cmd exp (ConstantProp.propogate cmd)


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
  same_cmd exp (ConstantProp.propogate cmd)


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
  same_cmd exp (ConstantProp.propogate cmd)


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
      ]
    ]
