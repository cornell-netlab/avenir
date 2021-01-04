open Alcotest
open Core
open Avenir
open Ast
open CompilerOpts


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
  same_cmd exp (optimize fvs cmd)


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
  @@ passive_optimize out_packet cmd

let test : unit test_case list =
  [test_case "active handwritten example" `Quick opts_minimize;
   test_case "passive handwritten example" `Quick rev_propogate_computes_single_path
  ]
