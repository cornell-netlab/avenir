open Core
open Avenir
open Avenir.Test

let construct_model_query_PA_is_sat1 _ =
  let open Cmd in
  Prover.make_provers "z3" ;
  let inpkt =
    Packet.mk_packet_from_list
      [("ipv4.dst", Value.make (5, 32)); ("port", Value.make (8, 9))]
  in
  let outpkt =
    Packet.mk_packet_from_list
      [("ipv4.dst", Value.make (5, 32)); ("port", Value.make (99, 9))]
  in
  let fvs = [("port", 9); ("ipv4.dst", 32)] in
  let phys =
    sequence
      [ "port" %<-% Expr.value (0, 9)
      ; "ipv4_action_run" %<-% Expr.value (0, 1)
      ; "exit" %<-% Expr.value (0, 1)
      ; ordered
          [ ( Var ("exit", 1) %<>% Expr.value (1, 1)
            , ordered
                [ ( bigand
                      [ Hole ("?AddRowtoipv4", 1) %=% Expr.value (1, 1)
                      ; Hole ("?ipv4.dst_ipv4", 32) %=% Var ("ipv4.dst", 32)
                      ; Hole ("?ActInipv4", 1) %=% Expr.value (0, 1) ]
                  , sequence
                      [ "port" %<-% Hole ("?port_tbl_0", 9)
                      ; "ipv4_action_run" %<-% Expr.value (1, 1) ] )
                ; (True, Skip) ] )
          ; (True, Skip) ]
      ; ordered
          [ ( Var ("port", 9) %=% Expr.value (0, 9)
            , sequence
                [ "ipv4.dst" %<-% Expr.value (0, 0)
                ; "port" %<-% Expr.value (0, 0) ] )
          ; (True, Skip) ] ]
  in
  let query =
    ModelFinder.construct_model_query ModelFinder.no_opts `PassiveAggressive
      fvs [(inpkt, outpkt)] inpkt phys outpkt
  in
  let res = Prover.is_sat Parameters.default query in
  Alcotest.(check bool) "is true" true res

let construct_model_query_PA_is_sat_hello _ =
  let open Cmd in
  Prover.make_provers "z3" ;
  let set_port e = "standard_metadata.egress_spec" %<-% e in
  let drop = set_port @@ Expr.value (0, 9) in
  let ipv4_tbl =
    let add = Hole ("?AddRowtoipv4_fwd", 1) %=% Expr.value (1, 1) in
    let key =
      Hole ("?hdr.ipv4.dstAddr_ipv4_fwd", 32) %=% Var ("hdr.ipv4.dstAddr", 32)
    in
    let act i = Hole ("?ActInipv4_fwd", 2) %=% Expr.value (i, 2) in
    let actrun i = "ipv4_fwd_action_run" %<-% Expr.value (i, 3) in
    ordered
      [ ( bigand [add; key; act 0]
        , sequence [actrun 1; set_port (Hole ("?port_0_ipv4_fwd", 9))] )
      ; (bigand [add; key; act 1], sequence [actrun 2; drop])
      ; (bigand [add; key; act 2], sequence [actrun 3])
      ; (True, drop) ]
  in
  let ipv4_rewrite_table =
    let add = Hole ("?AddRowtoipv4_rewrite", 1) %=% Expr.value (1, 1) in
    let key =
      Hole ("?hdr.ipv4.dstAddr_ipv4_rewrite", 32)
      %=% Var ("hdr.ipv4.dstAddr", 32)
    in
    let act i = Hole ("?ActInipv4_rewrite", 2) %=% Expr.value (i, 2) in
    let actrun i = "ipv4_rewrite_action_run" %<-% Expr.value (i, 3) in
    ordered
      [ ( bigand [add; key; act 0]
        , sequence
            [ actrun 1
            ; "hdr.ethernet.srcAddr" %<-% Var ("hdr.ethernet.dstAddr", 48)
            ; "hdr.ethernet.dstAddr" %<-% Hole ("?dstAddr_0_ipv4_rewrite", 48)
            ; ( "hdr.ipv4.ttl"
              %<-% Expr.(minus (Var ("hdr.ipv4.ttl", 8)) (Expr.value (1, 8)))
              ) ] )
      ; (bigand [add; key; act 1], sequence [actrun 2; drop])
      ; (bigand [add; key; act 2], sequence [actrun 3])
      ; (True, drop) ]
  in
  let hello_phys =
    sequence
      [ "exit" %<-% Expr.value (0, 1)
      ; "ipv4_fwd_action_run" %<-% Expr.value (0, 3)
      ; "ipv4_fwd_rewrite_action_run" %<-% Expr.value (0, 3)
      ; "return3" %<-% Expr.value (0, 1)
      ; "return4" %<-% Expr.value (0, 1)
      ; "standard_metadata.egress_port" %<-% Expr.value (0, 9)
      ; "return4" %<-% Expr.value (0, 1)
      ; ordered
          [ ( Var ("hdr.ipv4_valid", 1) %=% Expr.value (1, 1)
            , sequence
                [ "ipv4_fwd_action_run" %<-% Expr.value (0, 3)
                ; ipv4_tbl
                ; ordered
                    [ ( Var ("exit", 1) %=% Expr.value (0, 1)
                      , ordered
                          [ ( Var ("return4", 1) %=% Expr.value (0, 1)
                            , sequence
                                [ "ipv4_rewrite_action_run"
                                  %<-% Expr.value (0, 3)
                                ; ipv4_rewrite_table ] )
                          ; (True, Skip) ] )
                    ; (True, Skip) ] ] )
          ; (True, Skip) ]
      ; "standard_metadata.egress_port"
        %<-% Var ("standard_metadata.egress_spec", 9)
      ; ordered
          [ ( Var ("standard_metadata.egress_spec", 9) %<>% Expr.value (0, 9)
            , "return3" %<-% Expr.value (0, 1) )
          ; (True, Skip) ]
      ; ordered
          [ ( Var ("standard_metadata.egress_spec", 9) %=% Expr.value (0, 9)
            , sequence
                [ "hdr.ipv4.valid" %<-% Expr.value (0, 1)
                ; "hdr.ethernet.srcAddr" %<-% Expr.value (0, 48)
                ; "hdr.ethernet.dstAddr" %<-% Expr.value (0, 48)
                ; "hdr.ipv4.ttl" %<-% Expr.value (0, 8)
                ; "hdr.ipv4.dstAddr" %<-% Expr.value (0, 32)
                ; "hdr.ipv4.srcAddr" %<-% Expr.value (0, 32)
                ; "standard_metadata.egress_spec" %<-% Expr.value (0, 9) ] )
          ; (True, Skip) ] ]
  in
  let fvs =
    [ ("standard_metadata.egress_spec", 9)
    ; ("hdr.ipv4_valid", 1)
    ; ("hdr.ethernet.srcAddr", 48)
    ; ("hdr.ethernet.dstAddr", 48)
    ; ("hdr.ipv4.ttl", 8)
    ; ("hdr.ipv4.dstAddr", 32)
    ; ("hdr.ipv4.srcAddr", 32)
    ; ("standard_metadata.egress_spec", 9) ]
  in
  let inpkt =
    Packet.mk_packet_from_list
      [ ("hdr.ethernet.dstAddr", Value.str_make ("0x287bb416626", 48))
      ; ("hdr.ethernet.srcAddr", Value.str_make ("0xffffffffffff", 48))
      ; ("hdr.ipv4.dstAddr", Value.str_make ("0x0", 32))
      ; ("hdr.ipv4.srcAddr", Value.str_make ("0xabd62d8b", 32))
      ; ("hdr.ipv4.ttl", Value.str_make ("0x65", 8))
      ; ("hdr.ipv4_valid", Value.str_make ("0x1", 1))
      ; ("standard_metadata.egress_spec", Value.str_make ("0x1ff", 9)) ]
  in
  let outpkt =
    Packet.mk_packet_from_list
      [ ("hdr.ethernet.dstAddr", Value.str_make ("0x0", 48))
      ; ("hdr.ethernet.srcAddr", Value.str_make ("0x287bb416626", 48))
      ; ("hdr.ipv4.dstAddr", Value.str_make ("0x0", 32))
      ; ("hdr.ipv4.srcAddr", Value.str_make ("0xabd62d8b", 32))
      ; ("hdr.ipv4.ttl", Value.str_make ("0x64", 8))
      ; ("hdr.ipv4_valid", Value.str_make ("0x1", 1))
      ; ("ipv4_lpm_action_run", Value.str_make ("0x1", 1))
      ; ("return1", Value.str_make ("0x0", 1))
      ; ("return2", Value.str_make ("0x0", 1))
      ; ("standard_metadata.egress_port", Value.str_make ("0x1", 9))
      ; ("standard_metadata.egress_spec", Value.str_make ("0x1", 9)) ]
  in
  let query =
    ModelFinder.construct_model_query ModelFinder.no_opts `PassiveAggressive
      fvs [(inpkt, outpkt)] inpkt hello_phys outpkt
  in
  let res = Prover.is_sat Parameters.default query in
  Alcotest.(check bool) "is true" true res

let construct_model_query_PA_is_sat_hello_smaller _ =
  let open Cmd in
  let set_port e = "standard_metadata.egress_spec" %<-% e in
  let drop = set_port @@ Expr.value (0, 9) in
  let ipv4_tbl =
    let add = Hole ("?AddRowtoipv4_fwd", 1) %=% Expr.value (1, 1) in
    let key =
      Hole ("?hdr.ipv4.dstAddr_ipv4_fwd", 32) %=% Var ("hdr.ipv4.dstAddr", 32)
    in
    let act i = Hole ("?ActInipv4_fwd", 2) %=% Expr.value (i, 2) in
    let actrun i = "ipv4_fwd_action_run" %<-% Expr.value (i, 3) in
    ordered
      [ ( bigand [add; key; act 0]
        , sequence [actrun 1; set_port (Hole ("?port_0_ipv4_fwd", 9))] )
      ; (bigand [add; key; act 1], sequence [actrun 2; drop])
      ; (True, drop) ]
  in
  let ipv4_rewrite_table =
    let add = Hole ("?AddRowtoipv4_rewrite", 1) %=% Expr.value (1, 1) in
    let key =
      Hole ("?hdr.ipv4.dstAddr_ipv4_rewrite", 32)
      %=% Var ("hdr.ipv4.dstAddr", 32)
    in
    let act i = Hole ("?ActInipv4_rewrite", 2) %=% Expr.value (i, 2) in
    let actrun i = "ipv4_rewrite_action_run" %<-% Expr.value (i, 3) in
    ordered
      [ ( bigand [add; key; act 0]
        , sequence
            [ actrun 1
            ; "hdr.ethernet.srcAddr" %<-% Var ("hdr.ethernet.dstAddr", 48)
            ; "hdr.ethernet.dstAddr" %<-% Hole ("?dstAddr_0_ipv4_rewrite", 48)
            ] )
      ; (bigand [add; key; act 1], actrun 2)
      ; (True, drop) ]
  in
  let hello_phys =
    sequence
      [ "ipv4_fwd_action_run" %<-% Expr.value (0, 3)
      ; "ipv4_fwd_rewrite_action_run" %<-% Expr.value (0, 3)
      ; "standard_metadata.egress_port" %<-% Expr.value (0, 9)
      ; ipv4_tbl
      ; ipv4_rewrite_table
      ; ordered
          [ ( Var ("standard_metadata.egress_spec", 9) %=% Expr.value (0, 9)
            , sequence
                [ "hdr.ipv4.valid" %<-% Expr.value (0, 1)
                ; "hdr.ethernet.srcAddr" %<-% Expr.value (0, 48)
                ; "hdr.ethernet.dstAddr" %<-% Expr.value (0, 48)
                ; "hdr.ipv4.ttl" %<-% Expr.value (0, 8)
                ; "hdr.ipv4.dstAddr" %<-% Expr.value (0, 32)
                ; "hdr.ipv4.srcAddr" %<-% Expr.value (0, 32)
                ; "standard_metadata.egress_spec" %<-% Expr.value (0, 9) ] )
          ; (True, Skip) ] ]
  in
  let fvs =
    [ ("standard_metadata.egress_spec", 9)
    ; ("hdr.ipv4_valid", 1)
    ; ("hdr.ethernet.srcAddr", 48)
    ; ("hdr.ethernet.dstAddr", 48)
    ; ("hdr.ipv4.ttl", 8)
    ; ("hdr.ipv4.dstAddr", 32)
    ; ("hdr.ipv4.srcAddr", 32)
    ; ("standard_metadata.egress_spec", 9) ]
  in
  let inpkt =
    Packet.mk_packet_from_list
      [ ("hdr.ethernet.dstAddr", Value.str_make ("0x287bb416626", 48))
      ; ("hdr.ethernet.srcAddr", Value.str_make ("0xffffffffffff", 48))
      ; ("hdr.ipv4.dstAddr", Value.str_make ("0x0", 32))
      ; ("hdr.ipv4.srcAddr", Value.str_make ("0xabd62d8b", 32))
      ; ("hdr.ipv4.ttl", Value.str_make ("0x65", 8))
      ; ("hdr.ipv4_valid", Value.str_make ("0x1", 1))
      ; ("standard_metadata.egress_spec", Value.str_make ("0x1ff", 9)) ]
  in
  let outpkt =
    Packet.mk_packet_from_list
      [ ("hdr.ethernet.dstAddr", Value.str_make ("0x0", 48))
      ; ("hdr.ethernet.srcAddr", Value.str_make ("0x287bb416626", 48))
      ; ("hdr.ipv4.dstAddr", Value.str_make ("0x0", 32))
      ; ("hdr.ipv4.srcAddr", Value.str_make ("0xabd62d8b", 32))
      ; ("hdr.ipv4.ttl", Value.str_make ("0x65", 8))
      ; ("hdr.ipv4_valid", Value.str_make ("0x1", 1))
      ; ("ipv4_lpm_action_run", Value.str_make ("0x1", 1))
      ; ("return1", Value.str_make ("0x0", 1))
      ; ("return2", Value.str_make ("0x0", 1))
      ; ("standard_metadata.egress_port", Value.str_make ("0x1", 9))
      ; ("standard_metadata.egress_spec", Value.str_make ("0x1", 9)) ]
  in
  let query form =
    ModelFinder.construct_model_query ModelFinder.no_opts form fvs
      [(inpkt, outpkt)] inpkt hello_phys outpkt
  in
  let exp = Prover.is_sat Parameters.default (query `Passive) in
  let got = Prover.is_sat Parameters.default (query `PassiveAggressive) in
  if exp then Alcotest.(check bool) "PassiveAggressive is true" true got
  else Alcotest.(check bool) "Passive Failed!!!" true exp

let qe_test : unit Alcotest.test_case list =
  [ Alcotest.test_case "handwritten example" `Quick
      construct_model_query_PA_is_sat1
  ; Alcotest.test_case "hello_world example" `Quick
      construct_model_query_PA_is_sat_hello_smaller ]
