open Core
open Avenir
open Avenir.Test

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
  let query =
    ModelFinder.construct_model_query ModelFinder.no_opts fvs
      [(inpkt, outpkt)] inpkt hello_phys outpkt
  in
  let exp = Prover.is_sat Parameters.default query in
  Alcotest.(check bool) "Passive is sat" true exp

let test : unit Alcotest.test_case list =
  [ Alcotest.test_case "hello_world example" `Quick
      construct_model_query_PA_is_sat_hello_smaller ]
