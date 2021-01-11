open Core
open Avenir
open Avenir.Test
open Ast
open Equality

(*Edits*)
let extract_edits_from_model _ =
  let drop = "standard_metadata.egress_spec" %<-% Expr.value(0,9) in
  let phys =
    sequence [
        mkApply("ethernet",["hdr.ethernet.dstAddr", 48],["nexthop", ["nexthop",32],"meta.nexthop" %<-% Var("nexthop",32);"drop", [],drop],drop);
        mkOrdered [
            Var("hdr.ipv4.isValid",1) %=% Expr.value(1,1),
            sequence [
                mkApply("ipv4_fib", ["hdr.ipv4.dstAddr", 32], ["nexthop", ["nexthop",32],"meta.nexthop" %<-% Var("nexthop",32);"drop",[],drop], drop);
                mkApply("ipv4_rewrite",
                        ["hdr.ipv4.dstAddr", 32],
                        [ "rewrite",
                          ["dstAddr",48],
                         sequence [
                             "hdr.ethernet.srcAddr" %<-% Var("hdr.ethernet.dstAddr",48);
                             "hdr.ethernet.dstAddr" %<-% Var("dstAddr",32);
                             "hdr.ipv4.ttl" %<-%
                               SatMinus(Var("hdr.ipv4.ttl",16), Expr.value(1,16))
                           ]
                        ], Skip)
              ];
            True, Skip
          ];
        mkApply("nexthop",["meta.nexthop",32],["fwd", ["port",9], "standard_metadata.egress_spec" %<-% Var("port",9); "drop", [],drop],drop);
        mkApply("punt", ["hdr.ethernet.etherType", 16;
                         "hdr.ipv4.isValid",1;
                         "hdr.ipv4.version",4;
                         "hdr.ipv4.srcAddr",32;
                         "hdr.ipv4.dstAddr",32;
                         "hdr.ipv4.ttl", 8;
                        ],["drop", [],drop], Skip);
      ]
  in
  let model =
    Model.of_alist_exn [
      "?AddRowToethernet", Value.make(1,1);
      "?AddRowToipv4_fib", Value.make(0,1);
      "?AddRowToipv4_rewrite", Value.make(1,1);
      "?AddRowTonexthop", Value.make(1,1);
      "?AddRowTopunt", Value.make(0,1);
      "?ActInethernet", Value.make(0,1);
      "?ActInipv4_fib", Value.make(1,1);
      "?ActInipv4_rewrite", Value.make(0,1);
      "?ActInnexthop", Value.make(0,1);
      "?ActInpunt", Value.make(0,1);
      "?hdr.ethernet.dstAddr_ethernet", Value.make(0,48);
      "?hdr.ethernet.dstAddr_ethernet_mask", Value.make(0,48);
      "?hdr.ethernet.etherType_punt", Value.make(0,16);
      "?hdr.ethernet.etherType_punt_mask", Value.make(0,16);
      "?hdr.ipv4.dstAddr_ipv4_fib" , Value.big_make(Util.max_int 32, 32);
      "?hdr.ipv4.dstAddr_ipv4_fib_mask", Value.big_make(Util.max_int 32,32);
      "?hdr.ipv4.dstAddr_ipv4_rewrite", Value.big_make(Bigint.of_string "0xa008",32);
      "?hdr.ipv4.dstAddr_ipv4_rewrite_mask", Value.big_make(Util.max_int 32,32);
      "?hdr.ipv4.dstAddr_punt", Value.big_make(Bigint.of_string "0x200000", 32);
      "?hdr.ipv4.dstAddr_punt_mask", Value.big_make(Util.max_int 32,32);
      "?hdr.ipv4.isValid_punt", Value.make(0,1);
      "?hdr.ipv4.isValid_punt_mask", Value.make(0,1);
      "?hdr.ipv4.srcAddr_punt", Value.make(0,32);
      "?hdr.ipv4.srcAddr_punt_mask", Value.make(0,32);
      "?hdr.ipv4.ttl_punt", Value.make(0,8);
      "?hdr.ipv4.ttl_punt_mask", Value.make(0,8);
      "?hdr.ipv4.version_punt", Value.make(0,4);
      "?hdr.ipv4.version_punt_mask", Value.make(0,4);
      "?meta.nexthop_nexthop", Value.big_make(Bigint.of_string "0x226890e0",32);
      "?meta.nexthop_nexthRunop_mask", Value.big_make(Bigint.of_string "0xffffffff",32);
      "ethernet_0_nexthop_32", Value.big_make(Bigint.of_string "0x226890e0",32);
      "ipv4_fib_0_nexthop_32", Value.big_make(Bigint.of_string "0x6d1a0610",32);
      "ipv4_fib_action_run", Value.make(3,2);
      "ipv4_rewrite_0_dstAddr_48", Value.make(8,48);
      "nexthop_0_port_9", Value.make(8,9);
      ]
  in
  let expected_edits =
    let open Edit in
    [
      Add("nexthop",
          ([Match.exact_ "meta.nexthop" (Value.str_make("0x226890e0",32))],
           [Value.make(8,9)],0));
      Add("ipv4_rewrite",
          ([Match.exact_ "hdr.ipv4.dstAddr" (Value.str_make("0xa008",32))]
          ,[Value.make(8,48)],0));
      Add("ethernet",
          ([Match.wildcard "hdr.ethernet.dstAddr" 32;
            Match.wildcard "hdr.ethernet.etherType" 16;
           ],[Value.str_make("0x226890e0",32)],0) );

    ]
  in
  Edit.of_model phys model
  |> same_edits expected_edits


let test : unit Alcotest.test_case list =
  [Alcotest.test_case "extract from models" `Quick extract_edits_from_model]
