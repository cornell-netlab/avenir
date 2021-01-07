open Core
open Avenir
open Ast
open Equality

(*Edits*)
let extract_edits_from_model _ =
  let drop = "standard_metadata.egress_spec" %<-% mkVInt(0,9) in
  let phys =
    sequence [
        mkApply("ethernet",["hdr.ethernet.dstAddr", 48],["nexthop", ["nexthop",32],"meta.nexthop" %<-% Var("nexthop",32);"drop", [],drop],drop);
        mkOrdered [
            Var("hdr.ipv4.isValid",1) %=% mkVInt(1,1),
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
                               SatMinus(Var("hdr.ipv4.ttl",16), mkVInt(1,16))
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
      "?AddRowToethernet", mkInt(1,1);
      "?AddRowToipv4_fib", mkInt(0,1);
      "?AddRowToipv4_rewrite", mkInt(1,1);
      "?AddRowTonexthop", mkInt(1,1);
      "?AddRowTopunt", mkInt(0,1);
      "?ActInethernet", mkInt(0,1);
      "?ActInipv4_fib", mkInt(1,1);
      "?ActInipv4_rewrite", mkInt(0,1);
      "?ActInnexthop", mkInt(0,1);
      "?ActInpunt", mkInt(0,1);
      "?hdr.ethernet.dstAddr_ethernet", mkInt(0,48);
      "?hdr.ethernet.dstAddr_ethernet_mask", mkInt(0,48);
      "?hdr.ethernet.etherType_punt", mkInt(0,16);
      "?hdr.ethernet.etherType_punt_mask", mkInt(0,16);
      "?hdr.ipv4.dstAddr_ipv4_fib" , Int(Util.max_int 32, 32);
      "?hdr.ipv4.dstAddr_ipv4_fib_mask", Int(Util.max_int 32,32);
      "?hdr.ipv4.dstAddr_ipv4_rewrite", Int(Bigint.of_string "0xa008",32);
      "?hdr.ipv4.dstAddr_ipv4_rewrite_mask", Int(Util.max_int 32,32);
      "?hdr.ipv4.dstAddr_punt", Int(Bigint.of_string "0x200000", 32);
      "?hdr.ipv4.dstAddr_punt_mask", Int(Util.max_int 32,32);
      "?hdr.ipv4.isValid_punt", mkInt(0,1);
      "?hdr.ipv4.isValid_punt_mask", mkInt(0,1);
      "?hdr.ipv4.srcAddr_punt", mkInt(0,32);
      "?hdr.ipv4.srcAddr_punt_mask", mkInt(0,32);
      "?hdr.ipv4.ttl_punt", mkInt(0,8);
      "?hdr.ipv4.ttl_punt_mask", mkInt(0,8);
      "?hdr.ipv4.version_punt", mkInt(0,4);
      "?hdr.ipv4.version_punt_mask", mkInt(0,4);
      "?meta.nexthop_nexthop", Int(Bigint.of_string "0x226890e0",32);
      "?meta.nexthop_nexthRunop_mask", Int(Bigint.of_string "0xffffffff",32);
      "ethernet_0_nexthop_32", Int(Bigint.of_string "0x226890e0",32);
      "ipv4_fib_0_nexthop_32", Int(Bigint.of_string "0x6d1a0610",32);
      "ipv4_fib_action_run", mkInt(3,2);
      "ipv4_rewrite_0_dstAddr_48", mkInt(8,48);
      "nexthop_0_port_9", mkInt(8,9);
      ]
  in
  let expected_edits =
    let open Edit in
    [
      Add("nexthop",
          ([Match.exact_ "meta.nexthop" (Int(Bigint.of_string "0x226890e0",32))],
           [mkInt(8,9)],0));
      Add("ipv4_rewrite",
          ([Match.exact_ "hdr.ipv4.dstAddr" (Int(Bigint.of_string "0xa008",32))]
          ,[mkInt(8,48)],0));
      Add("ethernet",
          ([Match.wildcard "hdr.ethernet.dstAddr" 32;
            Match.wildcard "hdr.ethernet.etherType" 16;
           ],[Int(Bigint.of_string "0x226890e0",32)],0) );

    ]
  in
  Edit.of_model phys model
  |> same_edits expected_edits


let test : unit Alcotest.test_case list =
  [Alcotest.test_case "extract from models" `Quick extract_edits_from_model]
