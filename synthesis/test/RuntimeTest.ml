open Core
open Avenir
open Ast

(* Parses real rules *)
let bmv2_parser_parses_real_rules _ =

  let entries =
    ["table_add send_frame rewrite_mac 1 => 00:aa:bb:00:00:00";
     "table_add send_frame rewrite_mac 2 => 00:aa:bb:00:00:01";
     "table_add forward set_dmac 10.0.0.10 => 00:04:00:00:00:00";
     "table_add forward set_dmac 10.0.1.10 => 00:04:00:00:00:01";
     "table_add ipv4_lpm set_nhop 10.0.0.10/32 => 10.0.0.10 1";
     "table_add ipv4_lpm set_nhop 10.0.1.10/32 => 10.0.1.10 2" ] in
  let drop = "standard_metadata.egress_spec" %<-% mkVInt(0,9) in
  let phys =
    sequence [
        mkOrdered[
            bigand [
                Var("hdr.ipv4.isValid",1) %=% mkVInt(1,1);
                Var("hdr.ipv4.ttl",8) %>% mkVInt(0,8)]
          , sequence [
                mkApply("ipv4_lpm", ["hdr.ipv4.dstAddr",32],
                        [("set_nhop", ["nhop_ipv4",32; "port", 9],
                         sequence [
                             "meta.routing_metadata.nhop_ipv4" %<-% Var("nhop_ipv4",32);
                             "standard_metadata.egress_spec" %<-% Var("port",9);
                             "hdr.ipv4.ttl" %<-% mkMinus (Var("hdr.ipv4.ttl", 8)) (mkVInt(1,8));
                           ]);
                         ("drop", [], drop)
                        ], drop);
                mkApply("forward",["meta.routing_metadata.nhop_ipv4",32],
                        ["set_dmac", ["dmac",48], "hdr.ethernet.dstAddr" %<-% Var("dmac",48);
                         "drop", [], drop;
                        ], drop
                  );
              ] ;
            True, Skip
          ];
        mkApply("send_frame", ["standard_metadata.egress_port",9],
                ["rewrite_mac", ["smac",48], "hdr.ipv4.srcAddr" %<-% Var("smac",48);
                 "drop", [], drop
                ], drop
          );
      ]
  in
  let expected =
    let open Edit in
    [
      Add("send_frame", ([Match.exact_ "standard_metadata.egress_port" (Value.make(1,9))],
                         [Value.str_make("0x00aabb000000", 48)],
                         0));
      Add("send_frame", ([Match.exact_ "standard_metadata.egress_port" (Value.make(2,9))],
                         [Value.str_make("0x00aabb000001", 48)],
                         0));
      Add("forward", ([Match.exact_ "meta.routing_metadata.nhop_ipv4" (Value.str_make("0x0a00000a",32))],
                         [Value.str_make("0x000400000000", 48)],
                         0));
      Add("forward", ([Match.exact_ "meta.routing_metadata.nhop_ipv4" (Value.str_make("0x0a00010a",32))],
                         [Value.str_make("0x000400000001", 48)],
                         0));
      Add("ipv4_lpm", ([Match.exact_ "hdr.ipv4.dstAddr" (Value.str_make("0x0a00000a",32))],
                       [Value.str_make("0x0a00000a",32);Value.make(1,9)],
                       0));
      Add("ipv4_lpm", ([Match.exact_ "hdr.ipv4.dstAddr" (Value.str_make("0x0a00010a",32))],
                       [Value.str_make("0x0a00010a",32);Value.make(2,9)],
                       0));
    ]
  in
  List.map entries ~f:(Runtime.parse_bmv2_entry phys)
  |> List.map ~f:(Edit.to_bmv2_string phys)
  |> Equality.same_stringlist (List.map ~f:(Edit.to_bmv2_string phys) expected)

let test_bmv2 : unit Alcotest.test_case list =
  [Alcotest.test_case "parses_real_rules" `Quick bmv2_parser_parses_real_rules]
