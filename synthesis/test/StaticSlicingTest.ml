open Core
open Equality
open Avenir
open Ast


let slicing_retargeting_metadata_ethernet _ =
  let open Util in
  let params = Parameters.({default with above = false}) in
  let inst = StringMap.of_alist_exn [
                 "ethernet", [(
                   [Match.exact_ "hdr.ethernet.dstAddr" (Value.make(99,48))],
                   [Value.make(1,32)],
                   0
                 )];
                 "ipv4", [(
                   [Match.exact_ "hdr.ipv4.dstAddr" (Value.make(44,32))],
                   [Value.make(2,32)],
                   0
                 )];
                 "nexthop", [(
                   [Match.exact_ "meta.nhop" (Value.make(1,32))],
                   [Value.make(99,9)],
                   0
                 );(
                   [Match.exact_ "meta.nhop" (Value.make(2,32))],
                   [Value.make(44,9)],
                   0
                 )]
               ] in
  let edits =
    let open Edit in
    [Add("ethernet", (
           [Match.exact_ "hdr.ethernet.dstAddr" (Value.make(11,48))],
           [Value.make(3,32)],
           0));
     Add("nexthop", (
           [Match.exact_ "meta.nhop" (Value.make(3,32))],
           [Value.make(11,9)],
           0))]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (mkVInt(0,9)) in
    let _drop = "drop", [], drop in
    let set_metadata = "nhop", ["nhop",32], "meta.nhop" %<-% Var("nhop",32) in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      ["set_port",["port", 9], Var("port",9) |> set_port ; _drop]
    in
    sequence [
        mkApply("ethernet",["hdr.ethernet.dstAddr",48],classify_actions,drop);
        mkApply("ipv4",["hdr.ipv4.dstAddr",32],classify_actions,drop);
        mkApply("nexthop",["meta.nhop",32],fwd_actions,drop);
      ]
  in
  let expected =
    sequence [
        mkAssume (Var("hdr.ethernet.dstAddr",48) %=% mkVInt(11,48));
        "meta.nhop" %<-% mkVInt(3,32);
        mkAssume (Var("meta.nhop",32) %=% mkVInt(3,32));
        set_port (mkVInt(11,9));
      ]
  in
  StaticSlicing.edit_slice params inst edits cmd
  |> same_cmd expected


let slicing_retargeting_metadata_ipv4 _ =
  let open Util in
  let params = Parameters.({default with above = false}) in
  let inst = StringMap.of_alist_exn [
                 "ethernet", [(
                   [Match.exact_ "hdr.ethernet.dstAddr" (Value.make(99,48))],
                   [Value.make(1,32)],
                   0
                 )];
                 "ipv4", [(
                   [Match.exact_ "hdr.ipv4.dstAddr" (Value.make(44,32))],
                   [Value.make(2,32)],
                   0
                 )];
                 "nexthop", [(
                   [Match.exact_ "meta.nhop" (Value.make(1,32))],
                   [Value.make(99,9)],
                   0
                 );(
                   [Match.exact_ "meta.nhop" (Value.make(2,32))],
                   [Value.make(44,9)],
                   0
                 )]
               ] in
  let edits =
    let open Edit in
    [Add("ipv4", (
           [Match.exact_ "hdr.ipv4.dstAddr" (Value.make(11,32))],
           [Value.make(3,32)],
           0));
     Add("nexthop", (
           [Match.exact_ "meta.nhop" (Value.make(3,32))],
           [Value.make(11,9)],
           0))]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (mkVInt(0,9)) in
    let _drop = "drop", [], drop in
    let set_metadata = "nhop", ["nhop",32], "meta.nhop" %<-% Var("nhop",32) in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      ["set_port",["port", 9], Var("port",9) |> set_port ; _drop]
    in
    sequence [
        mkApply("ethernet",["hdr.ethernet.dstAddr",48],classify_actions,drop);
        mkApply("ipv4",["hdr.ipv4.dstAddr",32],classify_actions,drop);
        mkApply("nexthop",["meta.nhop",32],fwd_actions,drop);
      ]
  in
  let expected =
    sequence [
        mkAssume (Var("hdr.ipv4.dstAddr",32) %=% mkVInt(11,32));
        "meta.nhop" %<-% mkVInt(3,32);
        mkAssume (Var("meta.nhop",32) %=% mkVInt(3,32));
        set_port (mkVInt(11,9));
      ]
  in
  StaticSlicing.edit_slice params inst edits cmd
  |> same_cmd expected

let slicing_fabric_example _ =
  let open Util in
  let params = Parameters.({default with above = false}) in
  let inst = StringMap.of_alist_exn [
                 "nexthop", [(
                   [Match.exact_ "meta.nhop" (Value.make(1,32))],
                   [Value.make(11,9)],
                   0
                 )]
               ] in
  let edits =
    let open Edit in
    [Add("ethernet", (
           [Match.exact_ "hdr.ethernet.dstAddr" (Value.make(11,48))],
           [Value.make(1,32)],
           0))]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (mkVInt(0,9)) in
    let _drop = "drop", [], drop in
    let set_metadata = "nhop", ["nhop",32], "meta.nhop" %<-% Var("nhop",32) in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      ["set_port",["port", 9], Var("port",9) |> set_port ; _drop]
    in
    sequence [
        mkApply("ethernet",["hdr.ethernet.dstAddr",48],classify_actions,drop);
        mkApply("nexthop",["meta.nhop",32],fwd_actions,drop);
      ]
  in
  let expected =
    sequence [
        mkAssume (Var("hdr.ethernet.dstAddr",48) %=% mkVInt(11,48));
        "meta.nhop" %<-% mkVInt(1,32);
        mkAssume (Var("meta.nhop",32) %=% mkVInt(1,32));
        set_port (mkVInt(11,9));
      ]
  in
  StaticSlicing.edit_slice params inst edits cmd
  |> same_cmd expected


let test : unit Alcotest.test_case list =
  [Alcotest.test_case "slices away unnecessary extant rows" `Quick slicing_retargeting_metadata_ethernet;
   Alcotest.test_case "keeps necessary extant rows" `Quick slicing_fabric_example;
   Alcotest.test_case "does the right thing?" `Quick slicing_retargeting_metadata_ipv4;
  ]
