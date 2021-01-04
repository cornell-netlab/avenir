open Core
open Equality
open Avenir
open Ast

let packet_diff _ =
  let inpkt = Packet.mk_packet_from_list ["x", mkInt(11,32); "y", mkInt(100,32); "q", mkInt(55,32)] in
  let oupkt = Packet.mk_packet_from_list ["x", mkInt(11,32); "y", mkInt(5,32);   "z", mkInt(4,32)] in
  let open String in
  List.sort ~compare @@ Packet.diff_vars inpkt oupkt
  |> same_stringlist @@ List.sort ~compare ["y";"q";"z"]

let diff_test : unit Alcotest.test_case list =
  [Alcotest.test_case "diff computes differing vars" `Quick packet_diff]
