open Core
open Avenir
open Avenir.Test
open Semantics

let cross_packet =
  let open Cmd in
  let dst_is x = Var ("dst", 2) %=% Expr.value (x, 2) in
  let src_is x = Var ("src", 2) %=% Expr.value (x, 2) in
  let prog =
    sequence
      [ ordered
          [ (src_is 1, "smac" %<-% Expr.value (2, 2))
          ; (src_is 0, "smac" %<-% Expr.value (1, 2))
          ; (True, Skip) ]
      ; ordered
          [ (dst_is 1, "out" %<-% Expr.value (2, 2))
          ; (dst_is 0, "out" %<-% Expr.value (1, 2))
          ; (True, Skip) ] ]
  in
  let update f j pkt = Packet.set_field pkt f (Value.make (j, 2)) in
  let mkPacket i j = Packet.empty |> update "src" i |> update "dst" j in
  let update f j pkt = Packet.set_field pkt f (Value.make (j, 2)) in
  let pkt00 = mkPacket 0 0 in
  let pkt01 = mkPacket 0 1 in
  let pkt10 = mkPacket 1 0 in
  let pkt11 = mkPacket 1 1 in
  let run = eval_act prog in
  [ (run pkt00, pkt00 |> update "smac" 1 |> update "out" 1)
  ; (run pkt01, pkt01 |> update "smac" 1 |> update "out" 2)
  ; (run pkt10, pkt10 |> update "smac" 2 |> update "out" 1)
  ; (run pkt11, pkt11 |> update "smac" 2 |> update "out" 2) ]

let test : unit Alcotest.test_case list =
  List.map cross_packet ~f:(fun (run_pkt, ex_pkt) ->
      Alcotest.test_case "cross product" `Quick
      @@ fun _ -> Equality.same_packet ex_pkt run_pkt)
