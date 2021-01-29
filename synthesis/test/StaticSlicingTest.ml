open Core
open Equality
open Avenir
open Avenir.Test
open Cmd

let slicing_retargeting_metadata_ethernet _ =
  let params = Parameters.{default with above= false} in
  let inst =
    let open Instance in
    empty
    |> set_rows ~table:"ethernet"
         ~rows:
           [ ( [Match.exact_ "hdr.ethernet.dstAddr" (Value.make (99, 48))]
             , [Value.make (1, 32)]
             , 0 ) ]
    |> set_rows ~table:"ipv4"
         ~rows:
           [ ( [Match.exact_ "hdr.ipv4.dstAddr" (Value.make (44, 32))]
             , [Value.make (2, 32)]
             , 0 ) ]
    |> set_rows ~table:"nexthop"
         ~rows:
           [ ( [Match.exact_ "meta.nhop" (Value.make (1, 32))]
             , [Value.make (99, 9)]
             , 0 )
           ; ( [Match.exact_ "meta.nhop" (Value.make (2, 32))]
             , [Value.make (44, 9)]
             , 0 ) ]
  in
  let edits =
    let open Edit in
    [ Add
        ( "ethernet"
        , ( [Match.exact_ "hdr.ethernet.dstAddr" (Value.make (11, 48))]
          , [Value.make (3, 32)]
          , 0 ) )
    ; Add
        ( "nexthop"
        , ( [Match.exact_ "meta.nhop" (Value.make (3, 32))]
          , [Value.make (11, 9)]
          , 0 ) ) ]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (Expr.value (0, 9)) in
    let _drop = ("drop", [], drop) in
    let set_metadata =
      ("nhop", [("nhop", 32)], "meta.nhop" %<-% Var ("nhop", 32))
    in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      [("set_port", [("port", 9)], Expr.Var ("port", 9) |> set_port); _drop]
    in
    sequence
      [ apply
          ("ethernet", [("hdr.ethernet.dstAddr", 48)], classify_actions, drop)
      ; apply ("ipv4", [("hdr.ipv4.dstAddr", 32)], classify_actions, drop)
      ; apply ("nexthop", [("meta.nhop", 32)], fwd_actions, drop) ]
  in
  let expected =
    sequence
      [ assume (Var ("hdr.ethernet.dstAddr", 48) %=% Expr.value (11, 48))
      ; "meta.nhop" %<-% Expr.value (3, 32)
      ; assume (Var ("meta.nhop", 32) %=% Expr.value (3, 32))
      ; set_port (Expr.value (11, 9)) ]
  in
  StaticSlicing.edit_slice params inst edits cmd |> same_cmd expected

let slicing_retargeting_metadata_ipv4 _ =
  let params = Parameters.{default with above= false} in
  let inst =
    let open Instance in
    empty
    |> set_rows ~table:"ethernet"
         ~rows:
           [ ( [Match.exact_ "hdr.ethernet.dstAddr" (Value.make (99, 48))]
             , [Value.make (1, 32)]
             , 0 ) ]
    |> set_rows ~table:"ipv4"
         ~rows:
           [ ( [Match.exact_ "hdr.ipv4.dstAddr" (Value.make (44, 32))]
             , [Value.make (2, 32)]
             , 0 ) ]
    |> set_rows ~table:"nexthop"
         ~rows:
           [ ( [Match.exact_ "meta.nhop" (Value.make (1, 32))]
             , [Value.make (99, 9)]
             , 0 )
           ; ( [Match.exact_ "meta.nhop" (Value.make (2, 32))]
             , [Value.make (44, 9)]
             , 0 ) ]
  in
  let edits =
    let open Edit in
    [ Add
        ( "ipv4"
        , ( [Match.exact_ "hdr.ipv4.dstAddr" (Value.make (11, 32))]
          , [Value.make (3, 32)]
          , 0 ) )
    ; Add
        ( "nexthop"
        , ( [Match.exact_ "meta.nhop" (Value.make (3, 32))]
          , [Value.make (11, 9)]
          , 0 ) ) ]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (Expr.value (0, 9)) in
    let _drop = ("drop", [], drop) in
    let set_metadata =
      ("nhop", [("nhop", 32)], "meta.nhop" %<-% Var ("nhop", 32))
    in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      [("set_port", [("port", 9)], Expr.Var ("port", 9) |> set_port); _drop]
    in
    sequence
      [ apply
          ("ethernet", [("hdr.ethernet.dstAddr", 48)], classify_actions, drop)
      ; apply ("ipv4", [("hdr.ipv4.dstAddr", 32)], classify_actions, drop)
      ; apply ("nexthop", [("meta.nhop", 32)], fwd_actions, drop) ]
  in
  let expected =
    sequence
      [ assume (Var ("hdr.ipv4.dstAddr", 32) %=% Expr.value (11, 32))
      ; "meta.nhop" %<-% Expr.value (3, 32)
      ; assume (Var ("meta.nhop", 32) %=% Expr.value (3, 32))
      ; set_port (Expr.value (11, 9)) ]
  in
  StaticSlicing.edit_slice params inst edits cmd |> same_cmd expected

let slicing_fabric_example _ =
  let params = Parameters.{default with above= false} in
  let inst =
    let open Instance in
    set_rows empty ~table:"nexthop"
      ~rows:
        [ ( [Match.exact_ "meta.nhop" (Value.make (1, 32))]
          , [Value.make (11, 9)]
          , 0 ) ]
  in
  let edits =
    let open Edit in
    [ Add
        ( "ethernet"
        , ( [Match.exact_ "hdr.ethernet.dstAddr" (Value.make (11, 48))]
          , [Value.make (1, 32)]
          , 0 ) ) ]
  in
  let set_port i = "standard_metadata.egress_spec" %<-% i in
  let cmd =
    let drop = set_port (Expr.value (0, 9)) in
    let _drop = ("drop", [], drop) in
    let set_metadata =
      ("nhop", [("nhop", 32)], "meta.nhop" %<-% Var ("nhop", 32))
    in
    let classify_actions = [set_metadata; _drop] in
    let fwd_actions =
      [("set_port", [("port", 9)], Expr.Var ("port", 9) |> set_port); _drop]
    in
    sequence
      [ apply
          ("ethernet", [("hdr.ethernet.dstAddr", 48)], classify_actions, drop)
      ; apply ("nexthop", [("meta.nhop", 32)], fwd_actions, drop) ]
  in
  let expected =
    sequence
      [ assume (Var ("hdr.ethernet.dstAddr", 48) %=% Expr.value (11, 48))
      ; "meta.nhop" %<-% Expr.value (1, 32)
      ; assume (Var ("meta.nhop", 32) %=% Expr.value (1, 32))
      ; set_port (Expr.value (11, 9)) ]
  in
  StaticSlicing.edit_slice params inst edits cmd |> same_cmd expected

let slicing_microbench () =
  let params = Parameters.{default with above= false} in
  let edits = [
      Edit.Add("staging", ([Match.exact_ "x0" @@ Value.make (5,8)],[Value.make (44,8)],0));
      Edit.Add("meta0", ([Match.exact_ "meta" @@ Value.make (44,8)],[Value.make (3,8)],0))
    ] in   
  let expected = Instance.of_edits params edits
  in
  let cmd = RandomGen.Pipe.gen 8 1 1 in
  Printf.printf "%s" (Cmd.to_string cmd);
  let inst = Instance.of_edits params @@
               [ Edit.Add("staging", ([Match.exact_ "x0" @@ Value.make (2,8)],[Value.make (99,8)],0));
                 Edit.Add("meta0", ([Match.exact_ "meta" @@ Value.make (99,8)],[Value.make (88,8)],0))
               ] @ edits in

  StaticSlicing.rule_slice params inst edits cmd
  |> same_inst expected 
  

let edit_slicing_microbench () =
  let params = Parameters.{default with above= false} in
  let edits = [
      Edit.Add("staging", ([Match.exact_ "x0" @@ Value.make (5,8)],[Value.make (44,8)],0));
      Edit.Add("meta0", ([Match.exact_ "meta" @@ Value.make (44,8)],[Value.make (3,8)],0))
    ] in   
  let expected =
    sequence [
        Assume (Var("x0",8) %=% Expr.value (5,8)); 
        "meta" %<-% Expr.value (44,8);
        Assume (Var("meta",8) %=% Expr.value (44,8));
        "y0" %<-% Expr.value (3,8);
      ]
  in
  let cmd = RandomGen.Pipe.gen 8 1 1 in
  Printf.printf "%s" (Cmd.to_string cmd);
  let inst = Instance.of_edits params @@
               [ Edit.Add("staging", ([Match.exact_ "x0" @@ Value.make (2,8)],[Value.make (99,8)],0));
                 Edit.Add("meta0", ([Match.exact_ "meta" @@ Value.make (99,8)],[Value.make (88,8)],0))
               ] in

  StaticSlicing.edit_slice params inst edits cmd
  |> same_cmd expected 
  

  
let test : unit Alcotest.test_case list =
  [ Alcotest.test_case "slices away unnecessary extant rows" `Quick
      slicing_retargeting_metadata_ethernet
  ; Alcotest.test_case "keeps necessary extant rows" `Quick
      slicing_fabric_example
  ; Alcotest.test_case "does the right thing?" `Quick
      slicing_retargeting_metadata_ipv4
  ; Alcotest.test_case "edit slices microbench" `Quick
      edit_slicing_microbench
  ]
