open Core
open Equality
open Avenir
open DeadCode

let dc_remove_table _ =
  let open Cmd in
  let open Expr in
  let var x = Var (x, 32) in
  let int i = value (i, 32) in
  let cmd =
    sequence
      [ Apply
          { name= "tbl"
          ; keys= [Key.make ("z", 32); Key.make ("meta", 32)]
          ; actions=
              [ ( "action"
                , [("port", 9)]
                , sequence
                    [ "out" %<-% plus (Var ("port", 9)) (cast 9 @@ var "z")
                    ; "meta" %<-% plus (var "z") (var "z") ] )
              ; ( "action"
                , [("port", 9)]
                , sequence ["out" %<-% value (0, 9); "meta" %<-% int (2 * 99)]
                ) ]
          ; default= "meta" %<-% times (var "z") (int 2) }
      ; "addr" %<-% var "meta" ]
  in
  let exp = Skip in
  same_cmd exp @@ elim_vars [("mac", 48)] cmd

let test : unit Alcotest.test_case list =
  [Alcotest.test_case "eliminates irrelevant tables" `Quick dc_remove_table]
