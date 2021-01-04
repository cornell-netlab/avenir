open Core
open Equality
open Avenir
open Ast
open DeadCode


let dc_remove_table _ =
  let var x = Var(x,32) in
  let int i = mkVInt(i,32) in
  let cmd =
    sequence [
        Apply {name = "tbl";
               keys = [("z",32, None); ("meta",32,None)];
               actions = [ "action", ["port",9], sequence [
                                           "out" %<-% mkPlus (Var("port",9)) (mkCast 9 @@ var "z");
                                           "meta" %<-% mkPlus (var "z") (var "z");
                                         ]
                         ; "action", ["port",9], sequence [
                                           "out" %<-% mkVInt (0,9);
                                           "meta" %<-% int (2*99);
                         ] ];
               default = "meta" %<-% mkTimes (var "z") (int 2);
          };
        "addr" %<-% var "meta";
      ]
  in
  let exp = Skip in
  same_cmd exp @@ elim_vars ["mac",48] cmd

let test : unit Alcotest.test_case list =
  [Alcotest.test_case "eliminates irrelevant tables" `Quick dc_remove_table]
