open Avenir
open RandomGen

let obt_generator_422 _ =
  let open Cmd in
  Equality.same_cmd
    (sequence[
             apply ("onebigtable",
                    [("x0",4); ("x1",4)],
                    ["action0", [("arg0",4)], "y0" %<-% Var ("arg0",4);
                     "action1", [("arg1",4)], "y1" %<-% Var ("arg1",4)
                    ],
                    "meta" %<-% Value (Value.zero 4))])
    (Obt.gen
           4 (* bitwidth *)
           2 (* number of keys *)
           2 (* number of outputs/actions*))

let pipe_generator_422 _ =
  let open Cmd in
  Equality.same_cmd
    (sequence[
             apply ("staging",
                    [("x0",4); ("x1",4)],
                    ["set_meta", [("m",4)], assign "meta" (Var ("m",4))],
                    "meta" %<-% Value (Value.zero 4));
             apply ("meta0",
                    [("meta", 4)],
                    ["action0", [("arg0",4)], "y0" %<-% Var ("arg0",4)],
                    Skip);
             apply ("meta1",
                    [("meta", 4)],
                    ["action1", [("arg1",4)], "y1" %<-% Var ("arg1",4)],
                    Skip);
             apply ("meta2",
                    [("meta", 4)],
                    ["action2", [("arg2",4)], "y2" %<-% Var ("arg2",4)],
                    Skip)])
    (Pipe.gen
           4 (* bitwidth *)
           2 (* number of keys - 1 *)
           3 (* number of outputs/actions/tables*))

let test : unit Alcotest.test_case list =
  [Alcotest.test_case "pipe with 4 bits 2 keys 2 actions" `Quick pipe_generator_422;
   Alcotest.test_case "obt  with 4 bits 2 keys 2 actions" `Quick obt_generator_422;
  ]
