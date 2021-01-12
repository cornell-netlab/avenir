open Avenir
open RandomGen

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
                    Skip)])
    (Pipe.gen 4 2 2)

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
    (Obt.gen 4 2 2)

let test : unit Alcotest.test_case list =
  [Alcotest.test_case "pipe with 4 bits 2 keys 2 actions" `Quick pipe_generator_422;
   Alcotest.test_case "obt  with 4 bits 2 keys 2 actions" `Quick obt_generator_422;

  ]
