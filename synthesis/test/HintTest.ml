open Equality
open Avenir
open Ast
open Hint

 

let hints_injects_keys _ =
  let matches =
    let open Match in
    [exact_ "x" (mkInt (5,32));
     wildcard "y" 32;
     exact_ "q" (mkInt(55,32))]
    in
  let phys =
    sequence [
        mkApply ("p1", ["x",32;"y",32], ["action", [],Skip], Skip);
        mkOrdered [
            Var("x",32) %=% mkVInt(100,32),
            mkApply ("p2a", ["y", 32; "q",32], ["action", [], Skip], Skip);

            True,
            mkApply("p2b", ["x",32; "q",32; "z", 32], ["action", [], Skip], Skip);
          ]
      ]
  in
  let edit = Edit.Add ("logical", (matches, [], 0)) in
  let model = construct phys edit |> list_to_model `NoVals phys in
  let expected = Model.of_alist_exn
                   [ "?x_p2b_mask", Int(Util.max_int 32,32);
                     "?q_p2b_mask", Int(Util.max_int 32,32);
                     "?z_p2b", mkInt(0,32);
                     "?z_p2b_mask", mkInt(0,32);
                   ]
  in
  same_model expected model

let test : unit Alcotest.test_case list =
  [Alcotest.test_case "injects logical keys into physical table" `Quick hints_injects_keys]
