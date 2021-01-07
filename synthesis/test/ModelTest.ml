open Avenir
open Ast
open Equality

let set_eq_alist _ =
  let alist_model =
    Model.of_alist_exn
      ["?x", mkInt(0,1);
       "?y", mkInt(1,1)
      ]
  in
  let set_model =
    Model.empty
    |> Model.set ~key:"?x" ~data:(mkInt(0,1))
    |> Model.set ~key:"?y" ~data:(mkInt(1,1))
  in
  same_model alist_model set_model


let join_is_disjoint_union _ =
  let m1 =
    Model.of_alist_exn
      ["?x", mkInt(0,2);
       "?y", mkInt(1,2)
      ]
  in
  let m2 =
    Model.of_alist_exn
      [ "?y", mkInt(1,2);
        "?w", mkInt(2,2);
        "?z", mkInt(3,2)
      ]
  in
  let mj =
    Model.of_alist_exn
      ["?x", mkInt(0,2);
       "?y", mkInt(1,2);
       "?w", mkInt(2,2);
       "?z", mkInt(3,2)
      ]
  in
  same_model mj @@ Model.join m1 m2


let diff_separates_models _ =
  let m1 =
    Model.of_alist_exn
      ["?x", mkInt(0,2);
       "?y", mkInt(1,2)
      ]
  in
  let m2 =
    Model.of_alist_exn
      ["?y", mkInt(2,2);
       "?z", mkInt(3,2)
      ]
  in
  let md =
    Util.StringMap.of_alist_exn
      ["?y", (mkInt(1,2), mkInt(2,2))]
  in
  same_vv_stringmap md @@ Model.diff m1 m2

let test : unit Alcotest.test_case list = [
    Alcotest.test_case "set constructor and alist constructor produce equivalent maps" `Quick set_eq_alist;
    Alcotest.test_case "join computes union of disjoint maps" `Quick join_is_disjoint_union;
    Alcotest.test_case "diff computes difference in intersection" `Quick diff_separates_models;
  ]
