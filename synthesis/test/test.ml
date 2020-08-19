open Core
open Avenir
open Ast

let testable_string (type a) (f : a -> string) (eq : a -> a -> bool) =
  Alcotest.testable (Fmt.of_to_string f) (eq)


let test = testable_string string_of_test Stdlib.(=)
let same_test = Alcotest.(check test) "same test"

(* Testing equality smart constructor *)
let eq_test _ =
  let exp = Var ("x",8) %=%  mkVInt(7,8) in
  let got = mkVInt (7,8) %=% Var ("x",8) in
  same_test exp got


(*Testing weakest preconditions*)
let wp_skip_eq _ =
  let open Manip in
  let phi = (Var("x",8) %=% Var("y",8)) in
  same_test phi (wp `Negs Skip phi)

let wp_int_assign_eq _ =
  let open Manip in
  let pre = mkVInt(7,8) %=% Var ("g",8) in
  let cmd = "h" %<-% mkVInt(7,8) in
  let post = Var("h",8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_var_assign_eq _ =
  let open Manip in
  let pre = Var("hgets",8) %=% Var ("g",8) in
  let cmd = "h" %<-% Var("hgets",8) in
  let post = Var("h",8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_ordered_eq _ =
  let open Manip in
  let post = Var("g",8) %=% mkVInt(8,8) in
  let pre =
    bigand [
        bigand [
            Var ("h",8) %<>% mkVInt (2,8);
            mkNeg @@ bigor [Var("h",8) %=% mkVInt(99,8);
                            Var("h",8) %=% Var("g",8)] ;
          ] %=>% post;
        bigand [
            Var("h",8) %=%  mkVInt(99,8);
            Var("h",8) %<>% Var("g",8)
        ] %=>% post;
        Var("h",8) %=% Var ("g",9) %=>% (mkVInt(8,8) %=% mkVInt(8,8));
      ]
  in
  let cmd =
    mkOrdered [ Var ("h",8) %=%  Var ("g",8)    , "g" %<-% mkVInt (8,8);
                Var ("h",8) %=%  mkVInt (99,8)  , "h" %<-% mkVInt (4,8);
                Var ("h",8) %<>% mkVInt (2,8)   , "h" %<-% Var    ("g",8)
      ]
  in
  let post = Var("g",8) %=% mkVInt(8,8) in
  same_test pre (wp `Negs cmd post)

let wp_seq_eq _ =
  let open Manip in
  let cmd = ("h" %<-% mkVInt (10,8)) %:% ("h" %<-% mkVInt (80,8)) in
  let post = Var ("h",8) %=% Var ("g",8) in
  let pre = mkVInt (80,8) %=% Var ("g",8) in
  same_test pre (wp `Negs cmd post)


let wp_assume_eq _ = (* wp behaves well with assertions *)
  let open Manip in
  let phi = bigand [Var ("h",8) %<>% mkVInt (10,8); Var ("h",8) %<>% mkVInt (15,8)] in
  let cmd = Assume(phi) in
  let post =  Var ("h",8) %=% Var ("g",8) in
  same_test (phi %=>% post) (wp `Negs cmd post)



(*Semantics*)
let packet = testable_string Packet.string__packet Packet.equal

let same_packet = Alcotest.(check packet) "same packet"
let semantics =
  let open Semantics in
  let dst_is x = Var("dst", 2) %=% mkVInt(x,2) in
  let src_is x = Var("src", 2) %=% mkVInt(x,2) in
  let prog =
    sequence [
    mkOrdered [
        src_is 1, "smac" %<-% mkVInt(2,2);
        src_is 0, "smac" %<-% mkVInt(1,2);
        True, Skip
      ];
      mkOrdered [
          dst_is 1, "out" %<-% mkVInt(2,2);
          dst_is 0, "out" %<-% mkVInt(1,2);
          True, Skip ]
      ]
  in
  let update f j pkt = Packet.set_field pkt f (mkInt(j,2)) in
  let mkPacket i j = Packet.empty |> update "src" i |> update "dst" j in
  let update f j pkt = Packet.set_field pkt f (mkInt(j,2)) in
  let pkt00 = mkPacket 0 0 in
  let pkt01 = mkPacket 0 1 in
  let pkt10 = mkPacket 1 0 in
  let pkt11 = mkPacket 1 1 in
  let run = eval_act prog in
  [ run pkt00, pkt00 |> update "smac" 1 |> update "out" 1;
    run pkt01, pkt01 |> update "smac" 1 |> update "out" 2;
    run pkt10, pkt10 |> update "smac" 2 |> update "out" 1;
    run pkt11, pkt11 |> update "smac" 2 |> update "out" 2;
  ]







let () =
  let open Alcotest in
  run "Tests" [
      "smart constructors",
      [test_case "equality (Var, Value)" `Quick eq_test];
      "weakest precondition",
      [test_case "wp(skip,phi) = phi" `Quick wp_skip_eq;
       test_case "int assignment" `Quick wp_int_assign_eq;
       test_case "var assignment" `Quick wp_var_assign_eq;
       test_case "ordered" `Quick wp_ordered_eq;
       test_case "sequence" `Quick wp_seq_eq;
       test_case "assume" `Quick wp_assume_eq;
      ];
      "semantics",
      List.map semantics ~f:(fun (run_pkt, ex_pkt) ->
          test_case "cross product" `Quick @@
            fun _ -> same_packet ex_pkt run_pkt);

    ]
