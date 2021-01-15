open Alcotest
open Avenir
open Avenir.Test
open Manip
open Equality

(*Testing weakest preconditions*)
let wp_skip_eq _ =
  let phi = Var ("x", 8) %=% Var ("y", 8) in
  Equality.same_test phi (wp `Negs Skip phi)

let wp_int_assign_eq _ =
  let open Cmd in
  let pre = Expr.value (7, 8) %=% Var ("g", 8) in
  let cmd = "h" %<-% Expr.value (7, 8) in
  let post = Var ("h", 8) %=% Var ("g", 8) in
  same_test pre (wp `Negs cmd post)

let wp_var_assign_eq _ =
  let open Cmd in
  let pre = Var ("hgets", 8) %=% Var ("g", 8) in
  let cmd = "h" %<-% Var ("hgets", 8) in
  let post = Var ("h", 8) %=% Var ("g", 8) in
  same_test pre (wp `Negs cmd post)

let wp_ordered_eq _ =
  let open Cmd in
  let open Expr in
  let post = Var ("g", 8) %=% value (8, 8) in
  let pre =
    bigand
      [ bigand
          [ Var ("h", 8) %<>% value (2, 8)
          ; neg
            @@ bigor
                 [ Var ("h", 8) %=% value (99, 8)
                 ; Var ("h", 8) %=% Var ("g", 8) ] ]
        %=>% post
      ; bigand
          [Var ("h", 8) %=% value (99, 8); Var ("h", 8) %<>% Var ("g", 8)]
        %=>% post
      ; Var ("h", 8) %=% Var ("g", 9) %=>% (value (8, 8) %=% value (8, 8)) ]
  in
  let cmd =
    ordered
      [ (Var ("h", 8) %=% Var ("g", 8), "g" %<-% value (8, 8))
      ; (Var ("h", 8) %=% value (99, 8), "h" %<-% value (4, 8))
      ; (Var ("h", 8) %<>% value (2, 8), "h" %<-% Var ("g", 8)) ]
  in
  let post = Var ("g", 8) %=% value (8, 8) in
  same_test pre (wp `Negs cmd post)

let wp_seq_eq _ =
  let open Cmd in
  let open Expr in
  let cmd = "h" %<-% value (10, 8) %:% ("h" %<-% value (80, 8)) in
  let post = Var ("h", 8) %=% Var ("g", 8) in
  let pre = value (80, 8) %=% Var ("g", 8) in
  same_test pre (wp `Negs cmd post)

let wp_assume_eq _ =
  (* wp behaves well with assumptions *)
  let open Cmd in
  let open Manip in
  let open Expr in
  let phi =
    bigand [Var ("h", 8) %<>% value (10, 8); Var ("h", 8) %<>% value (15, 8)]
  in
  let cmd = assume phi in
  let post = Var ("h", 8) %=% Var ("g", 8) in
  same_test (phi %=>% post) (wp `Negs cmd post)

let test_wp : unit test_case list =
  [ test_case "wp(skip,phi) = phi" `Quick wp_skip_eq
  ; test_case "int assignment" `Quick wp_int_assign_eq
  ; test_case "var assignment" `Quick wp_var_assign_eq
  ; test_case "ordered" `Quick wp_ordered_eq
  ; test_case "sequence" `Quick wp_seq_eq
  ; test_case "assume" `Quick wp_assume_eq ]
