open Avenir

let microbench_equiv_test _ =
  let open Avenir.Test in
  let open Cmd in
  let obt =
    ordered
      [ ( bigand
            [ Var ("x0", 4) %=% Expr.value (10, 4)
            ; Var ("x1", 4) %=% Expr.value (9, 4) ]
        , "y1" %<-% Expr.value (7, 4) )
      ; (True, "meta" %<-% Expr.value (0, 4)) ]
  in
  let pip =
    sequence
      [ ordered
          [ ( bigand
                [ Var ("x0", 4) %=% Expr.value (10, 4)
                ; Var ("x1", 4) %=% Expr.value (9, 4) ]
            , "meta" %<-% Expr.value (12, 4) )
          ; (True, "meta" %<-% Expr.value (0, 4)) ]
      ; Skip
      ; ordered
          [ ( Var ("meta", 4) %=% Expr.value (12, 4)
            , "y1" %<-% Expr.value (7, 4) )
          ; (True, Skip) ] ]
  in
  let cex_opt =
    Synthesis.implements Parameters.default (ProfData.zero ())
      (Problem.make ~log:obt ~phys:pip
         ~fvs:[("x0", 4); ("x1", 4); ("y0", 4); ("y1", 4)]
         ~log_inst:Instance.empty ~phys_inst:Instance.empty ~log_edits:[] () )
  in
  match cex_opt with
  | None -> Alcotest.(check pass) "equivalent" true true
  | Some (inp, _) ->
      Printf.printf "IN : \n%s\n -> \n%!" (Packet.to_string inp) ;
      Printf.printf "OBT OUT: %s\n%!"
        (Semantics.eval_act obt inp |> Packet.to_string) ;
      Printf.printf "PIP OUT: %s\n%!"
        (Semantics.eval_act pip inp |> Packet.to_string) ;
      Alcotest.(check @@ fail "inequiv") "equivalent" true false

let equivs : unit Alcotest.test_case list =
  [ Alcotest.test_case "is a solution to obt->pipe(4,4,2)" `Quick
      microbench_equiv_test ]
