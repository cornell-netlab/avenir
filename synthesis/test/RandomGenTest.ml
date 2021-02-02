open Avenir
open RandomGen

let obt_generator_422 _ =
  let open Cmd in
  Equality.same_cmd
    (sequence
       [ apply
           ( "onebigtable"
           , [("x0", 4); ("x1", 4)]
           , [ ("action0", [("arg0", 4)], "y0" %<-% Var ("arg0", 4))
             ; ("action1", [("arg1", 4)], "y1" %<-% Var ("arg1", 4)) ]
           , "meta" %<-% Value (Value.zero 4) ) ])
    (Obt.gen 4 2 2 |> fst)

let pipe_generator_422 _ =
  let open Cmd in
  Equality.same_cmd
    (sequence
       [ apply
           ( "staging"
           , [("x0", 4); ("x1", 4)]
           , [("set_meta", [("m", 4)], assign "meta" (Var ("m", 4)))]
           , "meta" %<-% Value (Value.zero 4) )
       ; apply
           ( "meta0"
           , [("meta", 4)]
           , [("action0", [("arg0", 4)], "y0" %<-% Var ("arg0", 4))]
           , Skip )
       ; apply
           ( "meta1"
           , [("meta", 4)]
           , [("action1", [("arg1", 4)], "y1" %<-% Var ("arg1", 4))]
           , Skip )
       ; apply
           ( "meta2"
           , [("meta", 4)]
           , [("action2", [("arg2", 4)], "y2" %<-% Var ("arg2", 4))]
           , Skip ) ])
    (Pipe.gen 4 2 3)

let random_test_422 _ =
  Random.init 101 ;
  Log.set_debug () ;
  Log.set_z3 () ;
  Log.set_info () ;
  let edits = Obt.rand_edits 4 2 2 10 in
  let obt, fvs = Obt.gen 4 2 2 in
  let pip = Pipe.gen 4 2 2 in
  let problem_mkr =
    Problem.make ~log:obt ~phys:pip ~fvs ~log_edits:edits
      ~log_inst:Instance.empty ~phys_inst:Instance.empty
  in
  EAbstr.make () ;
  let es =
    Synthesis.cegis_math_sequence
      Parameters.
        { default with
          edits_depth= 2
        ; fastcx= true
        ; hints= true
        ; hint_type= "exact"
        ; no_defaults= true
        ; no_deletes= true
        ; ecache= Some 1 }
      (ProfData.zero ()) problem_mkr
  in
  Alcotest.(check bool) "synthesis finds an answer" true (Option.is_some es)

let test : unit Alcotest.test_case list =
  [ Alcotest.test_case "pipe with 4 bits 2 keys 2 actions" `Quick
      pipe_generator_422
  ; Alcotest.test_case "obt  with 4 bits 2 keys 2 actions" `Quick
      obt_generator_422
  ; Alcotest.test_case "obt->pipe (10) with 4 bits 2 keys 2 actions" `Slow
      random_test_422 ]
