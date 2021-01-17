let () =
  Avenir.Prover.make_provers "z3" ;
  let open Alcotest in
  run "Tests"
    [ ("smart_constructors", AstTest.smart_constructors)
    ; ("weakest precondition", ManipTest.test_wp)
    ; ("semantics", SemanticsTest.test)
    ; ("constant propagation", ConstantPropTest.test)
    ; ("dead code elimination", DeadCodeTest.test)
    ; ("passive propogation", ConstantPropTest.test_passive_prop)
    ; ("meet", ConstantPropTest.test_meet)
    ; ("construct sat queries", ModelFinderTest.test)
    ; ("hints", HintTest.test)
    ; ("packets", PacketTest.diff_test)
    ; ("edits", EditTest.test)
    ; ("bmv2_parser", RuntimeTest.test_bmv2)
    ; ("slicing", StaticSlicingTest.test)
    ; ("model interface", ModelTest.test)
    ; ("value interface", ValueTest.test)
    ; ("random generators", RandomGenTest.test)
    ; ("edit cache serialization", EAbstrTest.test)
    ; ("equivalence", SynthesisTest.equivs) ]
