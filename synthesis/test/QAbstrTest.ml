open Alcotest

let cache_eq _ =
  let initial = Avenir.QAbstr.random () in
  let serialized = Avenir.QAbstr.to_yojson initial in
  let unpack_result yoj =
    match Avenir.QAbstr.of_yojson yoj with
    | Result.Ok e -> e
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into edit") in
  let deserialized = unpack_result serialized in
  Equality.same_qcache initial deserialized

let cache_tests =
  let mk_test i = test_case ("query cache serialization/deserialization should round trip " ^ (string_of_int i)) `Quick cache_eq in
  List.init 5 mk_test

let test : unit Alcotest.test_case list = Random.self_init (); List.concat cache_tests
