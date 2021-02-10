open Alcotest

let edit_eq _ =
  let initial = Avenir.Edit.random () in
  let serialized = Avenir.Edit.to_yojson initial in
  let unpack_result yoj =
    match Avenir.Edit.of_yojson yoj with
    | Result.Ok e -> e
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into edit") in
  let deserialized = unpack_result serialized in
  Equality.same_edit initial deserialized

let cache_eq _ =
  let initial = Avenir.EAbstr.random_mapping () in
  let serialized = Avenir.EAbstr.mapping_to_yojson initial in
  let unpack_result yoj =
    match Avenir.EAbstr.mapping_of_yojson yoj with
    | Result.Ok c -> c
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into cache") in
  let deserialized = unpack_result serialized in
  Equality.same_mapping initial deserialized

(* let edit_tests =
  let mk_test i = test_case ("edit serialization/deserialization should round trip " ^ (string_of_int i)) `Quick edit_eq in
  List.init 5 mk_test *)

let cache_tests =
  let mk_test i = test_case ("cache serialization/deserialization should round trip " ^ (string_of_int i)) `Quick cache_eq in
  List.init 5 mk_test

let test : unit Alcotest.test_case list = Random.self_init (); List.concat [cache_tests]
