(* open Avenir.EAbstr *)
open Avenir.Match
open Avenir.Value
open Avenir.Edit
open Alcotest

let int_bound = 1024

let random_value () =
  let size = (Random.int 29) + 1 in
  Printf.printf "%d\n" size;
  random size

let random_match () =
  match Random.int 3 with
  | 0 -> exact_ "match_placeholder" (random_value ())
  | 1 -> between_ "match_placeholder" (random_value ()) (random_value ())
  | _ -> mask_ "match_placeholder" (random_value ()) (random_value ())

let random_action_data () =
  let rv _ = random_value () in
  let len = Random.int 8 in
  List.init len rv

let random_row () =
  let rv _ = random_match () in
  let len = Random.int 8 in
  let matches = List.init len rv in
  (matches, random_action_data (), Random.int int_bound)

let random_edit () =
  match Random.int 2 with
  | 0 -> Add ("edit_placeholder", random_row ())
  | _ -> Del ("edit_placeholder", Random.int int_bound)

let random_cache () =
  let cache_len = Random.int 16 in
  let random_entry _ =
    let re _ = random_edit () in
    let entry_len = Random.int 8 in
    let first = random_edit () in
    (first, List.init entry_len re) in
  List.init cache_len random_entry

let edit_eq _ =
  let initial = random_edit () in
  let serialized = Avenir.Edit.to_yojson initial in
  let unpack_result yoj =
    match Avenir.Edit.of_yojson yoj with
    | Result.Ok e -> e
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into edit") in
  let deserialized = unpack_result serialized in
  Equality.same_edit initial deserialized

let cache_eq _ =
  let initial = random_cache () in
  let serialized = Avenir.EAbstr.to_yojson initial in
  let unpack_result yoj =
    match Avenir.EAbstr.of_yojson yoj with
    | Result.Ok c -> c
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into cache") in
  let deserialized = unpack_result serialized in
  Equality.same_cache initial deserialized

(* let edit_tests =
  let mk_test i = test_case ("edit serialization/deserialization should round trip " ^ (string_of_int i)) `Quick edit_eq in
  List.init 5 mk_test *)

let cache_tests =
  let mk_test i = test_case ("cache serialization/deserialization should round trip " ^ (string_of_int i)) `Quick cache_eq in
  List.init 5 mk_test

let test : unit Alcotest.test_case list = Random.self_init (); List.concat [cache_tests]