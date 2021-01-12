(* open Avenir.EAbstr *)
open Avenir.Ast
open Avenir.Match
open Avenir.Tables
open Alcotest

(** 2^30 - 1 *)
let int_bound = 1073741823

let random_value () = Int ((Bigint.of_int (Random.int int_bound)), (Random.int int_bound))

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
  | 0 -> Edit.Add ("edit_placeholder", random_row ())
  | _ -> Edit.Del ("edit_placeholder", Random.int int_bound)

let random_cache () =
  let cache_len = Random.int 16 in
  let random_entry _ =
    let re _ = random_edit () in
    let entry_len = Random.int 8 in
    let first = random_edit () in
    (first, List.init entry_len re) in
  List.init cache_len random_entry

let value_eq _ =
  let initial = random_value () in
  let serialized = value_to_yojson initial in
  let unpack_result yoj =
    match value_of_yojson yoj with
    | Result.Ok v -> v
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into value") in
  let deserialized = unpack_result serialized in
  Equality.same_value initial deserialized

let match_eq _ =
  let initial = random_match () in
  let serialized = Avenir.Match.to_yojson initial in
  let unpack_result yoj =
    match Avenir.Match.of_yojson yoj with
    | Result.Ok m -> m
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into match") in
  let deserialized = unpack_result serialized in
  Equality.same_match initial deserialized

let row_eq _ =
  let initial = random_row () in
  let serialized = Avenir.Tables.Row.to_yojson initial in
  let unpack_result yoj =
    match Avenir.Tables.Row.of_yojson yoj with
    | Result.Ok r -> r
    | _ -> failwith ("Error occurred while deserializing yojson " ^ (Yojson.Safe.to_string yoj) ^ " into row") in
  let deserialized = unpack_result serialized in
  Equality.same_row initial deserialized

let edit_eq _ =
  let initial = random_edit () in
  let serialized = Avenir.Tables.Edit.to_yojson initial in
  let unpack_result yoj =
    match Avenir.Tables.Edit.of_yojson yoj with
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

let edit_tests =
  let mk_test i = test_case ("edit serialization/deserialization should round trip " ^ (string_of_int i)) `Quick edit_eq in
  List.init 5 mk_test

(* let cache_tests =
  let mk_test i = test_case ("cache serialization/deserialization should round trip " ^ (string_of_int i)) `Quick cache_eq in
  List.init 5 mk_test *)

let test : unit Alcotest.test_case list = List.concat [edit_tests]