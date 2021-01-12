open Core
open Avenir
open Ast

let testable_string (type a) (f : a -> string) (eq : a -> a -> bool) =
  Alcotest.testable (Fmt.of_to_string f) (eq)

let expr = testable_string string_of_expr Stdlib.(=)
let same_expr = Alcotest.(check expr) "same expr"

let test = testable_string string_of_test Stdlib.(=)
let same_test = Alcotest.(check test) "same test"

let cmd = testable_string sexp_string_of_cmd Stdlib.(=)
let same_cmd = Alcotest.(check cmd) "same cmd"

let packet = testable_string Packet.string__packet Packet.equal
let same_packet = Alcotest.(check packet) "same packet"

let model = testable_string string_of_map (Util.StringMap.equal veq)
let same_model = Alcotest.(check model) "same model"

let stringlist = testable_string (List.fold ~init:"" ~f:(Printf.sprintf "%s %s")) (Stdlib.(=))
let same_stringlist = Alcotest.(check stringlist) "same string list"

let stringset = testable_string (Util.string_of_strset) (Util.StringSet.equal)
let same_stringset = Alcotest.(check stringset) "same string set"

let edit = testable_string Tables.Edit.to_string Tables.Edit.equal
let same_edit = Alcotest.(check edit) "same edit"

let cache = testable_string EAbstr.string_of_cache EAbstr.cache_eq
let same_cache = Alcotest.(check cache) "same cache"

let edits =
  let open Tables in
  testable_string
    (Edit.list_to_string)
    (fun es es' -> String.(Edit.list_to_string es =  Edit.list_to_string es'))

let same_edits = Alcotest.(check edits) "same edits"
