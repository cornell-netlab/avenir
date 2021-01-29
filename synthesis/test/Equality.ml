open Core
open Avenir

let testable_string (type a) (f : a -> string) (eq : a -> a -> bool) =
  Alcotest.testable (Fmt.of_to_string f) eq

let expr = testable_string Expr.to_string Expr.equals

let same_expr = Alcotest.(check expr) "same expr"

let test = testable_string Avenir.Test.to_string Avenir.Test.equals

let same_test = Alcotest.(check test) "same test"

let cmd = testable_string Cmd.to_sexp_string Stdlib.( = )

let same_cmd = Alcotest.(check cmd) "same cmd"

let packet = testable_string Packet.to_string Packet.equal

let same_packet = Alcotest.(check packet) "same packet"

let vv_stringmap =
  testable_string
    (Util.string_of_strmap ~to_string:(fun (v1, v2) ->
         Printf.sprintf "(%s, %s)" (Value.to_string v1) (Value.to_string v2)))
    (Util.StringMap.equal (fun (v1, v2) (v1', v2') ->
         Value.eq v1 v1' && Value.eq v2 v2'))

let same_vv_stringmap =
  Alcotest.(check vv_stringmap) "same value^2 string map"

(* let packet = testable_string string_of_map (Util.StringMap.equal veq)
 * let same_packet = Alcotest.(check packet) "same packet" *)

let model = testable_string Model.to_string Model.equal

let same_model = Alcotest.(check model) "same model"

let stringlist =
  testable_string
    (List.fold ~init:"" ~f:(Printf.sprintf "%s %s"))
    Stdlib.( = )

let same_stringlist = Alcotest.(check stringlist) "same string list"

let stringset = testable_string Util.string_of_strset Util.StringSet.equal

let same_stringset = Alcotest.(check stringset) "same string set"

let edits =
  testable_string Edit.list_to_string (fun es es' ->
      String.(Edit.list_to_string es = Edit.list_to_string es'))

let same_edits = Alcotest.(check edits) "same edits"

let value = testable_string Value.to_string Value.eq

let valuele = testable_string Value.to_string Value.leq

let same_values = Alcotest.(check value) "same values"

let diff_values = Alcotest.(check (neg value)) "different values"

let leq_values = Alcotest.(check valuele) "less than"

let instance = testable_string Instance.to_string Instance.equal
let same_inst = Alcotest.(check instance) "same instances"
