open Core
open Util

let normalize_id id =
  match String.index id '@' with
  | None -> id
  | Some index -> String.drop_prefix id index

let extract_value = function
  | Z3.Smtlib.BitVec (n, w) -> Value.make (n, w)
  | Z3.Smtlib.BigBitVec (n, w) -> Value.big_make (n, w)
  | Z3.Smtlib.Int i -> Value.make (i, Int.max_value)
  | _ -> raise (Failure "not a supported value")

let of_smt_model (lst : (Z3.Smtlib.identifier * Z3.Smtlib.term) list) =
  List.fold lst ~init:StringMap.empty ~f:(fun model (Z3.Smtlib.Id id, x) ->
      let id = normalize_id id in
      let v = extract_value x in
      StringMap.set model ~key:id ~data:v)
