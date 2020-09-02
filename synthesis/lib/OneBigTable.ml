open Core
open Ast

open Manip

module StringMap = Map.Make (String)

let cross xs ys = List.map xs ~f:(fun x -> List.map ys ~f:(fun y -> (x, y)))

let combine_actions (act1 : string * (string * size) list * cmd) (act2 : string * (string * size) list * cmd) : string * (string * size) list * cmd =
  let (n1, p1, c1) = act1 in
  let (n2, p2, c2) = act2 in

  let uniquify_var ev (v, w) = (v, v ^ ev, w) in

  let new_p1 = List.map p1 ~f:(uniquify_var "_1") in
  let new_p2 = List.map p2 ~f:(uniquify_var "_2") in

  let remap v1 v2 = StringMap.of_alist_exn [(v1, v2)] in
  let new_c1 = List.fold new_p1 ~init:c1 ~f:(fun e (v1, v2, w) -> substitute_cmd e (remap v1 (Var (v2, w))) ~holes:false) in
  let new_c2 = List.fold new_p2 ~init:c2 ~f:(fun e (v1, v2, w) -> substitute_cmd e (remap v1 (Var (v2, w))) ~holes:false) in

  let new_p1' = List.map new_p1 ~f:(fun (_, v, w) -> v, w) in
  let new_p2' = List.map new_p2 ~f:(fun (_, v, w) -> v, w) in
  n1 ^ "_" ^ n2, new_p1' @ new_p2', new_c1 %:% new_c2

type only_apply = {keys:(string * size * value option) list;
                   actions: ((string * (string * size) list * cmd) list);
                   default: cmd}

let rec mk_one_big_table' (tbl : only_apply) c =
  match c with
  | Skip -> tbl
  | Assign _ ->
    { tbl with
      actions = List.map tbl.actions ~f:(fun (n, p, act) -> (n, p, act %:% c));
      default = tbl.default %:% c }
  | Assume _ -> failwith "Assume not handled"
  | Seq(c1, c2) -> mk_one_big_table' (mk_one_big_table' tbl c1) c2
  | Select(_, tcl) ->
    let free = List.map tcl
                ~f:(fun (t, _) -> List.map (free_vars_of_test t) ~f:(fun(f, s) -> (f, s, None)))  |> List.concat in
    let es = List.map tcl ~f:snd in
    
    let tbl_keys = {tbl with keys = dedup (tbl.keys @ free)} in
    List.fold es ~init:tbl_keys ~f:mk_one_big_table'
  | Apply app_t ->
    let cross_actions = List.map
                          (cross tbl.actions app_t.actions |> List.concat)
                          ~f:(fun (x, y) -> combine_actions x y) in
    let def_tbl_to_app_t = List.map app_t.actions ~f:(combine_actions ("DEFAULT", [], tbl.default)) in
    let tbl_to_def_app_t = List.map tbl.actions ~f:(fun t -> combine_actions t ("DEFAULT", [], app_t.default)) in
    { keys = dedup (tbl.keys @ app_t.keys);
      actions = def_tbl_to_app_t @ tbl_to_def_app_t @ cross_actions;
      default = tbl.default %:% app_t.default }

let mk_one_big_table c =
  let app_t = mk_one_big_table' { keys = []; actions = []; default = Skip } c in
  Apply { name = "OneBigTable";
          keys = app_t.keys;
          actions = app_t.actions;
          default = app_t.default }

