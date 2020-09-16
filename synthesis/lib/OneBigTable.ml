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

let combine_all_actions actions1 actions2 =
  List.map
    (cross actions1 actions2 |> List.concat)
    ~f:(fun (x, y) -> combine_actions x y)

type only_apply = {keys:(string * size * value option) list;
                   actions: ((string * (string * size) list * cmd) list);
                   default: cmd}

let rec replace_apply_with_def c =
  match c with
  | Skip
  | Assign _
  | Assume _ -> c
  | Seq(c1, c2) -> Seq(replace_apply_with_def c1, replace_apply_with_def c2)
  | Select(st, tc) -> Select(st, List.map tc ~f:(fun (t, c) -> (t, replace_apply_with_def c)))
  | Apply a -> a.default

let rec has_apply c =
  match c with
  | Skip
  | Assign _
  | Assume _ -> false
  | Seq(c1, c2) -> has_apply c1 || has_apply c2
  | Select(_, tc) -> List.fold (List.map tc ~f:(fun (_, c) -> has_apply c)) ~init:false ~f:(||)
  | Apply _ -> true

let empty_only_apply = {keys = []; actions = []; default = Skip}

let rec mk_one_big_table' (tbl : only_apply) c =
  match c with
  | Skip -> tbl
  | Assign _
  | Assume _ ->
    { tbl with
      actions = List.map tbl.actions ~f:(fun (n, p, act) -> (n, p, act %:% c));
      default = tbl.default %:% c }
  | Seq(c1, c2) -> mk_one_big_table' (mk_one_big_table' tbl c1) c2
  | Select(_, tcl) when has_apply c ->
    let free = List.map tcl
                ~f:(fun (t, _) -> List.map (free_vars_of_test t) ~f:(fun(f, s) -> (f, s, None)))  |> List.concat in
    let es_tbl = List.map tcl ~f:snd |> List.map ~f:(mk_one_big_table' empty_only_apply) in
    
    let tbl_keys = {tbl with keys = dedup (tbl.keys @ free)} in
    let acts = ("DEFAULT", [], tbl_keys.default) :: tbl_keys.actions in
    let es_tbl_acts = (List.map es_tbl ~f:(fun t -> "DEFAULT", [], t.default))
                        @ List.concat_map es_tbl ~f:(fun est -> est.actions) in
    let new_acts = combine_all_actions acts es_tbl_acts in
    { tbl_keys with actions = new_acts;
                    default = tbl_keys.default %:% replace_apply_with_def c }
  | Select _ -> (* We can assume no Apply in the Select, given the previous case *)
    { tbl with
      actions = List.map tbl.actions ~f:(fun (n, p, act) -> (n, p, act %:% c));
      default = tbl.default %:% c }
  | Apply app_t ->
    let cross_actions = combine_all_actions tbl.actions app_t.actions in
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

