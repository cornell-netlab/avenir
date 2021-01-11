open Core
open Ast


let add_row_prefix = "?AddRowTo"
let delete_row_prefix = "?delete"
let delete_row_infix = "In"
let which_act_prefix = "?ActIn"


let prefix = Printf.sprintf "%s%s"

let add_row_hole_name = prefix add_row_prefix

let delete_row_hole_name i tbl = Printf.sprintf "%s%d%s%s" delete_row_prefix i delete_row_infix tbl
let is_delete_hole n = String.(is_prefix ~prefix:delete_row_prefix n
                               && is_substring ~substring:delete_row_infix n)

let which_act_hole_name = prefix which_act_prefix
let is_which_act_hole = String.is_prefix ~prefix:which_act_prefix

let find_add_row = String.chop_prefix ~prefix:add_row_prefix
let is_add_row_hole = String.is_prefix ~prefix:add_row_prefix

let find_delete_row key =
  match String.chop_prefix key ~prefix:delete_row_prefix with
  | None -> None
  | Some idx_tbl ->
     match String.substr_index idx_tbl ~pattern:delete_row_infix with
     | None -> None
     | Some idx ->
        let row_idx = String.prefix idx_tbl idx |> int_of_string in
        let table_name = String.drop_prefix idx_tbl (idx + String.length delete_row_infix) in
        Some (table_name, row_idx)


let delete_hole i tbl = Expr.Hole(delete_row_hole_name i tbl, 1)
let add_row_hole tbl = Expr.Hole (add_row_hole_name tbl, 1)
let which_act_hole tbl actSize =
  assert (actSize > 0);
  Expr.Hole (which_act_hole_name tbl, actSize)

let match_hole_exact tbl x = Printf.sprintf "?%s_%s" x tbl
let match_holes_range tbl x =
  (Printf.sprintf "%s_lo" (match_hole_exact tbl x)
  , Printf.sprintf "%s_hi" (match_hole_exact tbl x))
let match_holes_mask tbl x = (match_hole_exact tbl x
                             , Printf.sprintf "%s_mask" (match_hole_exact tbl x))

let match_holes encode_tag tbl x sz =
  match encode_tag with
  | `Mask ->
     let (hv,hm) = match_holes_mask tbl x in
     Test.(Expr.(mask (Var(x, sz)) (Hole (hm,sz))) %=% Hole (hv, sz))
  | `Exact ->
     let h = match_hole_exact tbl x in
     Test.(Var(x, sz) %=% Hole (h,sz))

let match_holes_table encode_tag tbl keys  =
  let open Test in
  List.fold keys ~init:True
    ~f:(fun acc (x,sz,v_opt) ->
      acc %&% match v_opt with
              | None -> match_holes encode_tag tbl x sz
              | Some _ -> True
    )


let action_data_prefix tbl i = Printf.sprintf "%s_%d_" tbl i
(* let action_data_prefix _ _ = Printf.sprintf "" *)

let action_data tbl i v sz = Printf.sprintf "%s%s_%d" (action_data_prefix tbl i) v sz
let action_data_hole tbl i v sz = Expr.Hole(action_data tbl i v sz, sz)


let table_hole encode_tag (keys: (string * size * Value.t option) list) tbl actID actSize =
  let open Test in
  match_holes_table encode_tag tbl keys
  %&% (add_row_hole tbl %=% Expr.value (1,1))
  %&% (which_act_hole tbl actSize %=% Expr.value (actID,actSize))
