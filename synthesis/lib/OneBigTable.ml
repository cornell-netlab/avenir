open Core
open Ast

let renameVar (old : string) (nw : string) (vw : string * size) : string * size =
  match vw with
  | (v, w) -> (if v = old then nw else v), w

let rec rename_expr (old : string) (nw : string) e =
  match e with
  | Value _ -> e
  | Var(s, w) -> if s = old then Var(nw, w) else e
  | Hole _ -> e
  | Plus(e1, e2) -> mkPlus (rename_expr old nw e1) (rename_expr old nw e2)
  | Times(e1, e2) -> mkTimes (rename_expr old nw e1) (rename_expr old nw e2)
  | Minus(e1, e2) -> mkMinus (rename_expr old nw e1) (rename_expr old nw e2)
  | Mask(e1, e2) -> mkMask (rename_expr old nw e1) (rename_expr old nw e2)
  | Xor(e1, e2) -> mkXor (rename_expr old nw e1) (rename_expr old nw e2)
  | BOr(e1, e2) -> mkBOr (rename_expr old nw e1) (rename_expr old nw e2)
  | Shl(e1, e2) -> mkShl (rename_expr old nw e1) (rename_expr old nw e2)
  | Concat(e1, e2) -> mkConcat (rename_expr old nw e1) (rename_expr old nw e2)
  | Cast(i, e2) -> mkCast i (rename_expr old nw e2)
  | Slice {hi; lo; bits} -> mkSlice hi lo (rename_expr old nw bits)

let rec rename_test (old : string) (nw : string) t =
  match t with
  | True | False -> t
  | Eq(e1, e2) -> mkEq (rename_expr old nw e1) (rename_expr old nw e2)
  | Le(e1, e2) -> mkLe (rename_expr old nw e1) (rename_expr old nw e2)
  | And(t1, t2) -> mkAnd (rename_test old nw t1) (rename_test old nw t2)
  | Or(t1, t2) -> mkOr (rename_test old nw t1) (rename_test old nw t2)
  | Impl(t1, t2) -> mkImpl (rename_test old nw t1) (rename_test old nw t2)
  | Iff(t1, t2) -> mkIff (rename_test old nw t1) (rename_test old nw t2)
  | Neg t1 -> mkNeg (rename_test old nw t1)

let rec rename_cmd (old : string) (nw : string) c =
  match c with
  | Skip -> Skip
  | Assign(s, e) -> mkAssn (if s = old then nw else s) (rename_expr old nw e)
  | Assert t -> Assert (rename_test old nw t)
  | Assume t -> mkAssume (rename_test old nw t)
  | Seq(c1, c2) -> rename_cmd old nw c1 %:% rename_cmd old nw c2
  | While(t, c1) -> mkWhile (rename_test old nw t) (rename_cmd old nw c1)
  | Select(s_typ, tcl) ->
      mkSelect s_typ (List.map tcl ~f:(fun (t, c1) -> (rename_test old nw t, rename_cmd old nw c1)))
  | Apply {name; keys; actions; default } ->
    mkApply (name,
            List.map keys ~f:(fun (s, w) -> (if s = old then nw else s), w),
            List.map actions
              ~f:(fun (params, act_c) -> (List.map params ~f:(renameVar old nw), rename_cmd old nw act_c)),
            rename_cmd old nw default)

let cross xs ys = List.map xs ~f:(fun x -> List.map ys ~f:(fun y -> (x, y)))

let combine_actions (act1 : (string * size) list * cmd) (act2 : (string * size) list * cmd) : (string * size) list * cmd =
  let (p1, c1) = act1 in
  let (p2, c2) = act2 in

  let uniquify_var en (n, w) = (n, n ^ en, w) in

  let new_p1 = List.map p1 ~f:(uniquify_var "_1") in
  let new_p2 = List.map p2 ~f:(uniquify_var "_2") in

  let new_c1 = List.fold new_p1 ~init:c1 ~f:(fun e (v1, v2, _) -> rename_cmd v1 v2 e) in
  let new_c2 = List.fold new_p2 ~init:c2 ~f:(fun e (v1, v2,_) -> rename_cmd v1 v2 e) in

  let new_p1' = List.map new_p1 ~f:(fun (_, v, w) -> v, w) in
  let new_p2' = List.map new_p2 ~f:(fun (_, v, w) -> v, w) in
  new_p1' @ new_p2', new_c1 %:% new_c2

type only_apply = {keys:(string * size) list;
                   actions: (((string * size) list * cmd) list);
                   default: cmd}

let rec mk_one_big_table' (tbl : only_apply) c =
  match c with
  | Skip -> tbl
  | Assign _ ->
    { tbl with
      actions = List.map tbl.actions ~f:(fun (p, act) -> (p, act %:% c));
      default = tbl.default %:% c }
  | Assert _ | Assume _ -> failwith "Assert/Assume not handled"
  | Seq(c1, c2) -> mk_one_big_table' (mk_one_big_table' tbl c1) c2
  | While _ -> failwith "While not handled"
  | Select(_, tcl) ->
    let free = List.map tcl ~f:(fun (t, _) -> free_vars_of_test t)  |> List.concat in
    let es = List.map tcl ~f:snd in
    
    let tbl_keys = {tbl with keys = dedup (tbl.keys @ free)} in
    List.fold es ~init:tbl_keys ~f:mk_one_big_table'
  | Apply app_t ->
    let cross_actions = List.map
                          (cross tbl.actions app_t.actions |> List.concat)
                          ~f:(fun (x, y) -> combine_actions x y) in
    let def_tbl_to_app_t = List.map app_t.actions ~f:(combine_actions ([], tbl.default)) in
    let tbl_to_def_app_t = List.map tbl.actions ~f:(fun t -> combine_actions t ([], app_t.default)) in
    { keys = dedup (tbl.keys @ app_t.keys);
      actions = def_tbl_to_app_t @ tbl_to_def_app_t @ cross_actions;
      default = tbl.default %:% app_t.default }

let mk_one_big_table c =
  let app_t = mk_one_big_table' { keys = []; actions = []; default = Skip } c in
  Apply { name = "OneBigTable";
          keys = app_t.keys;
          actions = app_t.actions;
          default = app_t.default }

