open Core
open Util

(* post-order traversal, eliminating variable assignments that are unused
 * best if run after constant propogation
 *)
let rec eliminate_unused_vars cmd (used : StringSet.t) =
  let open Cmd in
  match cmd with
  | Skip -> (Skip, used)
  | Assign (f, e) ->
      if StringSet.mem used f then
        ( f %<-% e
        , fsts (Expr.frees `Var e)
          |> stringset_add_list (StringSet.remove used f) )
      else (Skip, used)
  | Assume t ->
      (Assume t, stringset_add_list used @@ List.map ~f:fst @@ Test.vars t)
  | Seq (c1, c2) ->
      let c2', used2 = eliminate_unused_vars c2 used in
      let c1', used1 = eliminate_unused_vars c1 used2 in
      (c1' %:% c2', used1)
  | Select (typ, cases) ->
      let cases', used' =
        List.fold cases ~init:([], used)
          ~f:(fun (acc_cases, acc_used) (b, c) ->
            let b_fvs = Test.vars b in
            let c', used' = eliminate_unused_vars c used in
            ( acc_cases @ [(b, c')]
            , stringset_add_list used' (fsts b_fvs)
              |> StringSet.union acc_used ) )
      in
      (select typ cases', used')
  | Apply {name; keys; actions; default} ->
      let default', def_used = eliminate_unused_vars default used in
      let actions', used' =
        List.fold actions ~init:([], def_used)
          ~f:(fun (acc_acts, acc_used) (n, data, act) ->
            let act', used_with_data = eliminate_unused_vars act used in
            let used_sans_data =
              strset_remove_list used_with_data (fsts data)
            in
            ( acc_acts @ [(n, data, act')]
            , StringSet.union acc_used used_sans_data ) )
      in
      if
        List.for_all actions' ~f:(fun (_, _, act) -> equals act Skip)
        && equals default' Skip
      then (Skip, used)
      else
        ( Apply {name; keys; actions= actions'; default= default'}
        , stringset_add_list used' (fsts @@ free_keys keys) )

let elim_vars fvs cmd =
  StringSet.of_list @@ fsts fvs |> eliminate_unused_vars cmd |> fst
