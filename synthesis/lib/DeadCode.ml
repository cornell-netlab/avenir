open Core
open Util
open Ast


(* post-order traversal, eliminating variable assignments that are unused
 * best if run after constant propogation
 *)
let rec eliminate_unused_vars cmd (used : StringSet.t) =
  match cmd with
  | Skip -> (Skip,used)
  | Assign(f,e) ->
     if StringSet.mem used f then
       (f %<-% e, fsts (free_of_expr `Var e)
                  |> stringset_add_list (StringSet.remove used f)
       )
     else
       (Skip, used)
  | Assert t ->
     (Assert t, stringset_add_list used @@ List.map ~f:fst @@ free_of_test `Var t)
  | Assume t ->
     (Assume t, stringset_add_list used @@ List.map ~f:fst @@ free_of_test `Var t)
  | Seq (c1,c2) ->
     let c2', used2 = eliminate_unused_vars c2 used in
     let c1', used1 = eliminate_unused_vars c1 used2 in
     (c1' %:% c2', used1)
  | Select (typ, cases) ->
     let cases', used' =
       List.fold cases ~init:([],used)
         ~f:(fun (acc_cases,acc_used) (b,c) ->
           let b_fvs = free_of_test `Var b in
           let c',used' = eliminate_unused_vars c used in
           (acc_cases @ [(b,c')],
            stringset_add_list used' (fsts b_fvs)
            |> StringSet.union acc_used
           )
         ) in
     (mkSelect typ cases', used')
  | Apply {name;keys;actions;default} ->
     let default', def_used = eliminate_unused_vars default used in
     let actions', used' =
       List.fold actions ~init:([],def_used)
         ~f:(fun (acc_acts, acc_used) (data,act) ->
           let act', used_with_data = eliminate_unused_vars act used in
           let used_sans_data = strset_remove_list used_with_data (fsts data) in
           (acc_acts @ [data,act'], StringSet.union acc_used used_sans_data)
         )
     in
     if List.for_all actions' ~f:(fun (_,act) -> act = Skip) && default' = Skip
     then (Skip, used)
     else (Apply {name;
                  keys;
                  actions = actions';
                  default = default'},
           stringset_add_list used' (fsts @@ free_keys keys))
  | While _ ->
     failwith "[DeadCode] While is deprecated"

let elim_vars fvs cmd =
  StringSet.of_list @@ fsts fvs
  |> eliminate_unused_vars cmd
  |> fst
