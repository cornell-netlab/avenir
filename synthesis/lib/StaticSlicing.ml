open Core
open Ast



let rec static_slice_aux (fvs : (string * int) list) (c : cmd) : (string*int) list * cmd  =
  match c with
  | Skip -> (fvs, Skip)
  | Assign (f,e) ->
     begin match List.find fvs ~f:(fun (s,_) -> f = s) with
     | None -> (fvs,Skip)
     | Some (s,_) ->
        List.filter fvs ~f:(fun (s',_) -> s <> s')
        @ free_of_expr `Var e @ free_of_expr `Hole e
        |> List.dedup_and_sort ~compare:(fun (s,_) (s',_) -> String.compare s s')
       , c
     end
  | Assume t ->
     fvs
     @ free_of_test `Var t
     @ free_of_test `Hole t
     |> List.dedup_and_sort ~compare:(fun (s,_) (s',_) -> String.compare s s')
    , c
  | Seq (c1, c2) ->
     let (fvs2', c2') = static_slice_aux fvs c2 in
     let (fvs1', c1') = static_slice_aux fvs2' c1 in
     fvs1', c1' %:% c2'
  | Select (styp, ss) ->
     let tfx =
       List.map ss ~f:(fun (b,c) ->
           let (fvs',c') = static_slice_aux fvs c in
             (b,c',fvs')
         ) in
     if List.exists tfx ~f:(fun (_,c,_) -> c <> Skip) then
       let (fvs', ss') = List.fold tfx ~init:([],[])
         ~f:(fun (fvs_acc,ss_acc) (b,c, fvs) ->
           (fvs_acc @ fvs @ free_of_test `Var b @ free_of_test `Hole b,
            ss_acc @ [(b,c)])
         ) in
       (List.dedup_and_sort fvs' ~compare:(fun (s,_) (s',_) -> String.compare s s')
       , mkSelect styp ss')
     else
       (fvs, Skip)
  | Apply { name; keys; actions; default;_ } ->

     let tfx =
       List.map actions ~f:(fun (vars,a) ->
           let (fvs',a') = static_slice_aux fvs a in
             (a',vars, List.filter fvs' ~f:(fun (s,_) -> not (List.exists vars ~f:(fun (s',_)-> s = s'))))
         )
     in
     let def_fvs, def' = static_slice_aux fvs default in
     if List.exists tfx ~f:(fun (c,_,_) -> c <> Skip) || (def' <> Skip) then
       let fvs = List.fold tfx ~init:[] ~f:(fun acc (_,_,fvs) -> acc @ fvs )
                 @ free_keys keys
                 @ def_fvs
                 |> List.dedup_and_sort ~compare:(fun (s,_) (s',_) -> String.compare s s') in
       let actions' = List.map tfx ~f:(fun (a,vars,_) -> (vars,a)) in
       (fvs, Apply {name; keys; actions = actions'; default = def'})
     else
       (fvs, Skip)
  | Assert _
  | While _ -> failwith "unsupported"


let static_slice fvs cmd = snd @@ static_slice_aux fvs cmd
