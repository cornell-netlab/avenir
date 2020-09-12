open Core
open Ast
open Util
open Tables


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
       List.map actions ~f:(fun (n, vars,a) ->
           let (fvs',a') = static_slice_aux fvs a in
             (n, a',vars, List.filter fvs' ~f:(fun (s,_) -> not (List.exists vars ~f:(fun (s',_)-> s = s'))))
         )
     in
     let def_fvs, def' = static_slice_aux fvs default in
     if List.exists tfx ~f:(fun (_, c,_,_) -> c <> Skip) || (def' <> Skip) then
       let fvs = List.fold tfx ~init:[] ~f:(fun acc (_,_,_,fvs) -> acc @ fvs )
                 @ free_keys keys
                 @ def_fvs
                 |> List.dedup_and_sort ~compare:(fun (s,_) (s',_) -> String.compare s s') in
       let actions' = List.map tfx ~f:(fun (n, a,vars,_) -> (n, vars,a)) in
       (fvs, Apply {name; keys; actions = actions'; default = def'})
     else
       (fvs, Skip)


let static_slice fvs cmd = snd @@ static_slice_aux fvs cmd



let could_hit facts (matches,_,_) =
  List.for_all matches
    ~f:(fun m ->
      match StringMap.find facts (Match.get_key m) with
      | None -> true
      | Some possible_values ->
         List.exists possible_values ~f:((&&) (Fn.non Match.is_wildcard m) %. Match.hits m)
    )


let size_of_facts =
  StringMap.fold ~init:0
    ~f:(fun ~key:_ ~data acc ->
      1 + List.length data + acc
    )


let edit_slice_table params (name,keys,actions,default) (facts : value list StringMap.t) (inst : Instance.t) (edits : Edit.t list) =
  let eliminable = List.find (fsts3 keys)
                     ~f:(fun k -> StringMap.find facts k
                                  |> Option.value_map ~f:(Fn.non List.is_empty) ~default:false) in
  let edits_to_add = List.filter edits ~f:((=) name %. Edit.table) in
  let sliced_inst =
    if Option.is_none eliminable then
      let () = Printf.printf "We can eliminate extant rows in table %s\n%!" name in
      Instance.of_edits params edits_to_add
    else
      let () = Printf.printf "we know about %s, so we can't eliminate extant rows in %s\n%!" (Option.value_exn eliminable) (name) in
      let extant_rows = Instance.get_rows (Instance.update_list params inst edits) name in
      let relevant_extant_rows = List.filter extant_rows ~f:(could_hit facts)  in
      let relevant_extant_inst = StringMap.(set empty ~key:name ~data:relevant_extant_rows) in
      Instance.update_list params relevant_extant_inst edits_to_add
  in
  (* if params.debug then Printf.printf "New %s Instance is:%s\n%!" name (Instance.to_string sliced_inst); *)
  let table =  Apply {name;keys;actions;default} in
  (* if params.debug then Printf.printf "Applying it to table %s\n%!" (string_of_cmd table); *)
  let table = Instance.verify_apply ~no_miss:true params sliced_inst table in
  let facts' = ConstantProp.propogate_choices facts table in
  (* if params.debug then Printf.printf "slicing %s to \n%s\n%!" (name) (string_of_cmd table); *)
  table, facts'

let project_to_exprfacts (multifacts : value list StringMap.t) : expr StringMap.t =
  StringMap.filter_map multifacts ~f:(function
      | [v] -> Some(Value v)
      | _ -> None)

let rec edit_slice_aux (params : Parameters.t) facts inst edits cmd =
  if params.debug then Printf.printf "Slice loop with %d facts across %d keys\n%!" (size_of_facts facts) (StringMap.keys facts |> List.length);
  match cmd with
  | Skip | Assume _ -> cmd, facts
  | Assign _ ->
     if params.debug then Printf.printf "ConstantProp\n%!";
     cmd, ConstantProp.propogate_choices facts cmd
  | Seq (c1,c2) ->
     if params.debug then Printf.printf "Seq\n%!";
     let c1', facts1 = edit_slice_aux params facts  inst edits c1 in
     let c2', facts2 = edit_slice_aux params facts1 inst edits c2 in
     if params.debug then Printf.printf "Normalizing seq\n%!";
     let c' = c1' %:% c2' in
     if params.debug then Printf.printf "---done with seq\n%!";
     c',facts2
  | Select (typ, cs) ->
     if params.debug then Printf.printf "select\n%!";
     let t = Time.now() in
     let cs, facts' =
       List.fold cs ~init:([], StringMap.empty)
         ~f:(fun (acc_cs, acc_facts) (b,c) ->
           let c', facts = edit_slice_aux params facts inst edits c in
           let b' = ConstantProp.propogate_test (project_to_exprfacts facts) b in
           acc_cs@[b',c'], multimap_union acc_facts facts
         )
     in
     let c' = mkSelect typ cs in
     if params.debug then Printf.printf "Select processing took %f seconds\n%!" (Time.(Span.(diff (now()) t |> to_ms)));
     c', facts'
  | Apply t ->
     if params.debug then Printf.printf "Edit Slice table %s\n%!" t.name;
     edit_slice_table params (t.name,t.keys,t.actions, t.default) facts inst edits


let edit_slice params inst edits cmd =
  edit_slice_aux params StringMap.empty inst edits cmd
  |> fst
