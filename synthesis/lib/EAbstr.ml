open Core
open Util

type t = (Edit.t * Edit.t list) list [@@deriving yojson]

let make ?(filename="") () : t =
  if (String.length filename) > 0 then
    match of_yojson (Yojson.Safe.from_file filename) with
    | Ok cache -> cache
    | Error _ -> failwith (Printf.sprintf "Could not read cache from file %s" filename)
  else []

let equal (c : t) (c' : t) : bool =
  let entry_eq entry entry' =
    match entry, entry' with
    | (edit, edits), (edit', edits') when (Edit.equal edit edit') -> List.equal Edit.equal edits edits'
    | _ -> false in
  List.equal entry_eq c c'

let string_of_cache (c : t) : string =
  let string_of_entry (e : Edit.t * Edit.t list) : string =
    let (x, y) = e in
    let init1 = (Edit.to_string x) ^ ":" in
    let fun1 = (fun a b -> a ^ "|" ^ (Edit.to_string b)) in
    List.fold y ~init: init1 ~f:fun1 in
  (List.map ~f:string_of_entry c) |> (String.concat ~sep:",")

let dump_yojson (cache : t) (filename : string) : unit =
  cache |> to_yojson |> Yojson.Safe.to_file filename

let similar (eold : Edit.t) (enew : Edit.t) =
  match (eold, enew) with
  | Add (ot, (oms, ods, oaid)), Add (nt, (nms, nds, naid))
    when String.(ot = nt) && oaid = naid (*&& ods = nds*) -> (
      let adata =
        List.fold2 ods nds ~init:(Some StringMap.empty) ~f:(fun acc od nd ->
            match acc with
            | None -> None
            | Some acc -> (
                if Value.equals od nd then Some acc
                else
                  let od_s = Value.to_string od in
                  match StringMap.find acc od_s with
                  | Some nd' ->
                      if Value.equals nd' nd then Some acc else None
                  | None -> StringMap.set acc ~key:od_s ~data:nd |> Some ))
        |> or_unequal_lengths_to_option |> Option.join
      in
      let matches =
        List.fold2 oms nms ~init:(Some StringMap.empty) ~f:(fun acc om nm ->
            match acc with
            | None -> None
            | Some acc -> (
                if Match.equal om nm then Some acc
                else
                  let om_s = Match.to_string om in
                  match StringMap.find acc om_s with
                  | Some nm' when Match.equal nm' nm -> Some acc
                  | Some _ -> None
                  | None -> StringMap.set acc ~key:om_s ~data:nm |> Some ))
        |> or_unequal_lengths_to_option |> Option.join
      in
      match matches with None -> None | Some m -> Some (adata, m) )
  | _, _ -> None

let sub_consts (adata : Value.t StringMap.t option)
    (map : Match.t StringMap.t) (e : Edit.t) : Edit.t option =
  match e with
  | Del _ -> None
  | Add (table, (ms, ad, idx)) -> (
      let ms' =
        List.fold ms ~init:(Some []) ~f:(fun acc m ->
            match acc with
            | None -> None
            | Some acc -> (
              match StringMap.find map (Match.to_string m) with
              | None -> acc @ [m] |> Some
              | Some m' -> acc @ [m'] |> Some ))
      in
      match ms' with
      | None -> None
      | Some ms' -> (
        match adata with
        | None -> Add (table, (ms', ad, idx)) |> Some
        | Some dmap ->
            let ad' =
              List.map ad ~f:(fun d ->
                  match StringMap.find dmap (Value.to_string d) with
                  | None -> d
                  | Some d' -> d')
            in
            Add (table, (ms', ad', idx)) |> Some ) )

let print_template subst =
  StringMap.iteri subst ~f:(fun ~key ~data ->
      Printf.eprintf "\t%s->%s\n%!" key (Match.to_string data))

let sub_edit_list edits (adata, subst) =
  List.fold edits ~init:(Some []) ~f:(fun acc e ->
      let open Option in
      acc >>= fun acc -> sub_consts adata subst e >>| fun e' -> acc @ [e'])

let similar_edit_list edits edits' =
  List.fold2_exn edits edits'
    ~init:(Some (Some StringMap.empty, StringMap.empty))
    ~f:(fun acc_opt e e' ->
      let open Option in
      acc_opt
      >>= fun (acc_data, acc_matches) ->
      similar e e'
      >>| fun (data, matches) ->
      ( liftO2 disjoint_union acc_data data
      , disjoint_union acc_matches matches ))

let equivalences diffmap : StringSet.t list =
  StringMap.fold diffmap ~init:[] ~f:(fun ~key ~data eq_classes ->
      if List.exists eq_classes ~f:(Fn.flip StringSet.mem key) then
        eq_classes
      else
        StringMap.fold diffmap ~init:(StringSet.singleton key)
          ~f:(fun ~key:key_inner ~data:data_inner acc ->
            if Stdlib.(data = data_inner) then StringSet.add acc key_inner
            else acc)
        :: eq_classes)

let infer_fresh phys (curr_edits : Edit.t list) substs
    (old_edits : Edit.t list list) : Edit.t list option =
  let inferred_old_edits =
    List.filter_map old_edits ~f:(Fn.flip sub_edit_list substs)
  in
  let curr_edit_model = Edit.list_to_model phys curr_edits in
  let old_edit_maps =
    List.map inferred_old_edits ~f:(Edit.list_to_model phys)
  in
  List.find_map old_edit_maps ~f:(fun old_model ->
      if Model.equal curr_edit_model old_model then None
      else
        let diff_map = Model.diff curr_edit_model old_model in
        let eqs =
          equivalences diff_map
          |> List.filter ~f:(fun s -> StringSet.length s > 1)
        in
        Option.some_if (not (List.is_empty eqs)) eqs)
  |> Option.map ~f:(fun eqs ->
         (*characteristic elements *)
         let chis =
           List.fold eqs ~init:StringMap.empty ~f:(fun acc s ->
               StringMap.set acc ~key:(StringSet.choose_exn s) ~data:[])
         in
         let prohibited : Value.t list StringMap.t =
           List.fold old_edit_maps ~init:chis ~f:Model.extend_multi_model
         in
         let valuation =
           StringMap.fold prohibited ~init:Model.empty
             ~f:(fun ~key ~data:prohibs m ->
               let random_x =
                 random_int_nin (List.map prohibs ~f:Value.get_int_exn)
               in
               Model.set m ~key
                 ~data:
                   (Value.make (random_x, Value.size (List.hd_exn prohibs))))
         in
         let expanded_valuation =
           Model.fold valuation ~init:Model.empty ~f:(fun ~key ~data acc ->
               match List.find eqs ~f:(Fn.flip StringSet.mem key) with
               | None -> Model.set acc ~key ~data
               | Some eqs ->
                   StringSet.fold eqs ~init:acc ~f:(fun acc key ->
                       Model.set acc ~key ~data))
         in
         Model.merge curr_edit_model expanded_valuation ~f:(fun ~key:_ ->
           function
           | `Left l -> Some l | `Right r -> Some r | `Both (_, r) -> Some r)
         |> Edit.of_model phys)

let infer (params : Parameters.t) (cache : t) (phys : Cmd.t) (e : Edit.t) =
  let open Option in
  let matching_cached_edits =
    List.filter_map cache ~f:(fun (loge, phys_edits) ->
        similar loge e >>| const phys_edits)
  in
  if List.length matching_cached_edits < Option.value_exn params.ecache then
    None
  else
    List.find_map cache ~f:(fun (log_edit, phys_edits) ->
        let open Option in
        similar log_edit e
        >>= fun (adata, subst) ->
        sub_edit_list phys_edits (adata, subst)
        >>= fun phys_edits' ->
        if true then
          match
            infer_fresh phys phys_edits' (adata, subst) matching_cached_edits
          with
          | Some edits -> return edits
          | None -> return phys_edits'
        else return phys_edits')

let update (cache : t) (log : Edit.t) (physs : Edit.t list) : t =
  if List.exists cache ~f:(fun (_, ps) -> Edit.equal ps physs) then cache
  else (log, physs) :: cache
