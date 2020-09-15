open Core
open Ast
open Tables
open Util

type t = (Edit.t * Edit.t list) list


let make () : t = []

let similar (eold : Edit.t) (enew : Edit.t) =
  (* Printf.printf "\tComparing: %s \n\t       to: %s\n%!" (Edit.to_string enew) (Edit.to_string eold); *)
  match eold, enew with
  | Add (ot, (oms, ods, oaid)), Add (nt, (nms, nds, naid))
       when ot = nt && oaid = naid (*&& ods = nds*) ->
     (* Printf.printf "\t table %s & action %d match \n%!"  ot oaid; *)
     let adata =
       List.fold2 ods nds ~init:(Some StringMap.empty)
         ~f:(fun acc od nd ->
           match acc with
           | None -> None
           | Some acc ->
              if od = nd then
                (* let () = Printf.printf "\t[DATA] %s identical, moving on\n%!" (string_of_value od) in *)
                Some acc
              else
                let od_s = string_of_value od in
                match StringMap.find acc od_s with
                | Some nd' -> if nd' = nd then Some acc else None
                | None -> StringMap.set acc ~key:od_s ~data:nd |> Some
         ) |> or_unequal_lengths_to_option |> Option.join
     in
     let matches = List.fold2 oms nms ~init:(Some StringMap.empty)
       ~f:(fun acc om nm ->
         match acc with
         | None -> None
         | Some acc ->
            if om = nm then
              (* let () = Printf.printf "\t[MTCH] %s identical, moving on\n%!" (Match.to_string om) in *)
              Some acc
            else
              (* if Match.is_wildcard om || Match.is_wildcard nm then
               *   None
               * else *)
                let om_s = Match.to_string om in
                begin match StringMap.find acc om_s with
                | Some nm' when nm' = nm -> Some acc
                | Some _ -> None
                | None ->
                   StringMap.set acc ~key:om_s ~data:nm |> Some
                end)
                   |> or_unequal_lengths_to_option |> Option.join
     in
     begin match matches with
     | None  ->(* Printf.printf "but... values are used differently\n%!";*)
        None
     | Some m -> (*Printf.printf "success!\n%!";*)
        Some (adata, m)
     end
  | _, _ -> (*Printf.printf "but... edits don't match\n%!";*)
     None


let sub_consts (adata : value StringMap.t option) (map : Match.t StringMap.t) (e : Edit.t) : Edit.t option =
  match e with
  | Del _ -> None
  | Add (table, (ms, ad, idx)) ->
     let ms' = List.fold ms ~init:(Some []) ~f:(fun acc m ->
                   match acc with
                   | None -> None
                   | Some acc ->
                      begin match StringMap.find map (Match.to_string m) with
                       | None -> acc @ [m] |> Some
                       | Some m' -> acc @ [m'] |> Some
                       end
                 ) in
     match ms' with
     | None -> None
     | Some ms' -> match adata with
                   | None -> Add (table, (ms', ad, idx)) |> Some
                   | Some dmap ->
                      let ad' =
                        List.map ad
                          ~f:(fun d ->
                            match StringMap.find dmap (string_of_value d) with
                            | None -> d
                            | Some d' -> d')
                      in
                      Add (table, (ms', ad', idx)) |> Some


let print_template subst =
  StringMap.iteri subst
    ~f:(fun ~key ~data ->
      Printf.eprintf "\t%s->%s\n%!" key (Match.to_string data)
    )

let sub_edit_list edits (adata,subst) =
  List.fold edits  ~init:(Some [])
    ~f:(fun acc e ->
      let open Option in
      acc >>= fun acc ->
      sub_consts adata subst e >>| fun e' ->
      acc @ [e'])

let similar_edit_list edits edits' =
  List.fold2_exn edits edits' ~init:(Some(Some StringMap.empty, StringMap.empty))
    ~f:(fun acc_opt e e' ->
      let open Option in
      acc_opt >>= fun (acc_data, acc_matches) ->
      similar e e' >>| fun (data, matches) ->
      liftO2 disjoint_union acc_data data,
      disjoint_union acc_matches matches
    )


let diff l r =
  StringMap.merge l r ~f:(fun ~key:_ -> function
      | `Both (l,r) when Stdlib.(l <> r) -> Some (l,r)
      | _ -> None
    )

let equivalences diffmap : StringSet.t list =
  StringMap.fold diffmap ~init:[]
    ~f:(fun ~key ~data eq_classes ->
      if List.exists eq_classes ~f:(Fn.flip StringSet.mem key) then
        eq_classes
      else
        StringMap.fold diffmap ~init:(StringSet.singleton key)
          ~f:(fun ~key:key_inner ~data:data_inner acc ->
            (* Printf.printf "Checking whether (%s,%s) = (%s,%s)%!\n"
             *   (string_of_value (fst data)) (string_of_value (snd data))
             *   (string_of_value (snd data_inner)) (string_of_value (snd data_inner)); *)
            if Stdlib.(data = data_inner) then
              (* let () = Printf.printf "\t it does, so concluding %s == %s \n%!" key_inner key in *)
              StringSet.add acc key_inner
            else
              (* let () = Printf.printf "\t it doesn't, concluding %s <> %s \n%!" key_inner key in *)
              acc
          )
        :: eq_classes
    )

let infer_fresh phys (curr_edits : Edit.t list) substs (old_edits : Edit.t list list) : Edit.t list option =
  let inferred_old_edits = List.filter_map old_edits ~f:(Fn.flip sub_edit_list substs) in
  let curr_edit_model = Edit.list_to_model phys curr_edits in
  let old_edit_maps = List.map inferred_old_edits ~f:(Edit.list_to_model phys) in
  List.find_map old_edit_maps
    ~f:(fun old_model ->
      if StringMap.equal Stdlib.(=) curr_edit_model old_model then None else
      let diff_map = diff curr_edit_model old_model in
      (* let () =
       *   List.iter (StringMap.keys curr_edit_model) ~f:(fun key ->
       *       let l_opt = StringMap.find curr_edit_model key in
       *       let r_opt = StringMap.find old_model key in
       *       let string_of_opt_value = Option.value_map ~f:string_of_value ~default:"??" in
       *       Printf.printf "%s |-->  %s <> %s\n%!" key
       *         (string_of_opt_value l_opt) (string_of_opt_value r_opt)
       *     )
       * in *)
      let eqs = equivalences diff_map |> List.filter ~f:(fun s -> StringSet.length s > 1) in
      (* if List.is_empty eqs then Printf.printf "Couldn't conclude any equivalences\n%!"; *)
      Option.some_if (not (List.is_empty eqs)) eqs
    )
  |> Option.map ~f:(fun eqs ->
         (*characteristic elements *)
         let chis = List.fold eqs ~init:StringMap.empty
                      ~f:(fun acc s -> StringMap.set acc ~key:(StringSet.choose_exn s) ~data:[]) in
         let prohibited : value list StringMap.t =
           List.fold old_edit_maps ~init:chis
             ~f:(fun chis edit_map ->
               StringMap.merge chis edit_map ~f:(fun ~key:_ -> function
                   | `Left l -> Some l
                   | `Right _ -> None
                   | `Both (l,r) -> Some (r::l)
                 )
             )
         in
         let valuation =
           StringMap.map prohibited ~f:(fun x ->
               let random_x = random_int_nin (List.map x ~f:(get_int_exn)) in
               mkInt(random_x, size_of_value (List.hd_exn x))
             )
         in
         (* Printf.printf "generating free vars : %s\n%!" (string_of_map valuation); *)
         let expanded_valuation =
           StringMap.fold valuation ~init:StringMap.empty
             ~f:(fun ~key ~data acc ->
               match List.find eqs ~f:(Fn.flip StringSet.mem key) with
               | None -> StringMap.set acc ~key ~data
               | Some eqs ->
           StringSet.fold eqs ~init:acc ~f:(fun acc key -> StringMap.set acc ~key ~data)
             )
         in
         StringMap.merge curr_edit_model expanded_valuation
           ~f:(fun ~key:_ -> function
             | `Left l -> Some l
             | `Right r -> Some r
             | `Both (_,r) -> Some r
           )
         |> Edit.extract phys
       )





let infer (cache : t) (phys : cmd) (e : Edit.t) =
  let open Option in
  let matching_cached_edits =
    List.filter_map cache
      ~f:(fun (loge, phys_edits) ->
        similar loge e >>| const phys_edits
      )
  in
  if List.length matching_cached_edits < 3 then
    None
  else
    (* let () = Printf.printf "Log edits\n\t%s\n%!" (Edit.to_string e) in *)
    List.find_map cache
      ~f:(fun (log_edit, phys_edits) ->
        let open Option in
        similar log_edit e >>= fun (adata,subst) ->
        sub_edit_list phys_edits (adata,subst)  >>= fun phys_edits' ->

        if true then
          match infer_fresh phys phys_edits' (adata, subst) matching_cached_edits with
          | Some edits ->
             (* Printf.printf "fresh inference succeeded\n%!"; *)
             (* Log.print_edits ~tab:true {Parameters.default with debug = true; thrift_mode = true} phys edits; *)
             return edits
          | None ->
             (* Printf.printf "fresh inference failed\n%!"; *)
             (* Log.print_edits ~tab:true {Parameters.default with debug = true; thrift_mode = true} phys phys_edits'; *)
             return phys_edits'
        else
          return phys_edits'
      )

let update (cache : t) (log : Edit.t) (physs : Edit.t list) : t =
  (* Printf.printf "Caching %s\n%!" (Edit.to_string log); *)
  if List.exists cache ~f:(fun (_,ps) ->  ps = physs)
  then cache
  else (log, physs) :: cache
