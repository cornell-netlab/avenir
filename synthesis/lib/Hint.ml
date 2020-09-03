open Core
open Util
open Ast
open Tables

type t = {table:string;
          match_opt:Match.t list option;
          act_id_opt:value option; (* unused *)
          act_data_opt:Row.action_data option} (* unused *)

(* let well_formed (hint : t) : bool =
 *   match hint.match_opt, hint.act_id_opt, hint.act_data_opt with
 *   | None  , None  , None   -> false (\*No point in an empty hint*\)
 *   | Some _, None  , None   -> true  (\*An key-only hint is just fine!*\)
 *   | _     , Some _, _      -> true  (\*Action Id without data is okay*\)
 *   | _     , None  , Some _ -> false (\*Action Data without id is bad*\) *)

let to_string (h : t) =
  let matches =
    match h.match_opt with
    | None -> ""
    | Some ms ->
       let first = ref true in
       List.fold ms ~init:"" ~f:(fun acc m ->
              let sep = if !first then "" else ";" in
              first := false;
              Printf.sprintf "%s%s%s" acc sep (Match.to_string m))
  in
  let (acts, act_id) = match h.act_id_opt with
    | None -> ("","")
    | Some a ->
       (Printf.sprintf "%s" (string_of_value a),
        match h.act_data_opt with
        | None -> ""
        | Some data -> Printf.sprintf "(%s)" (Row.action_data_to_string data))
  in
  Printf.sprintf "%s [%s] %s%s" h.table matches acts act_id


(*compute a list of tables and the vars that influence the choice of action in the table *)
(* v_influence(var) indicates the variables that influence the value of var *)
(* v_influence is accumulated in the first return position *)
(* the second return positon maps table names to the vars that influence the choice of action in the table *)
(* let rec influencing_tables phys (v_influence : StringSet.t StringMap.t) : (StringSet.t StringMap.t * StringSet.t StringMap.t) =
 *   let disjoint_union =
 *     StringMap.merge ~f:(fun ~key -> function
 *         | `Left l -> Some l
 *         | `Right r -> Some r
 *         | `Both _ ->
 *            failwith @@ Printf.sprintf "table %s occurs twice" key)
 *   in
 *   let union =
 *     StringMap.merge ~f:(fun ~key:_ -> function
 *         | `Left l -> Some l
 *         | `Right r -> Some r
 *         | `Both (l,r) -> Some (StringSet.union l r))
 *   in
 *   match phys with
 *   | Skip -> StringMap.(empty, empty)
 *   | Assign (f,e) ->
 *      let updatef data = StringMap.set v_influence ~key:f ~data in
 *      let new_influence = StringSet.of_list @@ fsts @@ free_of_expr `Var e in
 *      begin match StringMap.find v_influence f with
 *      | None -> (updatef new_influence, StringMap.empty)
 *      | Some old_influence ->
 *         let open StringSet in
 *         (remove old_influence f
 *          |> union new_influence
 *          |> updatef
 *         , StringMap.empty)
 *
 *      end
 *   | Assume _ -> StringMap.(empty,empty)
 *   | Seq (c1,c2) ->
 *      let vinf1,tables1 = influencing_tables c1 v_influence in
 *      let vinf2,tables2 = influencing_tables c2 vinf1 in
 *      (vinf2, disjoint_union tables1 tables2 )
 *   | Select (_, cs) ->
 *      List.fold cs ~init:StringMap.(empty,empty) ~f:(fun (vinf_acc, tables_acc) (_,c) ->
 *          let vinf,tables = influencing_tables c v_influence in
 *          (union vinf_acc vinf, disjoint_union tables tables_acc)
 *        )
 *   | Apply t ->
 *      let update tables new_data =
 *        StringMap.update tables t.name ~f:(function
 *            | None -> new_data
 *            | Some old_data -> StringSet.union new_data old_data)
 *      in
 *      let tables =
 *        List.fold (free_keys t.keys)
 *          ~init:(update StringMap.empty StringSet.empty)
 *          ~f:(fun acc (key,_) ->
 *            StringMap.find v_influence key
 *            |> Option.value ~default:(StringSet.singleton key)
 *            |> update acc
 *          )
 *      in
 *      let (v_inf,acts_tables) =
 *        List.fold t.actions
 *          ~init:(influencing_tables t.default v_influence)
 *          ~f:(fun (vinf_acc, tables_acc) (params,act) ->
 *            let (vinf, tables) = influencing_tables act v_influence in
 *            let params_set = StringSet.of_list (fsts params) in
 *            let remove_params = StringMap.map ~f:(fun influences -> StringSet.diff influences params_set) in
 *            let vinf_no_params = remove_params vinf in
 *            let tables_no_params = remove_params tables in
 *            (union vinf_acc vinf_no_params, disjoint_union tables_acc tables_no_params)
 *          )
 *      in
 *      (v_inf, disjoint_union tables acts_tables) *)

let get_poss_injections phys e : string list =
  let rel_keys =
    Edit.get_matches_exn e
    |> Match.relevant_keys
    |> StringSet.of_list
  in
  get_tables_vars ~keys_only:true phys
  |> List.filter_map
       ~f:(fun (t,vars) ->
         if StringSet.is_subset rel_keys ~of_:(fsts vars |> StringSet.of_list)
         then Some t
         else None)

let make edit table =
  let matches =
    Edit.get_matches_exn edit
    |> Match.relevant_matches
  in
  Printf.printf "for edit %s\n%!" (Edit.to_string edit);
  Printf.printf "the relevant matches are: %s\n%!" (Match.list_to_string matches);
  { table;
    match_opt = Some matches;
    act_id_opt = None;
    act_data_opt = None}

let construct phys (e : Edit.t) : t list =
  get_poss_injections phys e
  |> List.fold ~init:[]
       ~f:(fun acc table -> acc @ [make e table])


let extract_action_data table act_id params (data_opt : Row.action_data option) : (string * value) list =
  match data_opt with
  | None -> []
  | Some data ->
     List.fold2_exn params data ~init:[]
       ~f:(fun acc (param,sz) value  ->
         acc @ [Hole.action_data table act_id param sz, value]
       )

let aggregate_models =
  List.fold ~init:StringMap.empty
    ~f:(fun acc m ->
      StringMap.merge acc m ~f:(fun ~key -> function
          | `Left l -> Some l
          | `Right r -> Some r
          | `Both (l,r) when veq l r -> Some l
          | `Both (l,r) ->
             failwith @@
               Printf.sprintf "[Hint.aggregate_models] conflicting values %s is both %s and %s"
                 key (string_of_value l) (string_of_value r)))


let join_models m1 m2 = aggregate_models [m1;m2]

let to_model (phys : cmd) (hint : t): value StringMap.t =
  let (_, actions, _) = get_schema_of_table hint.table phys
                        |> Option.value_exn
                             ~message:(Printf.sprintf "couldn't find table %s in %s" hint.table (string_of_cmd phys))
  in
  let action =
    let add_hole = Hole.add_row_hole_name hint.table in
    match hint.act_id_opt with
    | None -> StringMap.empty
    | Some Int(act_id, act_size) ->
       let act_id_int = Bigint.to_int_exn act_id in
       let which_act = Hole.which_act_hole_name hint.table in
       let act_params, _ = List.nth_exn actions (act_id_int) in
       StringMap.of_alist_exn @@  [
           which_act, Int(act_id, act_size);
           add_hole, mkInt(1,1);
         ] @ extract_action_data hint.table act_id_int act_params hint.act_data_opt
  in
  let matches =
    match hint.match_opt with
    | None -> StringMap.empty
    | Some ms ->
       List.map ms ~f:(Match.to_model hint.table)
       |> aggregate_models
  in
  aggregate_models [matches;action]

let list_to_model (phys : cmd) (hints : t list) : value StringMap.t =
  let hints = List.map hints ~f:(to_model phys) in
  aggregate_models hints
