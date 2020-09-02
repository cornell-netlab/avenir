open Core
open Util
open Ast
open Tables

type t = {table:string;
          match_opt:Match.t option list option;
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
       List.fold ms ~init:"" ~f:(fun acc m_opt ->
           match m_opt with
           | None -> acc
           | Some m ->
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

let subset_list ks vs =
  let open List in
  for_all ks ~f:(fun k -> exists vs ~f:((=) k))

let get_poss_injections keys phys e : string list =
  match e with
  | Edit.Del _ -> []
  | Edit.Add (_, (matches, _,_)) ->
     get_tables_vars ~keys_only:true phys
     |> List.filter_map
          ~f:(fun (t,vars) ->
            let rel_keys = Match.relevant_keys matches in
            if List.is_empty rel_keys && keys = vars
               || subset_list rel_keys (fsts vars)
            then Some t
            else None)


let make edit table (log_keys, _, _) (phys_keys,_, _) =
  let log_keys = free_keys log_keys in
  let phys_keys = free_keys phys_keys in
  let match_opt =
    List.map phys_keys ~f:(fun (key,sz) ->
        match List.findi log_keys ~f:(fun _ (key',_) -> key = key') with
        | None ->
           let string = Printf.sprintf "0b%s" (String.make sz '0') in
           Some (Match.mask_ key (mkInt(0,sz)) (mkInt(int_of_string string, sz)))
        | Some (i,_) -> Edit.get_ith_match ~i edit)
    |> Some
  in
  { table;
    match_opt;
    act_id_opt = None;
    act_data_opt = None}

let construct log phys (e : Edit.t) : t list =
  match get_schema_of_table (Edit.table e) log with
  | None -> []
  | Some ((keys, _, _) as log_schema) ->
     get_poss_injections (free_keys keys) phys e
     |> List.fold ~init:[]
          ~f:(fun acc table ->
            match get_schema_of_table table phys with
            | None -> acc
            | Some phys_schema ->
               acc @ [make e table log_schema phys_schema])


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
       let _, act_params, _ = List.nth_exn actions (act_id_int) in
       StringMap.of_alist_exn @@  [
           which_act, Int(act_id, act_size);
           add_hole, mkInt(1,1);
         ] @ extract_action_data hint.table act_id_int act_params hint.act_data_opt
  in
  let matches =
    match hint.match_opt with
    | None -> StringMap.empty
    | Some ms ->
       List.filter_map ms ~f:(Option.map ~f:(Match.to_model hint.table))
       |> aggregate_models
  in
  aggregate_models [matches;action]




let list_to_model (phys : cmd) (hints : t list) : value StringMap.t =
  let hints = List.map hints ~f:(to_model phys) in
  (* Printf.printf "Print out the hints\n%!";
   * List.iter hints ~f:(fun h -> Printf.printf "%s\n%!" (string_of_map h)); *)
  aggregate_models hints
