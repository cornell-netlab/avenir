open Core
open Util
open Tables
open Ast
open Manip

type t = Row.t list StringMap.t (* Keys are table names, Rows are table rows*)

let empty = StringMap.empty

let update (inst : t) (e : Edit.t) =
  match e with
  | Add (tbl, row) ->
     StringMap.update inst tbl
       ~f:(fun rows_opt ->
         match rows_opt with
         | None -> [row]
         | Some rows -> row::rows)
  | Del (tbl, i) ->
     StringMap.change inst tbl
       ~f:(function
         | None -> None
         | Some rows -> List.filteri rows ~f:(fun j _ -> i <> j) |> Some)

let rec update_list (inst : t) (edits : Edit.t list) =
  match edits with
  | [] -> inst
  | (e::es) -> update_list (update inst e) es


let rec overwrite (old_inst : t) (new_inst : t) : t =
  StringMap.fold new_inst ~init:old_inst
    ~f:(fun ~key ~data acc -> StringMap.set acc ~key ~data)


let size : t -> int =
  StringMap.fold ~init:0 ~f:(fun ~key:_ ~data -> (+) (List.length data))

let delete_hole i tbl = Hole(Printf.sprintf "?DeleteRow%dIn%s" i tbl, 1)
let add_row_hole tbl = Hole (Printf.sprintf "?AddRowTo%s" tbl, 1)
let which_act_hole tbl actSize =
  assert (actSize > 0);
  Hole (Printf.sprintf "?ActIn%s" tbl, actSize)


let tbl_hole encode_tag keys tbl row_hole act_hole i actSize (hs : Hint.t list) =
  let default_match_holes =
    List.fold ~init:True
      ~f:(fun acc (x,sz) ->
        acc %&% Match.holes encode_tag x sz) in
  let match_holes =
    match List.find hs ~f:(fun h -> Stdlib.(h.table = tbl)) with
    | Some h ->
       begin match Hint.encode_match keys h encode_tag with
       | None ->  default_match_holes keys
       | Some phi -> phi
       end
    | None ->
       default_match_holes keys
 in
  match_holes
  %&% (row_hole %=% mkVInt (1,1))
  %&% (act_hole %=% mkVInt (i,actSize))

let rec apply ?no_miss:(no_miss = false)
          (tag : [< `NoHoles | `OnlyHoles of Hint.t list | `WithHoles of (string * int) list * Hint.t list]) encode_tag ?cnt:(cnt=0)
          (inst : t)
          (prog : cmd)
        : (cmd * int) =
  match prog with
  | Skip
    | Assign _
    | Assert _
    | Assume _ -> (prog, cnt)
  | Seq (c1,c2) ->
     let (c1', cnt1) = apply ~no_miss tag encode_tag ~cnt inst c1 in
     let (c2', cnt2) = apply ~no_miss tag encode_tag ~cnt:cnt1 inst c2 in
     (c1' %:% c2', cnt2)
  | While _ -> failwith "while loops not supported"
  | Select (typ, ss) ->
     let (ss, ss_cnt) =
       List.fold ss ~init:([],cnt)
         ~f:(fun (acc, cnt) (t, c) ->
           let (c', cnt') = apply ~no_miss tag encode_tag ~cnt inst c in
           acc @ [(t,c')], cnt'
         ) in
     (mkSelect typ ss, ss_cnt)
  | Apply t ->
     let actSize = max (log2(List.length t.actions)) 1 in
     let row_hole = add_row_hole t.name in
     let act_hole = which_act_hole t.name actSize in
     let rows = StringMap.find_multi inst t.name in
     let selects =
       match tag with
       | `OnlyHoles _ -> []
       | _ ->
          List.foldi rows ~init:[]
            ~f:(fun i acc (matches, data, action) ->
              let prev_tst = False in
              let tst = Match.list_to_test t.keys matches
                        %&% match tag with
                            | `WithHoles (ds,_) ->
                               (* delete_hole i tbl %=% mkVInt(0,1) *)
                               if List.exists ds ~f:((=) (t.name, i))
                               then delete_hole i t.name %=% mkVInt(0,1)
                               else True
                            | _ -> True in
              if action >= List.length t.actions then
                acc
              else begin
                  let cond = tst %&% !%(prev_tst) in
                  (* if params.debug then Printf.printf "[%s] Adding %s\n%!" tbl (string_of_test cond); *)
                  (cond, (List.nth t.actions action
                          |> Option.value ~default:([], t.default)
                          |> bind_action_data data))
                  :: acc
                end)
     in
     let holes =
       match tag with
       | `NoHoles -> []
       | `WithHoles (_,hs) | `OnlyHoles hs ->
          List.mapi t.actions
            ~f:(fun i (params, act) ->
              (tbl_hole encode_tag t.keys t.name row_hole act_hole i actSize hs
               %&% List.fold selects ~init:True
                     ~f:(fun acc (cond, _) -> acc %&% !%(cond))
              , holify (List.map params ~f:fst) act))
     in
     let dflt_row =
       let cond = match tag with
         | `OnlyHoles _ ->
            if no_miss
            then False
            else List.foldi rows ~init:(True)
                   ~f:(fun i acc (ms,_,act) ->
                     acc %&%
                       if act >= List.length t.actions then True else
                         !%(Match.list_to_test t.keys ms
                            %&% match tag with
                                | `WithHoles (ds,_) when List.exists ds ~f:((=) (t.name, i))
                                  -> delete_hole i t.name %=% mkVInt(0,1)
                                | _ -> True))
         | _ -> True
       in
       [(cond, t.default)]
     in
     let mk_select = match tag with
       | `OnlyHoles _ -> mkPartial
       | _ -> mkOrdered
     in
     let tbl_select = selects @ holes @ dflt_row |> mk_select in
     (* Printf.printf "TABLE %s: \n %s\n%!" tbl (string_of_cmd tbl_select); *)
     (tbl_select, cnt)



let update_consistently checker (params:Parameters.t) match_model (phys : cmd) (tbl_name : string) (act_data : Row.action_data option) (act : int) (acc : [`Ok of t | `Conflict of t]) : [`Ok of t | `Conflict of t] =
  let (keys,_,_) = get_schema_of_table tbl_name phys |> Option.value_exn in
  match acc with
  | `Ok pinst -> begin match StringMap.find pinst tbl_name,
                             Row.mk_new_row match_model phys tbl_name act_data act with
                 | _, None -> acc
                 | None,Some row ->
                    if params.interactive then
                      Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string row);
                    `Ok (StringMap.set pinst ~key:tbl_name ~data:[row])
                 | Some rows, Some (ks, data,act) ->
                    if params.interactive then
                      Printf.printf "+%s : %s" tbl_name (Row.to_string (ks,data,act));
                    begin match Row.remove_conflicts checker params tbl_name keys ks rows with
                    | None ->
                       `Ok (StringMap.set pinst ~key:tbl_name
                              ~data:((ks,data,act)::rows))
                    | Some rows' ->
                       `Conflict (StringMap.set pinst ~key:tbl_name
                                    ~data:((ks,data,act)::rows'))
                    end
                 end
  | `Conflict pinst ->
     begin match StringMap.find pinst tbl_name,
                 Row.mk_new_row match_model phys tbl_name (act_data) act with
     | _, None -> acc
     | None, Some row ->
        if params.interactive then
          Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string row);
        `Conflict (StringMap.set pinst ~key:tbl_name ~data:[row])
     | Some rows, Some (ks, data, act) ->
        if params.interactive then
          Printf.printf "+%s : %s\n%!" tbl_name (Row.to_string (ks,data,act));
        begin match Row.remove_conflicts checker params tbl_name keys ks rows with
        | None -> `Conflict (StringMap.set pinst ~key:tbl_name
                               ~data:((ks,data,act)::rows))
        | Some rows' ->
           `Conflict (StringMap.set pinst ~key:tbl_name
                        ~data:((ks,data,act)::rows'))
        end
     end

let remove_deleted_rows (params : Parameters.t) match_model (pinst : t) : t =
  StringMap.fold pinst ~init:empty ~f:(fun ~key:tbl_name ~data acc ->
      StringMap.set acc ~key:tbl_name
        ~data:(
          List.filteri data ~f:(fun i _ ->
              match delete_hole i tbl_name with
              | Hole(s,_) ->
                 begin match StringMap.find match_model s with
                 | None -> true
                 | Some do_delete when get_int do_delete = Bigint.one ->
                    if params.interactive then Printf.printf "- %s : row %d\n%!" tbl_name i;
                    false
                 | Some x -> true
                 end
              | _ -> true
            )
        )

    )

let fixup_edit checker (params : Parameters.t) (data : ProfData.t ref) match_model (action_map : (Row.action_data * size) StringMap.t option) (phys : cmd) (pinst : t) : [`Ok of t | `Conflict of t] =
  let st = Time.now() in
  match action_map with
  | Some m -> StringMap.fold ~init:(`Ok pinst) m ~f:(fun ~key:tbl_name ~data:(act_data,act) ->
                  update_consistently checker params match_model phys tbl_name (Some act_data) act)
  | None ->
     let tables_added_to =
       StringMap.fold match_model ~init:[]
         ~f:(fun ~key ~data acc ->
           if String.is_substring key ~substring:"AddRowTo"
              && data = Int(Bigint.one,1)
           then (String.substr_replace_all key ~pattern:"?" ~with_:""
                 |> String.substr_replace_first ~pattern:"AddRowTo" ~with_:"")
                :: acc
           else acc
         ) in
     let pinst' = remove_deleted_rows params match_model pinst in
     let out = List.fold tables_added_to ~init:(`Ok pinst')
                 ~f:(fun inst tbl_name ->
                   let str = ("?ActIn" ^ tbl_name) in
                   match StringMap.find match_model ("?ActIn" ^ tbl_name) with
                   | None ->
                      Printf.sprintf "Couldn't Find var %s\n" str |> failwith
                   | Some v ->
                      let act = get_int v |> Bigint.to_int_exn in
                      update_consistently checker params match_model phys tbl_name None act inst )
     in
     ProfData.update_time !data.fixup_time st;
     out