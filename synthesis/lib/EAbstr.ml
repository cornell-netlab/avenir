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
      Printf.printf "\t%s->%s\n%!" key (Match.to_string data)
    )


let infer (cache : t) (e : Edit.t) =
  (* Printf.printf "Log edits\n\t%s\n%!" (Edit.to_string e); *)
  List.find_map cache
    ~f:(fun (log_edit, phys_edits) ->
      match similar log_edit e with
      | None -> None
      | Some (adata, subst) ->
         (* Printf.printf "[EABSTR] New Logical Edit is \n%!"; *)
         (* Log.print_edits [e]; *)
         (* Printf.printf "[EABSTR] Template is:\n%!"; *)
         (* print_template subst; *)
         (* Printf.printf "[EABSTR] Physical guess is\n%!"; *)
         List.fold phys_edits ~init:(Some [])
           ~f:(fun acc e -> match acc with
                            | None -> None
                            | Some acc ->
                               match sub_consts adata subst e with
                               | None -> None
                               | Some e' ->
                                  (* Log.print_edits [e']; *)
                                  acc @ [e'] |> Some))


let update (cache : t) (log : Edit.t) (physs : Edit.t list) : t =
  (* Printf.printf "Caching %s\n%!" (Edit.to_string log); *)
  if List.exists cache ~f:(fun (_,ps) ->  ps = physs)
  then cache
  else (log, physs) :: cache
