open Core
open Ast
open Tables
open Util

type t = {table:string;
          match_opt:Match.t option list option;
          act_id_opt:int option;
          act_data_opt:Row.action_data option}

let well_formed (hint : t) : bool =
  match hint.match_opt, hint.act_id_opt, hint.act_data_opt with
  | None  , None  , None   -> false (*No point in an empty hint*)
  | Some _, None  , None   -> true  (*An action-only hint is just fine too!*)
  | _     , Some _, _      -> true  (*Action Id without data is okay*)
  | _     , None  , Some _ -> false (*Action Data without id is bad*)


let get_poss_injections keys phys e : string list =
  get_tables_vars ~keys_only:true phys
  |> List.filter_map
       ~f:(fun (t,vars) -> if nonempty_inter keys vars then Some t else None)


let make edit table (log_keys, log_acts, log_def) (phys_keys, phys_acts, phys_def) =
  let match_opt =
    List.map phys_keys ~f:(fun (key,sz) ->
        match List.findi log_keys ~f:(fun i (key',_) -> key = key') with
        | None -> None
        | Some (i,_) -> Edit.get_ith_match ~i edit
      ) |> Some
  in
  { table;
    match_opt;
    act_id_opt = None;
    act_data_opt = None}

let construct log phys (e : Edit.t) : t list =
  match get_schema_of_table (Edit.table e) log with
  | None -> []
  | Some ((keys, actions, default) as log_schema) ->
     get_poss_injections keys phys e
     |> List.fold ~init:[]
          ~f:(fun acc table ->
            match get_schema_of_table table phys with
            | None -> acc
            | Some phys_schema ->
               acc @ [make e table log_schema phys_schema]
          )

let encode_match keys h encode_tag =
  match h.match_opt with
  | None -> None
  | Some matches ->
     List.fold2 keys matches ~init:True
       ~f:(fun acc (x,sz) match_opt ->
         match match_opt with
         | None -> Match.holes encode_tag x sz
         | Some m ->
            Match.to_test (x,sz) m
       )
  |> or_unequal_lengths_to_option


let added_to_row (table : string) (m : value StringMap.t)  : bool =
  match "?AddRowTo" ^ table |> StringMap.find m with
  | Some data -> get_int data = Bigint.one
  | _ -> false


let add_to_model (phys : cmd) (hints : t list) (m : value StringMap.t) : value StringMap.t =
  List.fold hints ~init:m
    ~f:(fun acc hint ->
      if added_to_row hint.table acc
      then
        match hint.match_opt with
        | None -> acc
        | Some matches ->
           match get_schema_of_table hint.table phys with
           | None -> acc
           | Some (keys,_,_) ->
              List.fold2_exn keys matches ~init:acc ~f:(fun acc (ky,_) mtch ->
                  match mtch with
                  | None -> acc
                  | Some (Match.Exact data) -> StringMap.set acc ~key:("?"^ky) ~data
                  | Some (Match.Between (lo, hi)) ->
                     StringMap.set acc ~key:("?"^ky^"_lo") ~data:lo
                     |> StringMap.set ~key:("?"^ky^"_hi") ~data:hi
                  | Some (Match.Mask (v, m)) ->
                     StringMap.set acc ~key:("?"^ky) ~data:v
                     |> StringMap.set ~key:("?"^ky^"_mask") ~data:m
                )
      else
        acc)
