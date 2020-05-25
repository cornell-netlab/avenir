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
  | Some _, None  , None   -> true  (*An key-only hint is just fine!*)
  | _     , Some _, _      -> true  (*Action Id without data is okay*)
  | _     , None  , Some _ -> false (*Action Data without id is bad*)



let subset_list ks vs =
  let open List in
  for_all ks ~f:(fun k -> exists vs ~f:((=) k))

let get_poss_injections keys phys e : string list =
  match e with
  | Edit.Del _ -> []
  | Edit.Add (_, (matches, _,_)) ->
     let relevant_keys =
       List.fold2_exn keys matches ~init:[]
         ~f:(fun acc k m ->
           match m with
           | Exact _ -> acc @ [k]
           | Mask (v,m) ->
              if Bigint.(get_int m = zero)
              then acc
              else acc @ [k]
           | Between (Int(lo,sz),Int(hi,_)) ->
              if Bigint.(lo = zero) && Bigint.(hi = pow (of_int 2) (of_int sz) - one)
              then acc
              else acc @ [k]
         )
     in
     get_tables_vars ~keys_only:true phys
     |> List.filter_map
          ~f:(fun (t,vars) ->
            if List.is_empty relevant_keys && keys = vars
               || subset_list relevant_keys vars
            then Some t
            else None)


let make edit table (log_keys, log_acts, log_def) (phys_keys, phys_acts, phys_def) =
  let match_opt =
    List.map phys_keys ~f:(fun (key,sz) ->
        match List.findi log_keys ~f:(fun i (key',_) -> key = key') with
        | None ->
           let string = Printf.sprintf "0b%s" (String.make sz '0') in
           Some (Match.Mask(mkInt(0,sz), mkInt(int_of_string string, sz)))
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

let encode_match (keys : (string * size) list) (h : t) encode_tag =
  match h.match_opt with
  | None -> None
  | Some matches ->
     List.fold2 keys matches ~init:True
       ~f:(fun acc (x,sz) match_opt ->
         acc %&%
           (match match_opt with
            | None ->
               (* Printf.printf "Encoding hole for %s\n%!"x; *)
               let tst = Hole.match_holes encode_tag h.table x sz in
               (* Printf.printf " == %s\n%!" (string_of_test tst); *)
               tst
            | Some m ->
               Match.to_test (x,sz) m)
       )
     |> or_unequal_lengths_to_option


let added_to_row (table : string) (m : value StringMap.t)  : bool =
  match Hole.add_row_hole_name table |> StringMap.find m with
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
                  | None ->
                     (* Printf.printf "Skipping key %s\n%!" ky; *)
                     acc
                  | Some (Match.Exact data) ->
                     (* Printf.printf "Exact Matching key %s\n%!" ky; *)
                     let m = StringMap.set acc ~key:(Hole.match_hole_exact hint.table ky) ~data in
                     (* Printf.printf "Model is now: %s\n%!" (string_of_map m); *)
                     m

                  | Some (Match.Between (lo, hi)) ->
                     (* Printf.printf "Between Matching key %s\n%!" ky; *)
                     let (klo, khi) = Hole.match_holes_range hint.table ky in
                     StringMap.set acc ~key:klo ~data:lo
                     |> StringMap.set ~key:khi ~data:hi
                  | Some (Match.Mask (v, m)) ->
                     (* Printf.printf "Mask Matching key %s\n" ky; *)
                     let (k,km) = Hole.match_holes_mask hint.table ky in
                     StringMap.set acc ~key:k ~data:v
                     |> StringMap.set ~key:km ~data:m
                )
      else
        acc)


let tbl_hole encode_tag (keys: (string * size) list) tbl row_hole act_hole i actSize (hs : t list) =
  let default_match_holes =
    List.fold ~init:True
      ~f:(fun acc (x,sz) ->
        acc %&% if x = "l3_metadata__nexthop_index" then
                  Hole.match_holes `Exact tbl x sz
                else Hole.match_holes encode_tag tbl x sz) in
  let matches_holes =
(*     if List.exists ["acl";"vlan"] ~f:(fun x -> String.is_substring tbl ~substring:x)
    then False
    else *)
    match List.find hs ~f:(fun h -> Stdlib.(h.table = tbl)) with
    | Some h ->
       (* if String.is_substring h.table ~substring:"punt" then False else *)
       begin match encode_match keys h encode_tag with
       | None ->  default_match_holes keys
       | Some phi -> phi
       end
    (* | None when List.length hs > 0 -> False *)
    | _ -> default_match_holes keys
 in
  matches_holes
  %&% (row_hole %=% mkVInt (1,1))
  %&% (act_hole %=% mkVInt (i,actSize))
