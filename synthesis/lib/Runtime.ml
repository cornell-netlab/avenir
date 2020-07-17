open Bignum
open Core
open Ast
open Tables


let action_data_of_string (data_str : string) : Row.action_data =
  String.split data_str ~on:';'
  |> List.filter ~f:(fun s -> String.is_empty s |> not)
  |> List.map ~f:(fun arg_str ->
      let value_str, size_str = String.lsplit2_exn arg_str ~on:'#' in
      if String.is_substring value_str ~substring:"."
      then let addr_str = Option.(String.lsplit2 value_str ~on:'/' >>| fst |> value ~default:value_str) in
           match Classbenching.parse_ip_mask (addr_str ^ "/32") with
           | Match.Exact (addr) | Match.Mask (addr,_) -> addr
           |  _ -> failwith @@ Printf.sprintf "ERROR::Range extracted from match %s. Expected Exact or Mask!" arg_str
      else Int(Bigint.of_string value_str, int_of_string size_str))

let matches_of_string (data_str : string) : Match.t list =
  String.split data_str ~on:';'
  |> List.map ~f:(fun match_str ->
      match String.index match_str '[' with
      | Some _ ->
         begin match String.lsplit2 match_str ~on:'#' with
         | Some (vals, sz) ->
            let size = int_of_string sz in
            let range = String.strip
                          ~drop:(fun c -> c = '[' || c = ']') vals in
            begin match String.lsplit2 range ~on:':' with
            | Some (hi, lo) -> Match.Between (Int(Bigint.of_string hi, size), Int(Bigint.of_string lo, size))
            | _ -> Printf.sprintf "Couldn't parse match from string %s" range
                   |> failwith
            end
         | _ -> failwith @@ Printf.sprintf "Couldn't parse match from string %s" match_str
         end
      | _ ->
         begin match String.lsplit2 match_str ~on:'&' with
         | Some (fst, snd) ->
            begin match String.lsplit2 snd ~on:'#' with
            | Some (prefix_str, size_str) ->
               let size = int_of_string size_str in
               let mask = Bigint.of_string prefix_str in
               let addr = Bigint.of_string fst in
               Match.Mask(Int(addr, size), Int(mask, size))
            | _ -> Printf.sprintf "Couldn't parse match from string %s" match_str
                   |> failwith
            end
         | _ ->
            begin match String.lsplit2 match_str ~on:'/' with
            | Some _ ->
               begin match String.lsplit2 match_str ~on:'#' with
               | Some (match_str, size) when (size = "32")->
                  Classbenching.parse_ip_mask match_str
               | _ -> Printf.sprintf "Couldn't parse match from string %s" match_str
                      |> failwith
               end
            | _ ->
               begin match String.lsplit2 match_str ~on:'#' with
               | Some (value_str, size_str) ->
                  let size = int_of_string size_str in
                  let value = Bigint.of_string value_str in
                  Match.Exact(Int(value, size))
               | None ->
               Printf.sprintf "Couldn't parse match from string %s" match_str
               |> failwith
               end
            end
         end
       )


let parse filename : Edit.t list =
  let lines = In_channel.read_lines filename in
  let make_edit (data : string list) : Edit.t =
    match data with
    | ["ADD"; tbl_nm; matches; action_data; action] ->
      Add (tbl_nm,
           (matches_of_string matches,
            action_data_of_string action_data,
            int_of_string action))

    | ["DEL";tbl_nm;action] ->
       Del (tbl_nm, int_of_string action)
    | _ ->
      Printf.sprintf "Unrecognized row: %s\n%!"
        (List.intersperse data ~sep:"---" |> List.reduce_exn ~f:(^))
      |> failwith
  in
  let edits =
    List.map lines ~f:(fun line ->
        String.split line ~on:','
        |> make_edit
      ) in
  edits
