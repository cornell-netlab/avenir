open Bignum
open Core
open Ast
open Tables


let action_data_of_string (data_str : string) : Row.action_data =
  String.split data_str ~on:';'
  |> List.map ~f:(fun arg_str ->
         let value_str, size_str = String.lsplit2_exn arg_str ~on:'#' in
         Int(Bigint.of_string value_str, int_of_string size_str))
  
let matches_of_string (data_str : string) : Match.t list =
  String.split data_str ~on:';'
  |> List.map ~f:(fun match_str ->
         match String.lsplit2 match_str ~on:'#' with
         | Some (value_str, size_str) ->
            let size = int_of_string size_str in
            begin match String.lsplit2 value_str ~on:'/' with
            | Some (prefix_str, prefix_len_str) ->
               let prefix = Bigint.of_string prefix_str in
               let prefix_len = int_of_string prefix_len_str in
               if prefix_len = size then
                 Match.Exact(Int(prefix, size))
               else
                 let mask =
                   Printf.sprintf "0b%s%s" (String.make prefix_len '1') (String.make (size - prefix_len) '0') 
                   |> Bigint.of_string
                 in
                 Match.Mask (Int(prefix,size), Int(mask,size))
            | None -> Match.Exact(Int(Bigint.of_string value_str, size))
            end
         | None ->
            Printf.sprintf "Couldn't parse match from string %s" match_str
            |> failwith
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
      
    | "REMOVE"::_ ->
       failwith "cannot yet handle removes"
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
  
  
