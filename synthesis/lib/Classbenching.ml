open Core
open Ast
open Synthesis
open Util
open Packet
open Tables

let rec to_int (bytes : int list) =
  match bytes with
  | [] -> 0
  | x::xs -> Int.shift_left x (8 * List.length xs) + to_int xs


let parse_ip_mask str =
  let addr, mask =
    String.substr_replace_all str ~pattern:"@" ~with_:""
    |> String.rsplit2_exn ~on:'/' in
  let addr_ints =
    String.split addr ~on:'.'
    |> List.map ~f:(fun i -> int_of_string i)
  in
  let mask_idx = int_of_string mask in
  let lo, hi = List.foldi addr_ints ~init:([],[])
                 ~f:(fun i (lo, hi) char ->
                   if i < mask_idx then
                     (lo @ [char], hi @ [char])
                   else
                     (lo @ [0], hi @ [255])) in
  let lo_int = to_int lo in
  let hi_int = to_int hi in
  if lo_int = hi_int then
    Match.Exact(mkInt(lo_int, 32))
  else
    Match.Mask(mkInt(lo_int, 32), mkInt(mask_idx, 32))


let parse_port_range str =
  let lo,hi = String.lsplit2_exn str ~on:':' in
  let lo_int = String.strip lo |> int_of_string in
  let hi_int = String.strip hi |> int_of_string in
  if lo = hi
  then Match.Exact(mkInt(lo_int, 9))
  else Match.Between(mkInt(lo_int, 9), mkInt(hi_int, 9))

let parse_proto str =
  let proto,_ = String.lsplit2_exn str ~on:'/' in
  Match.Exact(Int(Bigint.of_string proto, 8))

let debug (q, w, e, r, t) = (Tables.Match.to_string q) ^ " " ^ (Tables.Match.to_string w) ^ " " ^  (Tables.Match.to_string e) ^ " " ^ (Tables.Match.to_string r) ^ " " ^ (Tables.Match.to_string t)

let parse_classbench fp =
  In_channel.read_lines fp
  |> List.fold ~init:[]
       ~f:(fun rows line ->
         let vars = String.split line ~on:'\t' in
         let ip_src_match = List.nth_exn vars 0 |> parse_ip_mask in
         let ip_dst_match = List.nth_exn vars 1 |> parse_ip_mask in
         let src_port_match = List.nth_exn vars 2 |> parse_port_range in
         let dst_port_match = List.nth_exn vars 3 |> parse_port_range  in
         let proto = List.nth_exn vars 4 |> parse_proto in
         rows @ [(ip_src_match, ip_dst_match, src_port_match, dst_port_match, proto)])

let classbench_to_acl fp table =
  let edits = parse_classbench fp in
  List.fold edits ~init:[]
    ~f:(fun acc (ip_src, ip_dst, src_port, dst_port, proto) ->
      let out_port = Random.int (pow 2 9) in
      acc
      @ [Tables.Edit.Add (table, ([ip_src; ip_dst; src_port; dst_port; proto], [mkInt(out_port, 9)], 0))]
    )