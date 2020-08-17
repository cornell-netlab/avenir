open Core
open Ast
open Tables


type tuple = {
    in_port : Match.t option;
    ip_dst : Match.t option;
    ip_src : Match.t option;
    eth_dst : Match.t option;
    eth_src : Match.t option;
    tcp_dport : Match.t option;
    tcp_sport : Match.t option;
    eth_typ : Match.t option;
    vlan : Match.t option;
    pcp : Match.t option;
    proto : Match.t option;
  }
let default =
  { in_port = None;
    ip_dst = None;
    ip_src = None;
    eth_dst = None;
    eth_src = None;
    tcp_dport = None;
    tcp_sport = None;
    eth_typ = None;
    vlan = None;
    pcp = None;
    proto = None
  }

let rec to_int (bytes : int list) =
  match bytes with
  | [] -> 0
  | x::xs -> Int.shift_left x (8 * List.length xs) + to_int xs


let parse_ip_mask str =
  let addr_str, len_str =
    match String.substr_replace_all str ~pattern:"@" ~with_:""
          |> String.rsplit2 ~on:'/'
    with
    | None -> (str, None)
    | Some (addr,len) -> (addr, Some len)
  in
  let addr =
    String.split addr_str ~on:'.'
    |> List.fold ~init:"0x" ~f:(fun acc char_s ->
           Printf.sprintf "%s%s"
             acc
             (Bigint.(of_string char_s |> Hex.to_string)
              |> String.substr_replace_all ~pattern:"0x" ~with_:"")
         )
    |> Bigint.of_string
  in
  (* let addr = Bigint.of_string
   *            @@ Printf.sprintf "0x%s"
   *            @@ String.substr_replace_all addr_str ~pattern:"." ~with_:"" in *)
  match len_str with
  | None -> Match.exact_(Int(addr,32))
  | Some len_str ->
     let len = int_of_string len_str in
     if len = 32 then
       Match.exact_(Int(addr,32))
     else
       let mask = Bigint.of_string
                  @@ Printf.sprintf "0b%s%s"
                       (String.make len '1' )
                       (String.make (32-len) '0')
       in
       let lo_addr = Bigint.(addr land mask) in
       Match.mask_ (Int(lo_addr, 32)) (Int(mask, 32))


let parse_port_range str =
  let lo,hi = String.lsplit2_exn str ~on:':' in
  let lo_int = String.strip lo |> int_of_string in
  let hi_int = String.strip hi |> int_of_string in
  if lo_int = hi_int
  then Match.exact_(mkInt(lo_int, 16))
  else Match.between_ (mkInt(lo_int, 16)) ( mkInt(hi_int, 16))

let parse_proto str =
  let list = String.split str ~on:'\t' in
  let proto,mask = String.lsplit2_exn (List.hd_exn list) ~on:'/' in
  Match.mask_ (Int(Bigint.of_string proto, 8)) (Int(Bigint.of_string mask, 8))



let parse_eth_addr str =
  let str' = "0x" ^ String.substr_replace_all str ~pattern:":" ~with_:"" in
  Match.Exact(Int(Bigint.of_string str',48))

let debug (q, w, e, r, t) = (Tables.Match.to_string q) ^ " " ^ (Tables.Match.to_string w) ^ " " ^  (Tables.Match.to_string e) ^ " " ^ (Tables.Match.to_string r) ^ " " ^ (Tables.Match.to_string t)



let get_mask = function
  | Some Match.Mask(_,m) -> get_int m
  | Some Match.Exact(v) -> Bigint.of_string ("0b"^String.make (size_of_value v) '1')
  | Some Match.Between(lo,hi) -> Bigint.(get_int hi - get_int lo)
  | None -> Bigint.zero

let to_mask_tuple cb =
  let open Bigint in
  get_mask cb.in_port
  + get_mask cb.ip_dst
  + get_mask cb.ip_src
  + get_mask cb.eth_dst
  + get_mask cb.eth_src
  + get_mask cb.tcp_dport
  + get_mask cb.tcp_sport
  + get_mask cb.eth_typ
  + get_mask cb.vlan
  + get_mask cb.pcp
  + get_mask cb.proto



let parse_classbench fp =
  let rules = In_channel.read_lines fp
              |> List.fold ~init:[]
                   ~f:(fun rows line ->
                     let vars = String.split line ~on:'\t' in
                     let ip_src = List.nth_exn vars 0 |> parse_ip_mask |> Some in
                     let ip_dst = List.nth_exn vars 1 |> parse_ip_mask |> Some in
                     let tcp_sport = List.nth_exn vars 2 |> parse_port_range |> Some in
                     let tcp_dport = List.nth_exn vars 3 |> parse_port_range |> Some  in
                     let proto = List.nth_exn vars 4 |> parse_proto |> Some in
                     rows @ [{default with ip_src; ip_dst; tcp_sport; tcp_dport; proto}]) in
  rules




let parse_of ident line : Match.t option =
  match String.substr_index line ~pattern:ident with
  | None -> None
  | Some st ->
     let nd = String.substr_index ~pos:st line ~pattern:","
              |> Option.value ~default:(String.length line) in
     let data1 = String.drop_suffix line (String.length line - nd)  in
     let data = String.drop_prefix data1 (st + (String.length ident) + 1)in
     let out = match ident with
       | "in_port" -> Match.Exact(Int(Bigint.of_string data, 9))
       | "nw_proto" -> Match.Exact(Int(Bigint.of_string data, 8))
       | "eth_type" | "tp_src" | "tp_dst" -> Match.Exact(Int(Bigint.of_string data ,16))
       | "nw_dst" | "nw_src" -> parse_ip_mask data
       | "dl_src" | "dl_dst" -> parse_eth_addr data
       | _ -> failwith @@ Printf.sprintf "unrecognized header %s" ident
     in
     (* Printf.printf "From %s \n extract %s and %s \n convert to %s~%s\n%!" line data1 data ident (Match.to_string out); *)
     Some(out)

  
let parse_classbench_of fp =
  In_channel.read_lines fp
  |> List.fold ~init:[]
       ~f:(fun rows line ->
         let cb_row =
           { default with
             in_port = parse_of "in_port" line;
             ip_dst = parse_of "nw_dst" line;
             ip_src = parse_of "nw_src" line;
             eth_dst = parse_of "dl_dst" line;
             eth_src = parse_of "dl_src" line;
             tcp_dport = parse_of "tp_src" line;
             tcp_sport = parse_of "tcp_dst" line;
             eth_typ = parse_of "eth_type" line;
             proto = parse_of "nw_proto" line;
           } in
         rows @ [cb_row])

let get cb_row h =
  if h = "in_port" then cb_row.in_port
  else if h = "ip_dst" then cb_row.ip_dst
  else if h = "ip_src" then cb_row.ip_src
  else if h = "eth_dst" then cb_row.eth_dst
  else if h = "eth_src" then cb_row.eth_src
  else if h = "tcp_dport" then cb_row.tcp_dport
  else if h = "tcp_sport" then cb_row.tcp_sport
  else if h = "eth_typ" then cb_row.eth_typ
  else if h = "vlan" then cb_row.vlan
  else if h = "pcp" then cb_row.pcp
  else if h = "proto" then cb_row.proto
  else failwith @@ Printf.sprintf "Couldn't find header %s" h

let set cb_row h v =
  if h = "in_port" then {cb_row with in_port = v}
  else if h = "ip_dst" then {cb_row with ip_dst = v}
  else if h = "ip_src" then {cb_row with ip_src = v}
  else if h = "eth_dst" then {cb_row with eth_dst = v}
  else if h = "eth_src" then {cb_row with eth_src = v}
  else if h = "tcp_dport" then {cb_row with tcp_dport = v}
  else if h = "tcp_sport" then {cb_row with tcp_sport = v}
  else if h = "eth_typ" then {cb_row with eth_typ = v}
  else if h = "vlan" then {cb_row with vlan = v}
  else if h = "pcp" then {cb_row with pcp = v}
  else if h = "proto" then {cb_row with proto = v}
  else failwith @@ Printf.sprintf "Couldn't find header %s" h

let rec project cb_row hdrs =
  match hdrs with
  | [] -> default
  | h::hdrs -> set (project cb_row hdrs) h @@ get cb_row h

let generate f acc cb_row sz =
  let biggest = List.fold acc ~init:Bigint.one ~f:(fun max_so_far curr ->
                    match get curr "out_port" with
                    | Some (Exact Int (i,_)) when Bigint.(i > max_so_far) -> i
                    | _ -> max_so_far
                  ) in
  set cb_row f (Some (Exact (Int(Bigint.(biggest + one), sz))))

(* let classbench_to_acl fp table =
 *   let edits = parse_classbench fp in
 *   List.fold edits ~init:[]
 *     ~f:(fun acc (ip_src, ip_dst, src_port, dst_port, proto) ->
 *       let out_port = Random.int (pow 2 9) in
 *       acc
 *       @ [Tables.Edit.Add (table, ([ip_src; ip_dst; src_port; dst_port; proto], [mkInt(out_port, 9)], 0))]
 *     ) *)
