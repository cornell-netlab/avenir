open Core
open Ast
open Synthesis
open Util
open Packet
open Tables
open Classbenching
module IntMap = Map.Make(Int)


let rec run_experiment iter seq (phys_seq : Edit.t list) (params : Parameters.t) hints (problem : Problem.t) =
  match seq with
  | [] -> Some phys_seq
  | edit::edits ->
     (* Printf.printf "==== BENCHMARKING INSERTION OF (%s) =====\n%!"
      *   (string_of_edit edit); *)
     let data = ProfData.zero () in
     let problem_inner = Problem.(replace_log_edits problem edit) in
     let st = Time.now () in
     assert (List.length (Problem.phys_edits problem_inner) = 0);
     match cegis_math params data problem_inner with
     | None -> None
        (* (\* let _ : Edit.t list option = cegis_math {params with debug = true} data problem_inner in *\)
         * failwith "example failed" *)
     | Some pedits ->
        !data.time := Time.(diff (now()) st);
        !data.log_inst_size :=  Problem.log_inst problem |> Instance.size ;
        !data.phys_inst_size := Problem.phys_inst problem |> Instance.size;
        Printf.printf "%s\n%!" (ProfData.to_string !data);
        run_experiment (iter + 1)
          edits
          (phys_seq @ pedits)
          (if false then {params with debug = true; interactive = true} else params )
          hints
          Problem.(problem
                   |> flip (apply_edits_to_log params) edit
                   |> flip (apply_edits_to_phys params) pedits
                   |> delete_phys_edits)
                        
let measure params hints problem insertions =
  Printf.printf "%s\n%!" ProfData.header_string;
  run_experiment 0 insertions [] params hints problem

                                            
let permute l =
  List.map l ~f:(inj_r (Random.int (List.length l)))
  |> List.sort ~compare:(fun (i, _) (j,_) -> compare i j)
  |> List.map ~f:(snd) 
              
let tbl = Printf.sprintf "tbl%d"
    
let rec mk_pipeline varsize =
  fun n ->
  if n = 0 then [] else
    (tbl n
    , [("k_" ^tbl n, varsize)]
    , [([Printf.sprintf "v%d" n,varsize],("x_"^tbl n) %<-% Var(Printf.sprintf "v%d" n,varsize))]
    , ("x_"^tbl n) %<-% mkVInt (0,varsize)
    ) :: mk_pipeline varsize (n-1)


let rec generate_n_insertions varsize length n avail_tables maxes : Edit.t list =
  if n = 0 then
    let _ : unit = Printf.printf "--generated--\n%!"in
    []
  else if avail_tables = [] then
    let _ : unit = Printf.printf "--filled up --\n%!" in
    []
  else
    let _ : unit = Printf.printf "generating %d \n%!" n in
    let rec loop_free_match avail_tables =
      if avail_tables = []
      then None
      else
        let i = Random.int (List.length avail_tables) |> List.nth_exn avail_tables in
        let max_i = StringMap.find maxes (tbl i) |> Option.value ~default:0 in
        Printf.printf "%s max : %d\n%!" (tbl i) max_i;
        if max_i >= pow 2 varsize
        then loop_free_match (List.filter avail_tables ~f:((<>) i))
        else
          let (max', mtch) =
            if Random.int 6 < 1 then
              (max_i + 1, Match.Exact (mkInt(max_i, varsize)))
            else
              let lo = max_i in
              let hi = min (lo + Random.int 3) (pow 2 varsize - 1) in
              if lo = hi then
                (hi + 1, Match.Exact (mkInt(hi, varsize)))
              else
                (hi + 1, Match.Between (mkInt(lo, varsize), mkInt(hi, varsize)))
          in
          let maxes' = StringMap.set maxes ~key:(tbl i) ~data:max' in
          let act_data = mkInt(Random.int (pow 2 varsize),varsize) in
          let row = ([mtch], [act_data], 0) in
          Some (maxes', avail_tables, tbl i, row)
    in
    match loop_free_match avail_tables with
    | None ->
       let _ : unit = Printf.printf "--filled up --\n%!" in
       []
    | Some (maxes', avail_tables', name, row) ->
       let _ : unit = Printf.printf "Inserting\n%!" in
       Add (name, row)
       :: generate_n_insertions varsize length (n-1) avail_tables' maxes'
                                  
let reorder_benchmark varsize length max_inserts params =
  Random.init 99;
  let logical_pipeline = mk_pipeline varsize length in
  let physical_pipeline = permute logical_pipeline in
  let mk_empty_inst acc (name,_,_,_) = Map.set acc ~key:name ~data:[]  in
  let log_inst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let phys_inst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let to_cmd line =  List.((line >>| fun t -> mkApply t)
                           |> reduce_exn ~f:(%:%)) in
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence =
    generate_n_insertions varsize length max_inserts (range_ex 1 (length +1)) StringMap.empty
    |> List.(map ~f:return)
  in
  let fvs = range_ex 1 (length + 1)
            |> List.map ~f:(fun i ->
                   [("k_" ^tbl i, varsize)
                   ; ("x_" ^tbl i, varsize)
                       (* ; ("?ActIn"^tbl i, 8) *)
                 ])
            |> List.join
  in
  let problem = Problem.make ~log ~phys ~fvs ~log_inst ~phys_inst ~log_edits:[] () in
  measure params (Some (List.return)) problem insertion_sequence


  
            
(** ONF BENCHMARK **)
   
let set_valid s = (s ^ ".valid") %<-% mkVInt(1,1)
let set_invalid s = (s ^ ".valid") %<-% mkVInt(0,1)
let is_valid s = Var(s^".valid",1) %=% mkVInt(1,1)
let std_meta_in_port_str = "standard_metadata.ingress_port"
let std_meta_in_port = Var(std_meta_in_port_str,9)
let cpu_port = mkVInt(255,9)
let hdr_packet_out_str = "hdr.packet_out"
let hdr_ethernet_str = "hdr.ethernet"
let fabric_metadata_vlan_id_str = "fabric_metadata_vlan_id"
let default_vlan_id = mkVInt(4094, 12)
let hdr_eth_type_str = "hdr.eth_type"
let hdr_eth_type_value_str = "hdr.eth_type.value"
let hdr_eth_type_value = Var(hdr_eth_type_value_str, 16)
let ethertype_mpls = mkVInt(34887,16)
let hdr_mpls_str = "hdr_mpls_str"
let fabric_metadata_mpls_label_str = "fabric_metadata.mpls_label_str"
let fabric_metadata_mpls_ttl_str = "fabric_metadata.mpls.ttl"
let hdr_mpls_ttl_str = "hdr.mpls.ttl"
let hdr_mpls_ttl = Var(hdr_mpls_ttl_str, 8)
(* let fabric_metadata_mpls_label = Var(fabric_metadata_mpls_label_str, 20) *)
let hdr_mpls_label_str = "hdr.mpls.label"
let hdr_mpls_label = Var(hdr_mpls_label_str, 20)
let hdr_ipv4_str = "hdr.ipv4"
let fabric_metadata_ip_proto_str = "fabric_metadata.ip_proto"
let fabric_metadata_ip_eth_typ_str = "fabric_metadata.ip_eth_typ"
let hdr_ipv4_protocol_str = "hdr.ipv4.protocol"
let hdr_ipv4_protocol = Var(hdr_ipv4_protocol_str, 8)
let ethertype_ipv4 = mkVInt(2048, 16)
let proto_tcp = mkVInt(6,8)
let proto_udp = mkVInt(17,8)
let proto_icmp = mkVInt(1,8)
let hdr_tcp_str = "hdr.tcp"
let fabric_metadata_l4_sport_str = "fabric_metadata.l4_sport"
let fabric_metadata_l4_dport_str = "fabric_metadata.l4_dport"  
let hdr_tcp_sport_str = "hdr.tcp.sport"
let hdr_tcp_sport = Var(hdr_tcp_sport_str, 16)
let hdr_tcp_dport_str = "hdr.tcp.dport"
let hdr_tcp_dport = Var(hdr_tcp_dport_str, 16)
let hdr_udp_str = "hdr.udp"
let hdr_udp_sport_str = "hdr.udp.sport"
let hdr_udp_sport = Var(hdr_udp_sport_str, 16)
let hdr_udp_dport_str = "hdr.udp.dport"
let hdr_udp_dport = Var(hdr_udp_dport_str, 16)
let hdr_icmp_str = "hdr.icmp"
                     
let hdr_packet_out_str = "hdr_packet_out_str"
let standard_metadata_egress_spec_str = "standard_metadata.egress_spec"
let standard_metadata_egress_spec = Var(standard_metadata_egress_spec_str, 9)
let hdr_packet_out_egress_port_str = "hdr.packet_out.egress_port"
let hdr_packet_out_egress_port = Var(hdr_packet_out_egress_port_str, 9)
let fabric_metadata_is_controller_packet_out_str = "fabric_metadata.is_controller_packet_out"
let default_mpls_ttl = mkVInt(64,8)
let fabric_metadata_fwd_type_str = "fabric_metadata.fwd_type"
let fabric_metadata_fwd_type = Var(fabric_metadata_fwd_type_str, 3)
let fwd_bridging = mkVInt(0,3)
let fwd_mpls = mkVInt(1,3)
let fwd_ipv4_unicast = mkVInt(2,3)
let fabric_metadata_skip_next_str = "fabric_metadata.skip_next"
let fabric_metadata_skip_next = Var(fabric_metadata_skip_next_str, 1)
let standard_metadata_ingress_port_str = "standard_metadata_ingress_port"
let hdr_ethernet_src_addr_str = "hdr_ethernet.src_addr"
let hdr_ethernet_dst_addr_str = "hdr_ethernet.dst_addr"
let hdr_ipv4_src_addr_str = "hdr_ipv4.src_addr"
let hdr_ipv4_dst_addr_str = "hdr_ipv4.dst_addr"
let hdr_icmp_icmp_type = "hdr_icmp_icmp_type"
let hdr_icmp_icmp_code = "hdr_icmp_icmp_code"
let fabric_metadata_next_id_str = "fabric_metadata.next_id"
let mark_to_drop = "standard_metadata.drop" %<-% mkVInt(1,1)
                                                       
let fabric_metadata_is_multicast_str = "fabric_metadata.is_multicast" 
let fabric_metadata_is_multicast = Var(fabric_metadata_is_multicast_str, 1) 
let standard_metadata_ingress_port = Var(standard_metadata_ingress_port_str, 9) 
let standard_metadata_egress_port_str = "standard_metadata.egress_port" 
let standard_metadata_egress_port = Var(standard_metadata_egress_port_str, 9) 
let hdr_mpls_str = "hdr.mpls" 
let hdr_mpls_tc_str = "hdr.mpls.tc" 
let hdr_mpls_bos_str = "hdr.mpls.bos" 
let hdr_mpls_ttl_str = "hdr.mpls.ttl" 
let hdr_mpls_ttl = Var(hdr_mpls_ttl_str, 8) 
let fabric_metadata_mpls_label = Var(fabric_metadata_mpls_label_str, 20) 
let fabric_metadata_ip_eth_typ = Var(fabric_metadata_ip_eth_typ_str, 16)
let fabric_metadata_mpls_ttl = Var(fabric_metadata_mpls_ttl_str, 8) 
let hdr_ipv4_ttl_str = "hdr.ipv4.ttl" 
let hdr_ipv4_ttl = Var(hdr_ipv4_ttl_str, 8)
let fabric_metadata_skip_forwarding_str = "fabric_metadata.skip_forwarding"
let fabric_metadata_skip_forwarding = Var(fabric_metadata_skip_forwarding_str, 1)                       
let fabric_metadata_is_controller_packet_out_str = "fabric_metadata.is_controller_packet_out"
let fabric_metadata_is_controller_packet_out = Var(fabric_metadata_is_controller_packet_out_str, 1)
let hdr_packet_in = "hdr.packet_in"
let hdr_packet_in_ingress_port_str = "hdr.packet_in.ingress_port"
let hdr_packet_in_egress_port_str = "hdr.packet_in.egress_port"
let loopback_port = mkVInt(13,9)
let local_metadata_l3_admit_str = "local_metadata_l3_admit"
let local_metadata_l3_admit = Var(local_metadata_l3_admit_str, 1)
let local_metadata_egress_spec_at_punt_match = "local_metadata.egress_spec_at_punt_match"
let local_metadata_egress_spec_at_punt_match = Var(local_metadata_egress_spec_at_punt_match, 9)

    
let onos_to_edits filename =
  let lines = In_channel.read_lines filename in
  let make_edit tbl_nm data : Edit.t =
    match data with
    | [ts; "ADD"; _; ipv6; id] ->
       Add (tbl_nm, ([Match.mk_ipv6_match ipv6], [Int(Bigint.of_string id, 32)], 0))
    | [_; "REMOVE"; _; _; _] ->
       failwith "cannot yet handle removes"
    | _ ->
       Printf.sprintf "Unrecognized row: %s\n%!"
         (List.intersperse data ~sep:"---" |> List.reduce_exn ~f:(^))
       |> failwith
  in
  let edits =
    List.map lines ~f:(fun line ->
        [String.split line ~on:','
         |> make_edit "ipv6"]
      ) in
  edits

                                                      

let fwd_classifier_table =
  mkApply(
      "fwd_classifier"
    , [ (standard_metadata_ingress_port_str, 9 (*exact*))
      ; (hdr_ethernet_dst_addr_str, 48 (*ternary*))
      ; (hdr_eth_type_value_str, 16 (*ternary*))
      ; (fabric_metadata_ip_eth_typ_str, 16 (*exact*))]
    , [ ([("fwd_type",3)], fabric_metadata_fwd_type_str %<-% Var("fwd_type", 3))]
    , fabric_metadata_fwd_type_str %<-% fwd_bridging)
let bridging_table =
  mkApply("bridging"
      , [ (fabric_metadata_vlan_id_str, 12 (*exact*))
        ; (hdr_ethernet_dst_addr_str, 48 (*ternary*))
        ]
      , [([("next_id",32)], fabric_metadata_next_id_str %<-% Var("next_id",32))]
      , Skip)
       
let mpls_table =
  mkApply("mpls"
      , [ (fabric_metadata_mpls_label_str, 20 (* exact *) ) ]
      , [ ([("next_id",32)],  fabric_metadata_next_id_str %<-% Var("next_id", 32)) ]
      , Skip )
let ipv4_tbl =
  mkApply("ipv4"
      , [ (hdr_ipv4_dst_addr_str, 20 (*exact *))]
      , [ ([("next_id",32)],  fabric_metadata_next_id_str %<-% Var("next_id", 32)) ]
      , Skip)
       
let acl_table = mkApply("acl",
                      [ (standard_metadata_ingress_port_str, 9  (*exact*))
                      ; (fabric_metadata_ip_proto_str, 8 (*ternary*))
                      ; (fabric_metadata_l4_sport_str, 16 (*ternary*))
                      ; (fabric_metadata_l4_sport_str, 16 (*ternary*))
                      ; (hdr_ethernet_dst_addr_str, 48 (*ternary*))
                      ; (hdr_eth_type_value_str, 16 (*ternary*))
                      ; (hdr_ipv4_src_addr_str, 32 (*ternary*))
                      ; (hdr_icmp_icmp_type, 8 (*ternary*))
                      ; (hdr_icmp_icmp_code, 8 (*ternary*))
                      ]
                      ,[ ([("next_id",32)], fabric_metadata_next_id_str %<-% Var("next_id", 32))
                       ; ([], sequence [ standard_metadata_egress_spec_str %<-% cpu_port
                                       ; fabric_metadata_skip_next_str %<-% mkVInt(1,1)])
                       ; ([], sequence [mark_to_drop; fabric_metadata_skip_next_str %<-% mkVInt(1,1)])
                       ; ([], Skip)
                       ] 
                      , fabric_metadata_fwd_type_str %<-% fwd_bridging)
                     

let apply_simple_next =
  mkApply("next.simple"
      , [(fabric_metadata_next_id_str, 32 (*exact*))]
      , [ ([("port",9)], standard_metadata_egress_spec_str %<-% Var("port", 9))
        ; ([("port",9);("smac",48);("dmac",48)], sequence [ hdr_ethernet_src_addr_str %<-% Var("smac", 48)
                                            ; hdr_ethernet_dst_addr_str %<-% Var("dmac", 48)
                                            ; standard_metadata_egress_spec_str %<-% Var("port", 8)])
        ; ([("label",20);("port",9);("smac",48);("dmac",48)],
           sequence [ fabric_metadata_mpls_label_str %<-% Var("label", 20)
                    ; hdr_ethernet_src_addr_str %<-% Var("smac", 48)
                    ; hdr_ethernet_dst_addr_str %<-% Var("dmac", 48)
                    ; standard_metadata_egress_spec_str %<-% Var("port",8)])
        ]
      , Skip)



       
let prsr = Skip
 (*   [ [ (std_meta_in_port %=% cpu_port, set_valid hdr_packet_out_str)
 *     ; (True, Skip) ] |> mkOrdered
 *   ; set_valid hdr_ethernet_str
 *   ; fabric_metadata_vlan_id_str %<-% default_vlan_id
 *   ; set_valid hdr_eth_type_str
 *   ; [ (hdr_eth_type_value %=% ethertype_mpls
 *       , [ set_valid hdr_mpls_str
 *         ; fabric_metadata_mpls_label_str %<-% hdr_mpls_label
 *         ; fabric_metadata_mpls_ttl_str %<-% hdr_mpls_ttl
 *         ; ipv4_prsr] |> sequence)
 *     ; (hdr_eth_type_value %=% ethertype_ipv4
 *       , ipv4_prsr)
 *     ; (True, Skip)
 *     ] |> mkOrdered
 *   ] |> List.reduce_exn ~f:(%:%) *)
                       
let ingress =
  [(is_valid hdr_packet_out_str,
    [ standard_metadata_egress_spec_str %<-% hdr_packet_out_egress_port
    ; set_invalid hdr_packet_out_str
    ; fabric_metadata_is_controller_packet_out_str %<-% mkVInt(1,1)
    ] |> sequence)
  ;  (True,
      [ (is_valid hdr_mpls_str,
         fabric_metadata_mpls_ttl_str %<-% Plus(default_mpls_ttl, mkVInt(1,8)))
      ; (True,
         [ fwd_classifier_table
         ; mkOrdered[
               ( fabric_metadata_skip_forwarding %=% mkVInt(0,1)
               , [ (fabric_metadata_fwd_type %=% fwd_bridging, bridging_table)
                 ; (fabric_metadata_fwd_type %=% fwd_mpls, mpls_table)
                 ; (fabric_metadata_fwd_type %=% fwd_ipv4_unicast, ipv4_tbl)
                 ] |> mkOrdered)
             ; (True, Skip)]
         ; acl_table
         ; [ (fabric_metadata_skip_next %=% mkVInt(0,1), apply_simple_next) (*and others?*)
           ; (True, Skip)
           ] |> mkOrdered
         ] |> sequence

      )] |> mkOrdered)
       
  ] |> mkOrdered
         
let egress =
  mkOrdered [
      (fabric_metadata_is_controller_packet_out %=% cpu_port, Skip)
    ; (standard_metadata_egress_port %=% cpu_port,
       sequence [
           set_valid hdr_packet_in
         ; hdr_packet_in_ingress_port_str %<-% standard_metadata_egress_port])
    ; (True, 
       sequence [
         (*   mkOrdered
          *     [ ((fabric_metadata_is_multicast %=% mkVInt(1,1)) %&% (standard_metadata_ingress_port %=% standard_metadata_egress_port)
          *       , mark_to_drop)
          *     ; (True, Skip)]
          * ; *)
           mkOrdered
             [ (is_valid hdr_mpls_str, sequence [ set_invalid hdr_mpls_str
                                                ; hdr_eth_type_value_str %<-% fabric_metadata_ip_eth_typ])
             ; (True,
                sequence [
                    set_valid hdr_mpls_str
                  ; hdr_mpls_label_str %<-% fabric_metadata_mpls_label
                  ; hdr_mpls_tc_str %<-% mkVInt(0,3)
                  ; hdr_mpls_bos_str %<-% mkVInt(1,1)
                  ; hdr_mpls_ttl_str %<-% fabric_metadata_mpls_ttl
                  ; hdr_eth_type_value_str %<-% ethertype_mpls]
               )
             ]
         ; mkOrdered
             [ (is_valid hdr_mpls_str,
                sequence [
                    hdr_mpls_ttl_str %<-% Minus(hdr_mpls_ttl, mkVInt(1,8))
                  ; mkOrdered [
                        (hdr_mpls_ttl %=% mkVInt(0,8), mark_to_drop)
                      ; (True, Skip) 
                      ]
               ])
             ; (is_valid hdr_ipv4_str,
                sequence [
                    hdr_ipv4_ttl_str %<-% Minus(hdr_ipv4_ttl, mkVInt(1,8))
                  ; mkOrdered [
                   (hdr_ipv4_ttl %=% mkVInt(0,8), mark_to_drop)
                      ; (True, Skip)
                      ]
               ])
             ; (True, Skip)
             ]
         ]
      )
    ]
           

    
let fabric_no_vlan =
  sequence
    [ prsr
    ; ingress
    ; egress
    ]

    


let eth_ipv4_tcp_prsr =
  sequence [
    (*   mkOrdered [
     *       (standard_metadata_ingress_port %=% cpu_port, set_valid hdr_packet_out_str)
     *     ; (True, Skip)
     *     ]
     * ; *)
      set_valid hdr_ethernet_str
    ; fabric_metadata_vlan_id_str %<-% default_vlan_id
    ; set_valid hdr_eth_type_str
    ; Assume(hdr_eth_type_value %=% ethertype_ipv4)
    ; set_valid hdr_ipv4_str
    ; fabric_metadata_ip_proto_str %<-% hdr_ipv4_protocol
    ; fabric_metadata_ip_eth_typ_str %<-% ethertype_ipv4
    ; Assume (hdr_ipv4_protocol %=% proto_tcp)
    ; set_valid hdr_tcp_str
    ; fabric_metadata_l4_sport_str %<-% hdr_tcp_sport
    ; fabric_metadata_l4_dport_str %<-% hdr_tcp_dport]

let ingress_ipv4_tcp =
  sequence [
      mkOrdered [
        (*   (is_valid hdr_packet_out_str,
         *    sequence [
         *        standard_metadata_egress_spec_str %<-% hdr_packet_out_egress_port
         *      ; set_invalid hdr_packet_out_str
         *      ; fabric_metadata_is_controller_packet_out_str %<-% mkVInt(1,1) ])
         * ; *) (True,
           sequence [
               fwd_classifier_table
             ; mkOrdered [
                   (fabric_metadata_skip_forwarding %=% mkVInt(0,1), 
                    mkOrdered
                      [ (fabric_metadata_fwd_type %=% fwd_bridging, bridging_table)
                      ; (fabric_metadata_fwd_type %=% fwd_mpls, mpls_table)
                      ; (fabric_metadata_fwd_type %=% fwd_ipv4_unicast, ipv4_tbl)
                      ; (True, Skip)
                   ])
                 ; (True, Skip)
                 ]
             ; acl_table
             ; mkOrdered [
                    (fabric_metadata_skip_next %=% mkVInt(0,1), apply_simple_next)
                  ; (True, Skip)
                 ]
             ]
          )
        ]
    ]

let egress_ipv4_tcp =
  (* mkOrdered [
   *   (\*   (fabric_metadata_is_controller_packet_out %=% cpu_port, Skip)
   *    * ; *\)
   *   (\*   (standard_metadata_egress_port %=% cpu_port,
   *    *    sequence [
   *    *        set_valid hdr_packet_in
   *    *      ; hdr_packet_in_ingress_port_str %<-% standard_metadata_egress_port])
   *    * ; *\)
   *     (True, *)
       sequence [
          (*  mkOrdered [
          *       ((fabric_metadata_is_multicast %=% mkVInt(1,1))
          *        %&% (standard_metadata_ingress_port %=% standard_metadata_egress_port)
          *       , mark_to_drop)
          *     ; (True, Skip)
          *     ]
          * ; *) mkOrdered [
               (is_valid hdr_ipv4_str, sequence [ hdr_ipv4_ttl_str %<-% Minus(hdr_ipv4_ttl, mkVInt(1,8))
                                                ; mkOrdered [(hdr_ipv4_ttl %=% mkVInt(0,8), mark_to_drop); (True, Skip)]
                                         ]
                                                
               )
             ; (True, Skip)
             ]
         ]
    (*   )
     * ] *)

           
           
    
let fabric_eth_ipv4_tcp =
  sequence
    [ eth_ipv4_tcp_prsr
    ; ingress_ipv4_tcp
    ; egress_ipv4_tcp ]    
    

let bcm_eth_ipv4_tcp_parser =
  sequence [
      Assume(standard_metadata_ingress_port %<>% cpu_port)
    ; set_valid hdr_ethernet_str
    ; Assume(hdr_eth_type_value %=% ethertype_ipv4)
    ; set_valid hdr_ipv4_str
    ; Assume(hdr_ipv4_protocol %=% proto_tcp)
    ; set_valid hdr_tcp_str
    ; fabric_metadata_l4_sport_str %<-% hdr_tcp_sport
    ; fabric_metadata_l4_dport_str %<-% hdr_tcp_dport
    ]

let my_station_table =
  mkApply("my_station_table"
      , [(hdr_ethernet_dst_addr_str, 48 (*ternary*))]
      , [ ( [] , local_metadata_l3_admit_str %<-% mkVInt(1,1) )
        ; ( [], Skip )]
      , Skip)

let l3_fwd =
  mkApply("l3_fwd"
      , [(hdr_ipv4_dst_addr_str, 32 (*lpm*))
        ; (hdr_ipv4_src_addr_str, 32 (*selector*))
        ; (hdr_ipv4_protocol_str, 8 (*selector*))
        ; (fabric_metadata_l4_sport_str, 16 (*selector*))
        ]
      , [ [("port",9); ("smac",48); ("dmac",48); (*"dst_vlan",*)],
          sequence [
              standard_metadata_egress_spec_str %<-% Var("port", 9)
              (*            ; local_metadata_dst_vlan %<-% Var("dst_vlan", ??)*)
            ; hdr_ethernet_src_addr_str %<-% Var("smac", 48)
            ; hdr_ethernet_dst_addr_str %<-% Var("dmac", 48)
            ; hdr_ipv4_ttl_str %<-% Minus(hdr_ipv4_ttl, mkVInt(1,8))
            ]
        ]
      , Skip)

let l2_unicast_table =
  mkApply("l2_unicast_table"
      , [(hdr_ethernet_dst_addr_str, 48 (*exact*))]
      , [([("port",9)], standard_metadata_egress_spec_str %<-% Var("port", 9) )]
      , Skip)
       
let bcm_eth_ipv4_tcp_ingress=
  let cpu_proc =
    mkOrdered [
        (is_valid hdr_packet_out_str, sequence [
                                          standard_metadata_egress_spec_str %<-% hdr_packet_out_egress_port
                                        ; set_invalid hdr_packet_out_str])
      ; (True, Skip)
      ]
  in
  (* let punt_table = mkApply("punt", [], [], Skip) in *)
  let l2_l3_tables =
    mkOrdered [
        ((standard_metadata_egress_spec %=% mkVInt(0,9)) %+% (standard_metadata_egress_spec %=% loopback_port),
         sequence [
             my_station_table
           ; mkOrdered [
                 (local_metadata_l3_admit %=% mkVInt(1,1), l3_fwd)
               ; (True, l2_unicast_table)
               ]
           (* ; punt_table *)
           ]
        )
      ]
  in
  sequence [
      cpu_proc
    ; l2_l3_tables
    ]

let bcm_eth_ipv4_tcp_egress =
  mkOrdered [
      (standard_metadata_egress_spec %=% cpu_port, sequence [
                                                       set_valid hdr_packet_in
                                                     ; hdr_packet_in_ingress_port_str %<-% standard_metadata_ingress_port
                                                     ; hdr_packet_in_egress_port_str %<-% local_metadata_egress_spec_at_punt_match ])
    ; (True, Skip)
    ]
           
let bcm_eth_ipv4_tcp =
  sequence
    [ standard_metadata_egress_spec_str %<-% mkVInt(0,9)
    ; local_metadata_l3_admit_str %<-% mkVInt(0,1)
    ; bcm_eth_ipv4_tcp_parser
    ; bcm_eth_ipv4_tcp_ingress
    (*; bcm_eth_ipv4_tcp_egress*) ]

(* let simple_benchmark =
 *   let fvs = (List.filter (free_vars_of_cmd fabric_eth_ipv4_tcp) ~f:(fun v -> List.exists (free_vars_of_cmd bcm_eth_ipv4_tcp) ~f:((=) v))) in
 *   synthesize_edit ~gas:5 ~iter:0 (Prover.solver ())
 *     ~fvs
 *     fabric_eth_ipv4_tcp (\*logical*\)    
 *     bcm_eth_ipv4_tcp (\*physical*\)
 *     StringMap.empty
 *     StringMap.empty
 *     ("ipv4",([Between(0, pow 2 32, 32)], [], 1))
 *   |> ignore *)


let (%>) c c' = ignore c; c'


let basic_onf_ipv4 params filename = 
  let log =
    sequence [
        "class_id" %<-% mkVInt(0,32)
      ; mkApply("ipv6",
              [("ipv6_dst", 128)],
              [([("next_id",32)], "class_id"%<-% Var("next_id",32))],
              Skip)
      ;
        mkOrdered [
            Var("class_id",32) %=% mkVInt(1017,32), "out_port" %<-% mkVInt(17,9) ;
            Var("class_id",32) %=% mkVInt(1018,32), "out_port" %<-% mkVInt(18,9) ;
            Var("class_id",32) %=% mkVInt(1010,32), "out_port" %<-% mkVInt(10,9) ;
            Var("class_id",32) %=% mkVInt(1012,32), "out_port" %<-% mkVInt(12,9) ;
            Var("class_id",32) %=% mkVInt(1011,32), "out_port" %<-% mkVInt(11,9) ;
            Var("class_id",32) %=% mkVInt(1008,32), "out_port" %<-% mkVInt(08,9) ;
            Var("class_id",32) %=% mkVInt(1003,32), "out_port" %<-% mkVInt(03,9) ;
            Var("class_id",32) %=% mkVInt(1019,32), "out_port" %<-% mkVInt(19,9) ;
            True, "out_port" %<-% mkVInt(0,9)
          ]
      ] in
  let phys =
    mkApply("l3_fwd"
        , [ ("ipv6_dst", 128) (*; ("ipv4_src", 32); ("ipv4_proto", 16)*) ]
        , [ ([("port",9)], "out_port"%<-% Var("port",9))]
        , "out_port" %<-% mkVInt(0,9)) in
  let fvs = [("ipv6_dst", 128); ("out_port", 9); (*("ipv4_src", 32); ("ipv4_proto", 16)*)] in
  (* synthesize_edit  ~gas ~iter ~fvs p 
   *   logical
   *   physical
   *   StringMap.empty
   *   StringMap.empty
   *   ("next", ([Exact (1,32)], [(1,9)],0))
   * %> *)
  let problem =
    Problem.make ~log ~phys ~fvs
      ~log_inst:StringMap.(set empty ~key:"ipv6" ~data:[])
      ~log_edits:[]
      ~phys_inst:StringMap.(set empty ~key:"l3_fwd" ~data:[]) ()
  in
  measure params None problem (onos_to_edits filename)




let parse_rule_to_update line =
  let columns = String.split line in columns 

      
let get_classbench_data n =
  let rules = Shell.run_lines "/home/ericthewry/Downloads/classbench-ng/classbench" ["v4";"/home/ericthewry/Downloads/classbench-ng/vendor/parameter_files/fw1_seed";Printf.sprintf "--count=%d" n] in
  List.map rules ~f:parse_rule_to_update 
      

let running_example gas widening =
  let log =
    sequence [
        mkApply("src_table"
            , [("src", 2)]
            , [ ["s",2], "smac" %<-% (Var("s",2))
              ; ["d",2], "dst" %<-% (Var("d",2))
              ; [], Skip
              ]
            , Skip)
      ; mkApply("dst_table"
            , [("dst",2)]
            , [ ["p",2], "out" %<-% (Var("p",2))
              ; [], Skip
              ]
            ,Skip)
      ] in
  let phys =
        mkApply("src_dst_table"
            , ["src",2; "dst", 2]
            , [ ["s",2], "smac" %<-% Var("s",2)
              ; ["d",2], "dst" %<-% (Var("d",2))
              ; ["o",2], "out" %<-% (Var("o",2))
              ; ["s",2; "o", 2], ("smac" %<-% Var("s",2)) %:% ("out" %<-% Var("o",2))
              ; ["d",2; "p", 2], ("dst" %<-% Var("d",2)) %:% ("out" %<-% Var("p",2))
              ; [], Skip
              ]
            , Skip)
  in
  let params = Parameters.({default with widening}) in
  let problem  =
    Problem.make ~log ~phys
      ~fvs:["src", 2; "dst", 2; "smac", 2; "dmac", 2; "out", 2 ]
      ~log_inst:StringMap.(of_alist_exn
                             [("src_table", [([Match.Exact (mkInt(0,2))], [mkInt(1,2)], 0)
                                            ;([Match.Exact (mkInt(1,2))], [mkInt(2,2)], 1)])
                             ; ("dst_table", [([Match.Exact (mkInt(0,2))], [mkInt(1,2)], 0)])])
      ~log_edits:[Add ("dst_table", ([Exact (mkInt(1,2))], [mkInt(2,2)], 0))]
      ~phys_inst:StringMap.empty
      () in
  synthesize ~iter:1
    params
    None
    (ProfData.zero ())
    problem
      
                                      
            



let onf_representative gas widening =
  let fwd_type_i i  = mkVInt(i, 2) in
  let fwd_type_v = Var("fwd_type", 2) in
  let fwd_bridge = fwd_type_i 0 in
  let fwd_mpls = fwd_type_i 1 in
  let fwd_ipv4 = fwd_type_i 2 in
  let station =
    mkApply("my_station_table"
        , [("inport", 9); ("dmac", 48); ("eth_type", 16)]
        , [["sf", 2], ("fwd_type" %<-% Var("sf", 2))]
        , "fwd_type" %<-% fwd_bridge)
  in
  let bridge =
    mkApply("bridge_table"
        , [("vlan_id", 12); ("dmac", 48)]
        , [[("bv",32)], "next_id"%<-% Var("bv",32)]
        , Skip)
  in
  let mpls =
    mkApply("mpls_table"
        , [("mpls_label", 20)]
        , [["mv",32], ("mpls_label"%<-% mkVInt(0,20)) %:% ("next_id"%<-% Var("mv", 32))]
        ,Skip)
  in
  let ipv4 =
    mkApply("ipv4_dst"
        , [("ivp4_dst", 32)]
        , [["iv",32], "next_id" %<-% Var("iv", 32)]
        , Skip)
  in
  let set_next =
    mkOrdered [
        fwd_type_v %=% fwd_ipv4, ipv4
      ; fwd_type_v %=% fwd_mpls, mpls
      ; fwd_type_v %=% fwd_bridge, bridge
      ]
  in
  let acl = 
    mkApply ("acl"
         , ["in_port", 9; "ip_proto", 8;
            "l4_src", 16; "l4_dst", 16;
            "dmac", 48 ; "smac", 48;
            "eth_type", 16; "ipv4_src", 32;
            "icmp_type", 16; "icmp_code", 16
           ]
         , [ ["av", 32], "next_id" %<-% Var("av",32)
           ; [], "out_port"%<-% mkVInt(255, 9)
           ; [], ("drop"%<-% mkVInt(1,1)) %:% ("skip_next" %<-% mkVInt(1,1))
           ; [], Skip ]
         , Skip
      ) in
  let next =
    mkOrdered [ Var("skip_next",1) %=% mkVInt(1,1),
                mkApply("next"
                    , ["next_id", 32]
                    , [["nv",9], "out_port" %<-% Var("nv",9)
                      ; ["nvv", 9; "nvs", 48; "nvd", 48], sequence [
                                                              "out_port" %<-% Var("nvv",9)
                                                            ; "smac" %<-% Var("nvs", 48)
                                                            ; "dmac" %<-% Var("nvd", 48)]
                                                                   
                      ; ["nvvv", 9; "nvvs", 48; "nvvd", 48; "nvvm", 20],
                        sequence [
                            "mpls_label" %<-% Var("nvvm", 20)
                          ; "out_port" %<-% Var("nvvv",9)
                          ; "smac" %<-% Var("nvvs", 48)
                          ; "dmac" %<-% Var("nvvd", 48)
                      ]]
                    , Skip)
              ; True, Skip]
  in
  let fabric_egress =
    sequence [ Skip
               (* mkOrdered [ Var("in_port",9) %=% Var("out_port", 9), "drop"%<-% mkVInt(1,1)
                *           ; True, Skip ]; *)
               (* mkordered [
                *     (fwd_type_v %=% fwd_ipv4) %+% (fwd_type_v %=% fwd_mpls),
                *     sequence [ "ttl" %<-% Minus(Var("ttl", 8), mkVInt(1,8))
                *              ; mkOrdered [Var("ttl", 8) %=% mkVInt(0,8), "drop" %<-% mkVInt(1,1)
                *                         ; True, Skip]]
                *   ; True, Skip
                *   ] *)
      ]
  in
  let logical = sequence [station; set_next; acl; next; fabric_egress] in
  let admit =
    mkApply("admit"
        , ["dmac", 48]
        , [[], "l3_admit" %<-% mkVInt(1,1)]
        , "l3_admit" %<-% mkVInt(0,1))
  in
  let l2 =
    mkApply("l2"
        , ["dmac", 48]
        , [["out_port", 9], "out_port" %<-% Var("out_port",9)]
        , Skip) in
  let l3 =
    mkApply ("l3"
         , ["dmac", 48; "smac", 48; "ip_proto", 16; "l4_src", 16; "l4_dst", 16; (*"ttl", 8*)]
         , [["op", 9; "smac", 48; "dmac", 48;],
            sequence [
                "out_port" %<-% Var("op", 9)
              ; "smac" %<-% Var("smac", 48)
              ; "dmac" %<-% Var("dmac", 48)
                               (* ; "ttl" %<-% Minus(Var("ttl",8), mkVInt(1,8)) *)
              ]
           ; [], Skip
           ; [], "drop" %<-% mkVInt(1,1)]
         , Skip )
  in
  let l2_or_l3 =
    mkOrdered[
        Var("l3_admit",1) %=% mkVInt(0,1), l3;
        Var("l3_admit",1) %=% mkVInt(1,1), l2
      ] in
  let punt =
    mkApply("punt"
        , ["in_port", 9; "out_port", 9; "eth_type", 16
           ; "ipv4_src", 32; "ip_proto", 16; "icmp_code", 16
           ; "vlan_id", 12
          ] (* ip_diffserve vlan[0].vid, vlan[0].pcp, class_id, vrf_id *)
        , [ [], "out_punt" %<-% Var("out_port",9)
          ; [], sequence [
                    "out_punt" %<-% Var("out_port", 9)
                  ; "out_port" %<-% mkVInt(255, 9)
                  ]
          ; ["vpt", 9], sequence [
                            "out_punt" %<-% Var("out_port", 9)
                          ; "out_port" %<-% Var("vpt", 9)
          ]]
        , Skip  ) in
  let physical = sequence [admit; l2_or_l3; punt] in
  let fvs =
    [ "in_port", 9
    ; "out_port", 9
    ; "dmac", 48
    ; "smac", 48
    ; "eth_type", 16
    ; "mpls_label", 20
    ; "ipv4_src", 32
    ; "ipv4_dst", 32
    ; "ip_proto", 16
    (* ; "ttl", 8 *)
    ; "l4_src", 16
    ; "l4_dst", 16
    ; "icmp_type", 16
    ; "icmp_code", 16
    ; "drop", 1
    ; "vlan_id", 20
    ]
  in
  let init_metadata =
    sequence [
        "out_port" %<-% Var("in_port", 9)
      ; "drop" %<-% mkVInt(0,1)
      ; "skip_next" %<-% mkVInt(0,1)
      ]
  in
  let params = Parameters.({default with widening}) in
  let problem =
    Problem.make
      ~log:(init_metadata %:% logical)
      ~phys:(init_metadata %:% physical)
      ~fvs
      ~log_inst:StringMap.(of_alist_exn
                             [ ("my_station_table", [ ([Match.Between(mkInt(0,9), mkInt(pow 2 9,9));
                                                        Match.Between(mkInt(0,48), mkInt(pow 2 48,48));
                                                        Match.Between(mkInt(0, 16), mkInt(pow 2 16, 16))], [mkInt(2, 2)], 0 ) ])
                             ; ("ipv4_dst", [([Match.Exact(mkInt(1, 32))], [mkInt(1,32)], 0)])
                             ])
      ~log_edits:[Add("next", ([Match.Exact(mkInt(1,32))], [mkInt(1,9)], 0))]
      ~phys_inst:Instance.empty
      ()
  in
  synthesize ~iter:1
    params
    None
    (ProfData.zero ())
    problem




(**** BEGIN CLASSBENCH ***)

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
  let mask_idx =
    let f_idx = float_of_string(mask) /. 4.0 in
    if Float.round_up f_idx = Float.round_down f_idx then
      int_of_float f_idx
    else failwith ("unknown mask " ^ mask)
  in
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
    Match.Between(mkInt(lo_int, 32), mkInt(hi_int, 32))
  
    
let parse_port_range str =
  let lo,hi = String.lsplit2_exn str ~on:':' in
  let () = Printf.printf "(%s:%s)\n%!" lo hi in
  let lo_int = String.strip lo |> int_of_string in
  let hi_int = String.strip hi |> int_of_string in
  if lo = hi
  then Match.Exact(mkInt(lo_int, 9))
  else Match.Between(mkInt(lo_int, 9), mkInt(hi_int, 9))

let parse_proto str =
  let proto,_ = String.lsplit2_exn str ~on:'/' in
  Match.Exact(Int(Bigint.of_string proto, 8))
              
let generate_edits cb_rules =
  let drop_table = List.fold (pow 2 9 |> range_ex 0) ~init:IntMap.empty
                     ~f:(fun acc port -> IntMap.set acc ~key:port ~data:(Random.int 2)) in
  List.fold cb_rules ~init:[]
    ~f:(fun acc (_, ip_dst, _,_,_) ->
      let out_port = Random.int (pow 2 9) in
      acc @ [
          IntMap.fold drop_table ~init:[]
            ~f:(fun ~key ~data acc ->
                acc @ [("of", ([ip_dst; Match.Exact(mkInt(key, 9))], [mkInt(out_port, 9)], data))]
            )
        ]
    )

let generate_pipe1_edits cb_rules : Edit.t list list =
  let drop_table = List.fold (pow 2 9 |> range_ex 0) ~init:IntMap.empty
                     ~f:(fun acc port -> IntMap.set acc ~key:port ~data:(Random.int 2)) in
  let ip_table =
    List.fold cb_rules ~init:[]
    ~f:(fun acc (_, ip_dst, _,_,_) ->
      let out_port = Random.int (pow 2 9) in
      acc
      @ (if Random.int 2 = 0
          then []
          else let pt = Random.int (pow 2 9) in
               [[Tables.Edit.Add ("ingress", ([Match.Exact(mkInt(pt,9))], [], IntMap.find_exn drop_table pt))]]
        )
      @ [[Tables.Edit.Add ("ipv4_fwd", ([ip_dst], [mkInt(out_port, 9)], 0))]]
    )
  in
  ip_table

let restart_timer (params : Parameters.t) st =
  match params.timeout with
  | None -> params
  | Some (_,dur) -> {params with timeout = Some(st,dur)}

let mk_ith_meta i = Printf.sprintf "m%d" i
let mk_ith_var i =  Printf.sprintf "x%d" i


let mk_normal_keys sz num_xs =
  List.map (range_ex 0 (num_xs+1)) ~f:(fun i ->
      mk_ith_var i, sz)

let mk_ith_keys sz num_xs ith_meta =
  List.map (range_ex 0 (num_xs+1))
    ~f:(fun i ->
      if i <= ith_meta then
        if ith_meta > num_xs then
          (mk_ith_meta ((ith_meta - num_xs) + i), sz)
        else
          (mk_ith_meta i, sz)
      else
        (mk_ith_var i,sz)
    )


let mk_ith_table sz num_tables tbl_idx num_xs num_ms =
  let idx_of_min_mtbl = num_tables - num_ms -1 in
  mkApply(Printf.sprintf "physical%d" tbl_idx,
          (if tbl_idx > idx_of_min_mtbl then
             mk_ith_keys sz num_xs (tbl_idx - idx_of_min_mtbl - 1)
          else
            mk_normal_keys sz num_xs),
          (if tbl_idx >= num_tables - num_ms - 1 && tbl_idx < num_tables - 1
           then [[Printf.sprintf "d%i" tbl_idx, sz], mk_ith_meta (tbl_idx - idx_of_min_mtbl) %<-% Var(Printf.sprintf "d%i" tbl_idx, sz)]
           else [[Printf.sprintf "d%i" tbl_idx, 9], "out" %<-% Var(Printf.sprintf "d%i" tbl_idx, 9)]),
          Skip)



let initialize_ms sz num_ms =
  if num_ms = 0 then Skip else
  List.map (range_ex 0 num_ms)
    ~f:(fun i -> mk_ith_meta i %<-% mkVInt (0,sz))
  |> sequence


let create_bench sz num_tables num_xs num_ms =
  sequence [ initialize_ms sz num_ms
           ; sequence @@
               List.map (range_ex 0 num_tables)
                 ~f:(fun tbl_idx ->
                   mk_ith_table sz num_tables tbl_idx num_xs num_ms
                 )
    ]


let match_row sz num_xs ~ith ~has_value =
  let open Match in
  let wildcard =  Mask(mkInt(0,32),mkInt(0,32)) in
  let matches =
    Util.repeat (ith) wildcard
    @ [ Exact(mkInt(has_value,sz)) ]
    @ Util.repeat (num_xs - ith) wildcard
  in
  Edit.Add("logical",(matches, [mkInt(has_value,9)], 0))

let match_row_easier sz num_xs ~has_value =
  let open Match in
  let wildcard =  Mask(mkInt(0,32),mkInt(0,32)) in
  let matches =
    Exact(mkInt(has_value,sz)) :: Util.repeat (num_xs) wildcard
  in
  Edit.Add("logical",(matches, [mkInt(has_value,9)], 0))


let rec create_log_edits_easier sz i max_edits num_xs=
  if i = max_edits then []
  else
    match_row_easier sz num_xs ~has_value:i
    :: create_log_edits_easier sz (i + 1) max_edits num_xs


let rec create_log_edits sz i max_edits num_xs =
  if i = max_edits then []
  else
    match_row sz num_xs ~ith:(i mod num_xs) ~has_value:(i / num_xs)
    :: create_log_edits sz (i + 1) max_edits num_xs

let square_bench params sz n max_edits =
  let fvs = ("out",9) :: mk_normal_keys sz n in
  let logical_table =
    mkApply("logical",
            mk_normal_keys sz n,
            [["o", 9], "out" %<-% Var("o", 9)],
            Skip)
  in
  Printf.printf "Logical table: \n %s\n\n" (string_of_cmd logical_table);
  let physical_tables =
    List.fold (range_ex 0 n) ~init:[]
    ~f:(fun init num_xs ->
      List.fold(range_ex 0 n) ~init
        ~f:(fun acc num_ms ->
          let p = create_bench sz n num_xs num_ms in
          Printf.printf "\n\n-------%d---%d----------\n%s\n--------------\n\n%!"num_xs num_ms (string_of_cmd p);
          acc @ [(num_xs, num_ms), p]))
  in
  let log_edits =
    create_log_edits_easier 32 0 max_edits n |> List.map ~f:(List.return)
  in
  let problem phys = Problem.make ~log:logical_table ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  List.fold physical_tables ~init:"numxs,num_ms,time"
    ~f:(fun acc ((xs,ms),phys) ->
      Printf.printf "\n------%d,%d-----\n" xs ms;
      (* if xs = 10 && ms = 8 then acc else *)
      Synthesis.edit_cache := EAbstr.make ();
      let st = Time.now () in
      let es = measure (restart_timer params st) None (problem phys) log_edits in
      let nd = Time.now () in
      let dur = Time.diff nd st in
      match es with
      | None ->
         Printf.sprintf "%s\n%d,%d,TIMEOUT" acc xs ms
      | Some _ ->
         Printf.sprintf "%s\n%d,%d,%f" acc xs ms (Time.Span.to_ms dur)
    )
  |> Printf.printf "%s\n"



(****************************************
 *     Representative pipeline          *
 ****************************************)

let cb_to_matches table fvs cb_row =
  List.map fvs ~f:(fun (f,sz) ->
                    get cb_row f
                    |> Option.value ~default:(Mask(mkInt(0,sz),mkInt(0,sz))))

let generate_out acc cb_row  =
  let open Edit in
  let biggest = List.fold acc ~init:Bigint.one ~f:(fun max_so_far curr ->
                    match curr with
                    | Add(_,(_,[Int(i,sz)],_)) when Bigint.(i > max_so_far) -> i
                    | _ -> max_so_far
                  ) in
  Bigint.(biggest + one)

  
let rep params data =
  let fvs = ["ip_src",32; "ip_dst",32] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches "logical" fvs) in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> List.(ms = matches)
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then acc
           else
             let outp = generate_out acc cb_row in
             acc @ [Add("obt", (matches, [Int(outp,9)], 0))]
         )
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data);
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["ip_src",32;"ip_dst",32],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply("validation",
                ["ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["ip_dst",32],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["ip_src",32; "ip_dst",32],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))


let rep_middle params data =
  let fvs = ["ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches "logical" fvs) in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> List.(ms = matches)
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then acc
           else
             let outp = generate_out acc cb_row in
             acc @ [Add("obt", (matches, [Int(outp,9)], 0))]
         )
  in
  Printf.printf "there are cleaned rules %d\n%!" (List.length gen_data);
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["ip_src",32;"ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        (* mkOrdered [
         *     Var("proto",8) %=% mkVInt(6,8),
         *     sequence [
         *         "tcp_sport" %<-% mkVInt(0,16);
         *         "tcp_dport" %<-% mkVInt(0,16)
         *       ];
         *     True , Skip
         *   ]; *)
        mkApply("validation",
                ["ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["ip_dst",32; "proto",8; "tcp_dport",16],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["ip_src",32; "ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))



let rep_of params data =
  let fvs = ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16] in
  let cb_fvs = List.map ~f:fst fvs in
  let gen_data : Edit.t list =
    let cb_rows = parse_classbench_of data in
    Printf.printf "There are %d rows \n%!" (List.length cb_rows);
    List.fold cb_rows ~init:([] : Edit.t list)
         ~f:(fun acc cb_row ->
           let open Edit in
           let fields = cb_fvs in
           let matches = project cb_row fields |> (cb_to_matches "logical" fvs) in
           if List.exists acc ~f:(function
                  | Add (_,(ms,_,_)) -> List.(ms = matches)
                  | _ -> false)
              || List.for_all matches ~f:(function
                     | Mask(_,m) -> Bigint.(get_int m = zero)
                     | _ -> false)
           then
             let () = Printf.printf "\tthrowing out %s\n" (Edit.to_string (Add("obt",(matches, [], -1)))) in
             acc
           else
             let outp = generate_out acc cb_row in
             let e =  Add("obt", (matches, [Int(outp,9)], 0)) in
             let () = Printf.printf "Keeping %s\n" (Edit.to_string e) in
             acc @ [e]
         )
  in
  Printf.printf "there are %d cleaned rules\n%!" (List.length gen_data);
  let fvs = ("out", 9) :: fvs in
  let log =
    sequence [
        "out" %<-% mkVInt(0,9);
        mkApply ("obt",
                 ["in_port",9;"eth_src",48;"eth_dst",48;"eth_typ",16;"ip_src",32; "ip_dst",32; "proto",8; "tcp_sport",16; "tcp_dport",16],
                 [["o",9], "out" %<-% Var("o",9)],
                 Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let phys =
    sequence [
        "out" %<-% mkVInt(0,9);
        (* mkOrdered [
         *     Var("proto",8) %=% mkVInt(6,8),
         *     sequence [
         *         "tcp_sport" %<-% mkVInt(0,16);
         *         "tcp_dport" %<-% mkVInt(0,16)
         *       ];
         *     True , Skip
         *   ]; *)
        mkApply("validation",
                ["in_port", 9;"eth_src",48;"eth_typ",16;"ip_src",32],
                [["ov",9], "out" %<-% Var("ov",9)],
                Skip) ;
        mkApply("fwd",
                ["eth_dst",48;"ip_dst",32; "proto",8; "tcp_dport",16],
                [["of",9], "out" %<-% Var("of",9) ],
                Skip);
        mkApply("acl",
                ["in_port", 9;"eth_src",48; "eth_dst",48; "eth_typ",16; "ip_src",32; "ip_dst",32; "proto",8; "tcp_sport", 16; "tcp_dport", 16],
                [["oa",9], "out" %<-% Var("oa", 9)],
                Skip);
        mkOrdered [
            Var("out",9) %=% mkVInt(0,9),
            List.fold fvs ~init:Skip ~f:(fun acc (fv,sz) ->
                acc %:% (fv %<-% mkVInt(0,sz))
              );
            True, Skip
          ]
      ]
  in
  let problem = Problem.make ~log:log ~phys ~fvs ~log_edits:[] ~log_inst:Instance.empty ~phys_inst:Instance.empty () in
  measure (restart_timer params (Time.now())) None problem (List.(gen_data >>| return))
