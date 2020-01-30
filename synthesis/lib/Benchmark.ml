open Core
open Ast
open Synthesis
open Util
open Packet

module IntMap = Map.Make(Int)
  
                        
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
    , [(["v",varsize],("x_"^tbl n) %<-% Var1("v",varsize))]
    , ("x_"^tbl n) %<-% mkVInt (0,varsize)
    ) :: mk_pipeline varsize (n-1)


let rec generate_n_insertions varsize length n avail_tables maxes =
  if n = 0 then
    let _ = Printf.printf "--generated--\n%!"in
    []
  else if avail_tables = [] then
    let _ = Printf.printf "--filled up --\n%!" in
    []
  else
    let _ = Printf.printf "generating %d \n%!" n in
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
              (max_i + 1, Exact (max_i, varsize))
            else
              let lo = max_i in
              let hi = min (lo + Random.int 3) (pow 2 varsize - 1) in
              if lo = hi then
                (hi + 1, Exact (hi, varsize))
              else
                (hi + 1, Between (lo, hi, varsize))
          in
          let maxes' = StringMap.set maxes ~key:(tbl i) ~data:max' in
          let act_data = (Random.int (pow 2 varsize),varsize) in
          let row = ([mtch], [act_data], 0) in
          Some (maxes', avail_tables, tbl i, row)
    in
    match loop_free_match avail_tables with
    | None ->
       let _ = Printf.printf "--filled up --\n%!" in
       []
    | Some (maxes', avail_tables', name, row) ->
       let _ = Printf.printf "Inserting\n%!" in
       (name, row)
       :: generate_n_insertions varsize length (n-1) avail_tables' maxes'
                                  
let reorder_benchmark varsize length max_inserts widening =
  Random.init 99;
  let logical_pipeline = mk_pipeline varsize length in
  let physical_pipeline = permute logical_pipeline in
  let mk_empty_inst acc (name,_,_,_) = Map.set acc ~key:name ~data:[]  in
  let linst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let pinst = List.fold logical_pipeline ~init:StringMap.empty ~f:mk_empty_inst in
  let to_cmd line =  List.((line >>| fun t -> Apply t)
                           |> reduce_exn ~f:(%:%)) in
  (* let hints = Some(fun vMap -> (\*[vMap]*\)
   *                 [List.fold ~init:vMap (range_ex 1 (length + 8))
   *                   ~f:(fun acc i ->
   *                     StringMap.set acc ~key:("?AddRowTo" ^ tbl i)
   *                       ~data:(mkVInt(1,1))
   *                 )]
   *               ) in *)
  let hints = Some (List.return) in
  (* let hints = None in *)
  let log = to_cmd logical_pipeline in
  let phys = to_cmd physical_pipeline in
  let insertion_sequence = 
    generate_n_insertions varsize length max_inserts (range_ex 1 (length +1)) StringMap.empty
  in
  let fvs = range_ex 1 (length + 1)
            |> List.map ~f:(fun i ->
                   [("k_" ^tbl i, 32)
                   ; ("x_" ^tbl i, 32)
                       (* ; ("?ActIn"^tbl i, 8) *)
                 ])
            |> List.join
  in
  let rec run_experiment i seq linst pinst =
    match seq with
    | [] -> []
    | edit::edits ->
       Printf.printf "==== BENCHMARKING INSERTION OF (%s) =====\n%!"
         (string_of_edit edit);
       let (totalt,checkt,checkn, searcht, searchn, wpt,lwpt,pwpt,sizes,pinst')  =
         synthesize_edit ~widening ~gas:5 ~fvs ~hints ~iter:i (Prover.solver ()) log phys linst pinst (Some edit) in
       Printf.printf "=== DONE=================================\n%!";
       (i, totalt, checkt, checkn, searcht, searchn, wpt,lwpt,pwpt, sizes)
       :: run_experiment (i + 1) edits (apply_edit linst edit) pinst'
  in
  let data = run_experiment 0 insertion_sequence linst pinst in
  let mean ds = List.fold ds ~init:0 ~f:((+)) / List.length ds in
  let max_l ds = List.fold ds ~init:0 ~f:(max) in
  let min_l ds = List.fold ds ~init:(max_l ds) ~f:(min) in
  Printf.printf "size,time,check_time,num_z3_calls_check,model_search_z3_time,num_z3_calls_model_search,search_wp_time,check_log_wp_time,check_phys_wp_time,mean_tree_size,max_tree_size,min_tree_size\n";
  List.iter data ~f:(fun (i,t,c,cn,s,sn,wpt,lwpt, pwpt,sizes) ->
      Printf.printf "%d,%f,%f,%d,%f,%d,%f,%f,%f,%d,%d,%d\n"
        i (Time.Span.to_ms t)
        (Time.Span.to_ms c) cn
        (Time.Span.to_ms s) sn
        (Time.Span.to_ms wpt)
        (Time.Span.to_ms lwpt)
        (Time.Span.to_ms pwpt)
        (mean sizes)
        (max_l sizes)
        (min_l sizes)
    )

            
(** ONF BENCHMARK **)
   
let sequence = List.reduce_exn ~f:(%:%)
let set_valid s = (s ^ ".valid") %<-% mkVInt(1,1)
let set_invalid s = (s ^ ".valid") %<-% mkVInt(0,1)
let is_valid s = Var1(s^".valid",1) %=% mkVInt(1,1)
let std_meta_in_port_str = "standard_metadata.ingress_port"
let std_meta_in_port = Var1(std_meta_in_port_str,9)
let cpu_port = mkVInt(255,9)
let hdr_packet_out_str = "hdr.packet_out"
let hdr_ethernet_str = "hdr.ethernet"
let fabric_metadata_vlan_id_str = "fabric_metadata_vlan_id"
let default_vlan_id = mkVInt(4094, 12)
let hdr_eth_type_str = "hdr.eth_type"
let hdr_eth_type_value_str = "hdr.eth_type.value"
let hdr_eth_type_value = Var1(hdr_eth_type_value_str, 16)
let ethertype_mpls = mkVInt(34887,16)
let hdr_mpls_str = "hdr_mpls_str"
let fabric_metadata_mpls_label_str = "fabric_metadata.mpls_label_str"
let fabric_metadata_mpls_ttl_str = "fabric_metadata.mpls.ttl"
let hdr_mpls_ttl_str = "hdr.mpls.ttl"
let hdr_mpls_ttl = Var1(hdr_mpls_ttl_str, 8)
(* let fabric_metadata_mpls_label = Var1(fabric_metadata_mpls_label_str, 20) *)
let hdr_mpls_label_str = "hdr.mpls.label"
let hdr_mpls_label = Var1(hdr_mpls_label_str, 20)
let hdr_ipv4_str = "hdr.ipv4"
let fabric_metadata_ip_proto_str = "fabric_metadata.ip_proto"
let fabric_metadata_ip_eth_typ_str = "fabric_metadata.ip_eth_typ"
let hdr_ipv4_protocol_str = "hdr.ipv4.protocol"
let hdr_ipv4_protocol = Var1(hdr_ipv4_protocol_str, 8)
let ethertype_ipv4 = mkVInt(2048, 16)
let proto_tcp = mkVInt(6,8)
let proto_udp = mkVInt(17,8)
let proto_icmp = mkVInt(1,8)
let hdr_tcp_str = "hdr.tcp"
let fabric_metadata_l4_sport_str = "fabric_metadata.l4_sport"
let fabric_metadata_l4_dport_str = "fabric_metadata.l4_dport"  
let hdr_tcp_sport_str = "hdr.tcp.sport"
let hdr_tcp_sport = Var1(hdr_tcp_sport_str, 16)
let hdr_tcp_dport_str = "hdr.tcp.dport"
let hdr_tcp_dport = Var1(hdr_tcp_dport_str, 16)
let hdr_udp_str = "hdr.udp"
let hdr_udp_sport_str = "hdr.udp.sport"
let hdr_udp_sport = Var1(hdr_udp_sport_str, 16)
let hdr_udp_dport_str = "hdr.udp.dport"
let hdr_udp_dport = Var1(hdr_udp_dport_str, 16)
let hdr_icmp_str = "hdr.icmp"
                     
let hdr_packet_out_str = "hdr_packet_out_str"
let standard_metadata_egress_spec_str = "standard_metadata.egress_spec"
let standard_metadata_egress_spec = Var1(standard_metadata_egress_spec_str, 9)
let hdr_packet_out_egress_port_str = "hdr.packet_out.egress_port"
let hdr_packet_out_egress_port = Var1(hdr_packet_out_egress_port_str, 9)
let fabric_metadata_is_controller_packet_out_str = "fabric_metadata.is_controller_packet_out"
let default_mpls_ttl = mkVInt(64,8)
let fabric_metadata_fwd_type_str = "fabric_metadata.fwd_type"
let fabric_metadata_fwd_type = Var1(fabric_metadata_fwd_type_str, 3)
let fwd_bridging = mkVInt(0,3)
let fwd_mpls = mkVInt(1,3)
let fwd_ipv4_unicast = mkVInt(2,3)
let fabric_metadata_skip_next_str = "fabric_metadata.skip_next"
let fabric_metadata_skip_next = Var1(fabric_metadata_skip_next_str, 1)
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
let fabric_metadata_is_multicast = Var1(fabric_metadata_is_multicast_str, 1) 
let standard_metadata_ingress_port = Var1(standard_metadata_ingress_port_str, 9) 
let standard_metadata_egress_port_str = "standard_metadata.egress_port" 
let standard_metadata_egress_port = Var1(standard_metadata_egress_port_str, 9) 
let hdr_mpls_str = "hdr.mpls" 
let hdr_mpls_tc_str = "hdr.mpls.tc" 
let hdr_mpls_bos_str = "hdr.mpls.bos" 
let hdr_mpls_ttl_str = "hdr.mpls.ttl" 
let hdr_mpls_ttl = Var1(hdr_mpls_ttl_str, 8) 
let fabric_metadata_mpls_label = Var1(fabric_metadata_mpls_label_str, 20) 
let fabric_metadata_ip_eth_typ = Var1(fabric_metadata_ip_eth_typ_str, 16)
let fabric_metadata_mpls_ttl = Var1(fabric_metadata_mpls_ttl_str, 8) 
let hdr_ipv4_ttl_str = "hdr.ipv4.ttl" 
let hdr_ipv4_ttl = Var1(hdr_ipv4_ttl_str, 8)
let fabric_metadata_skip_forwarding_str = "fabric_metadata.skip_forwarding"
let fabric_metadata_skip_forwarding = Var1(fabric_metadata_skip_forwarding_str, 1)                       
let fabric_metadata_is_controller_packet_out_str = "fabric_metadata.is_controller_packet_out"
let fabric_metadata_is_controller_packet_out = Var1(fabric_metadata_is_controller_packet_out_str, 1)
let hdr_packet_in = "hdr.packet_in"
let hdr_packet_in_ingress_port_str = "hdr.packet_in.ingress_port"
let hdr_packet_in_egress_port_str = "hdr.packet_in.egress_port"
let loopback_port = mkVInt(13,9)
let local_metadata_l3_admit_str = "local_metadata_l3_admit"
let local_metadata_l3_admit = Var1(local_metadata_l3_admit_str, 1)
let local_metadata_egress_spec_at_punt_match = "local_metadata.egress_spec_at_punt_match"
let local_metadata_egress_spec_at_punt_match = Var1(local_metadata_egress_spec_at_punt_match, 9)



let fwd_classifier_table =
  Apply(
      "fwd_classifier"
    , [ (standard_metadata_ingress_port_str, 9 (*exact*))
      ; (hdr_ethernet_dst_addr_str, 48 (*ternary*))
      ; (hdr_eth_type_value_str, 16 (*ternary*))
      ; (fabric_metadata_ip_eth_typ_str, 16 (*exact*))]
    , [ ([("fwd_type",3)], fabric_metadata_fwd_type_str %<-% Var1("fwd_type", 3))]
    , fabric_metadata_fwd_type_str %<-% fwd_bridging)
let bridging_table =
  Apply("bridging"
      , [ (fabric_metadata_vlan_id_str, 12 (*exact*))
        ; (hdr_ethernet_dst_addr_str, 48 (*ternary*))
        ]
      , [([("next_id",32)], fabric_metadata_next_id_str %<-% Var1("next_id",32))]
      , Skip)
       
let mpls_table =
  Apply("mpls"
      , [ (fabric_metadata_mpls_label_str, 20 (* exact *) ) ]
      , [ ([("next_id",32)],  fabric_metadata_next_id_str %<-% Var1("next_id", 32)) ]
      , Skip )
let ipv4_tbl =
  Apply("ipv4"
      , [ (hdr_ipv4_dst_addr_str, 20 (*exact *))]
      , [ ([("next_id",32)],  fabric_metadata_next_id_str %<-% Var1("next_id", 32)) ]
      , Skip)
       
let acl_table = Apply("acl",
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
                      ,[ ([("next_id",32)], fabric_metadata_next_id_str %<-% Var1("next_id", 32))
                       ; ([], sequence [ standard_metadata_egress_spec_str %<-% cpu_port
                                       ; fabric_metadata_skip_next_str %<-% mkVInt(1,1)])
                       ; ([], sequence [mark_to_drop; fabric_metadata_skip_next_str %<-% mkVInt(1,1)])
                       ; ([], Skip)
                       ] 
                      , fabric_metadata_fwd_type_str %<-% fwd_bridging)
                     

let apply_simple_next =
  Apply("next.simple"
      , [(fabric_metadata_next_id_str, 32 (*exact*))]
      , [ ([("port",9)], standard_metadata_egress_spec_str %<-% Var1("port", 9))
        ; ([("port",9);("smac",48);("dmac",48)], sequence [ hdr_ethernet_src_addr_str %<-% Var1("smac", 48)
                                            ; hdr_ethernet_dst_addr_str %<-% Var1("dmac", 48)
                                            ; standard_metadata_egress_spec_str %<-% Var1("port", 8)])
        ; ([("label",20);("port",9);("smac",48);("dmac",48)],
           sequence [ fabric_metadata_mpls_label_str %<-% Var1("label", 20)
                    ; hdr_ethernet_src_addr_str %<-% Var1("smac", 48)
                    ; hdr_ethernet_dst_addr_str %<-% Var1("dmac", 48)
                    ; standard_metadata_egress_spec_str %<-% Var1("port",8)])
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
  Apply("my_station_table"
      , [(hdr_ethernet_dst_addr_str, 48 (*ternary*))]
      , [ ( [] , local_metadata_l3_admit_str %<-% mkVInt(1,1) )
        ; ( [], Skip )]
      , Skip)

let l3_fwd =
  Apply("l3_fwd"
      , [(hdr_ipv4_dst_addr_str, 32 (*lpm*))
        ; (hdr_ipv4_src_addr_str, 32 (*selector*))
        ; (hdr_ipv4_protocol_str, 8 (*selector*))
        ; (fabric_metadata_l4_sport_str, 16 (*selector*))
        ]
      , [ [("port",9); ("smac",48); ("dmac",48); (*"dst_vlan",*)],
          sequence [
              standard_metadata_egress_spec_str %<-% Var1("port", 9)
              (*            ; local_metadata_dst_vlan %<-% Var1("dst_vlan", ??)*)
            ; hdr_ethernet_src_addr_str %<-% Var1("smac", 48)
            ; hdr_ethernet_dst_addr_str %<-% Var1("dmac", 48)
            ; hdr_ipv4_ttl_str %<-% Minus(hdr_ipv4_ttl, mkVInt(1,8))
            ]
        ]
      , Skip)

let l2_unicast_table =
  Apply("l2_unicast_table"
      , [(hdr_ethernet_dst_addr_str, 48 (*exact*))]
      , [([("port",9)], standard_metadata_egress_spec_str %<-% Var1("port", 9) )]
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
  (* let punt_table = Apply("punt", [], [], Skip) in *)
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


let basic_onf_ipv4 _ = 
  let logical =
    sequence [
        "class_id" %<-% mkVInt(0,32)
      ; Apply("ipv4",
              [("ipv4_dst", 32)],
              [([("next_id",32)], "class_id"%<-% Var1("next_id",32))],
              Skip)
      ; Apply("next",
              [("class_id", 32)],
              [([("port",9)], "out_port"%<-% Var1("port",9))],
              Skip)
      ] in
  let physical =
    Apply("l3_fwd"
        , [ ("ipv4_dst", 32) (*; ("ipv4_src", 32); ("ipv4_proto", 16)*) ]
        , [ ([("port",9)], "out_port"%<-% Var1("port",9))]
        , Skip) in
  let gas = 2 in
  let iter = 1 in
  let p = Prover.solver in
  let fvs = [("ipv4_dst", 32); ("out_port", 32); (*("ipv4_src", 32); ("ipv4_proto", 16)*)] in
  (* synthesize_edit  ~gas ~iter ~fvs p 
   *   logical
   *   physical
   *   StringMap.empty
   *   StringMap.empty
   *   ("next", ([Exact (1,32)], [(1,9)],0))
   * %> *)
    synthesize_edit ~gas ~iter ~fvs p
      logical
      physical
      StringMap.(set empty ~key:"next" ~data:[[Exact (1,32)], [(1,9)],0])
      StringMap.(set empty ~key:"l3_fwd" ~data:[])
      (Some ("ipv4", ( [Between (0,20,32)], [(1,32)],0)))


let running_example gas widening =
  let logical =
    sequence [
        Apply("src_table"
            , [("src", 2)]
            , [ ["s",2], "smac" %<-% (Var1("s",2))
              ; ["d",2], "dst" %<-% (Var1("d",2))
              ; [], Skip
              ]
            , Skip)
      ; Apply("dst_table"
            , [("dst",2)]
            , [ ["p",2], "out" %<-% (Var1("p",2))
              ; [], Skip
              ]
            ,Skip)
      ] in
  let physical =
        Apply("src_dst_table"
            , ["src",2; "dst", 2]
            , [["s",2], "smac" %<-% Var1("s",2)
              ; ["d",2], "dst" %<-% (Var1("d",2))
              ; ["o",2], "out" %<-% (Var1("o",2))
              ; ["s",2; "o", 2], ("smac" %<-% Var1("s",2)) %:% ("out" %<-% Var1("o",2))
              ; ["d",2; "p", 2], ("dst" %<-% Var1("d",2)) %:% ("out" %<-% Var1("p",2))
              ; [], Skip
              ]
            , Skip)
  in
  synthesize_edit ~widening ~gas ~iter:1 ~fvs:["src", 2; "dst", 2; "smac", 2; "dmac", 2; "out", 2 ]
    (Prover.solver ())
    logical
    physical
    (StringMap.of_alist_exn [("src_table", [([Exact (0,2)], [1,2], 0)
                                           ;([Exact (1,2)], [2,2], 1)])
                           ; ("dst_table", [([Exact (0,2)], [1,2], 0)])
    ])
    StringMap.empty
    (Some ("dst_table", ([Exact (1,2)], [2,2], 0)))
      
                                      
            



let onf_representative gas widening =
  let fwd_type_i i  = mkVInt(i, 2) in
  let fwd_type_v = Var1("fwd_type", 2) in
  let fwd_bridge = fwd_type_i 0 in
  let fwd_mpls = fwd_type_i 1 in
  let fwd_ipv4 = fwd_type_i 2 in
  let station =
    Apply("my_station_table"
        , [("inport", 9); ("dmac", 48); ("eth_type", 16)]
        , [["sf", 2], ("fwd_type" %<-% Var1("sf", 2))]
        , "fwd_type" %<-% fwd_bridge)
  in
  let bridge =
    Apply("bridge_table"
        , [("vlan_id", 12); ("dmac", 48)]
        , [[("bv",32)], "next_id"%<-% Var1("bv",32)]
        , Skip)
  in
  let mpls =
    Apply("mpls_table"
        , [("mpls_label", 20)]
        , [["mv",32], ("mpls_label"%<-% mkVInt(0,20)) %:% ("next_id"%<-% Var1("mv", 32))]
        ,Skip)
  in
  let ipv4 =
    Apply("ipv4_dst"
        , [("ivp4_dst", 32)]
        , [["iv",32], "next_id" %<-% Var1("iv", 32)]
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
    Apply ("acl"
         , ["in_port", 9; "ip_proto", 8;
            "l4_src", 16; "l4_dst", 16;
            "dmac", 48 ; "smac", 48;
            "eth_type", 16; "ipv4_src", 32;
            "icmp_type", 16; "icmp_code", 16
           ]
         , [ ["av", 32], "next_id" %<-% Var1("av",32)
           ; [], "out_port"%<-% mkVInt(255, 9)
           ; [], ("drop"%<-% mkVInt(1,1)) %:% ("skip_next" %<-% mkVInt(1,1))
           ; [], Skip ]
         , Skip
      ) in
  let next =
    mkOrdered [ Var1("skip_next",1) %=% mkVInt(1,1),
                Apply("next"
                    , ["next_id", 32]
                    , [["nv",9], "out_port" %<-% Var1("nv",9)
                      ; ["nvv", 9; "nvs", 48; "nvd", 48], sequence [
                                                              "out_port" %<-% Var1("nvv",9)
                                                            ; "smac" %<-% Var1("nvs", 48)
                                                            ; "dmac" %<-% Var1("nvd", 48)]
                                                                   
                      ; ["nvvv", 9; "nvvs", 48; "nvvd", 48; "nvvm", 20],
                        sequence [
                            "mpls_label" %<-% Var1("nvvm", 20)
                          ; "out_port" %<-% Var1("nvvv",9)
                          ; "smac" %<-% Var1("nvvs", 48)
                          ; "dmac" %<-% Var1("nvvd", 48)
                      ]]
                    , Skip)
              ; True, Skip]
  in
  let fabric_egress =
    sequence [ Skip
        (* mkOrdered [ Var1("in_port",9) %=% Var1("out_port", 9), "drop"%<-% mkVInt(1,1)
         *           ; True, Skip ]; *)
        (* mkordered [
         *     (fwd_type_v %=% fwd_ipv4) %+% (fwd_type_v %=% fwd_mpls),
         *     sequence [ "ttl" %<-% Minus(Var1("ttl", 8), mkVInt(1,8))
         *              ; mkOrdered [Var1("ttl", 8) %=% mkVInt(0,8), "drop" %<-% mkVInt(1,1)
         *                         ; True, Skip]]
         *   ; True, Skip
         *   ] *)
      ]
  in
  let logical = sequence [station; set_next; acl; next; fabric_egress] in
  let admit =
    Apply("admit"
        , ["dmac", 48]
        , [[], "l3_admit" %<-% mkVInt(1,1)]
        , "l3_admit" %<-% mkVInt(0,1))
  in
  let l2 =
    Apply("l2"
        , ["dmac", 48]
        , [["out_port", 9], "out_port" %<-% Var1("out_port",9)]
        , Skip) in
  let l3 =
    Apply ("l3"
         , ["dmac", 48; "smac", 48; "ip_proto", 16; "l4_src", 16; "l4_dst", 16; (*"ttl", 8*)]
         , [["op", 9; "smac", 48; "dmac", 48;],
            sequence [
                "out_port" %<-% Var1("op", 9)
              ; "smac" %<-% Var1("smac", 48)
              ; "dmac" %<-% Var1("dmac", 48)
              (* ; "ttl" %<-% Minus(Var1("ttl",8), mkVInt(1,8)) *)
              ]
           ; [], Skip
           ; [], "drop" %<-% mkVInt(1,1)]
         , Skip )
  in
  let l2_or_l3 =
    mkOrdered[
        Var1("l3_admit",1) %=% mkVInt(0,1), l3;
        Var1("l3_admit",1) %=% mkVInt(1,1), l2
      ] in
  let punt =
    Apply("punt"
        , ["in_port", 9; "out_port", 9; "eth_type", 16
           ; "ipv4_src", 32; "ip_proto", 16; "icmp_code", 16
           ; "vlan_id", 12
          ] (* ip_diffserve vlan[0].vid, vlan[0].pcp, class_id, vrf_id *)
        , [ [], "out_punt" %<-% Var1("out_port",9)
          ; [], sequence [
                    "out_punt" %<-% Var1("out_port", 9)
                  ; "out_port" %<-% mkVInt(255, 9)
                  ]
          ; ["vpt", 9], sequence [
                            "out_punt" %<-% Var1("out_port", 9)
                          ; "out_port" %<-% Var1("vpt", 9)
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
        "in_port" %<-% mkVInt(0,9)
      ; "out_port" %<-% Var1("in_port", 9)
      ; "drop" %<-% mkVInt(0,1)
      ; "skip_next" %<-% mkVInt(0,1)
      ]
  in
  synthesize_edit ~widening ~gas ~iter:1 ~fvs (Prover.solver ())
    (init_metadata %:% logical)
    (init_metadata %:% physical)
    StringMap.empty
    StringMap.empty
    None
    
    
    
