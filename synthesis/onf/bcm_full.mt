out_port := 0#9;
local_metadata__l3_admit := 0#1;
local_metadata__vrf_id := 0#10;
local_metadata__class_id := 0#8;

hdr__mpls__isValid := 0#1;
hdr__vlan_tag__isValid := 0#1;
hdr__vlan_tag__eth_type := 0#16;
hdr__ipv4__isValid := 0#1;
hdr__ipv4__version := 0#4;
hdr__ipv4__ttl := 0#8;
hdr__ipv4__dst_addr := 0#32;
hdr__ipv4__src_addr := 0#32;
hdr__ipv6__isValid := 1#1;
hdr__packet_in__isValid := 0#1;
hdr__packet_out__isValid := 0#1;
ipv6_next_header := 0#8;

if ordered
  hdr__packet_out__isValid#1 = 1#1
  -> standard_metadata__egress_spec := hdr__packet_out__egress_port#9;
     hdr__packet_out__isValid := 0#1 []
  true -> skip []
fi;

if ordered
  out_port#9 = 0#9
  -> apply(my_station_table,
          (hdr__ethernet__dst_addr#48,),
	  ({local_metadata__l3_admit := 1#1}),
	  {skip});
     if ordered
       local_metadata__l3_admit#1 = 1#1
       -> apply(l3_fwd,
               (ipv6_dst#128, ipv6_src#128, ipv6_next_header#8,),
               ( {\ (port#9,) -> out_port := port#9;
                                 if ordered
	                            ipv6_hop_count#8 = 0#8 -> ipv6_hop_count := 0#8 []
			            true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8 []
                                 fi }),
               {out_port := 0#9})[]
       true ->
           apply(l2_fwd,
	        (hdr__ethernet__dst_addr#48,),
		( {\ (port2#9,) -> out_port := port2#9 }),
		{ skip })
       []
     fi;
     apply(punt,
          (ipv6_hop_count#8,
	   standard_metadata__ingress_port#9,
	   out_port#9,
	   hdr__ipv6_base__traffic_class#8,
	   ipv6_src#128,
	   ipv6_dst#128,
	   ipv6_next_header#8,
	   hdr__vlan_tag__vlan_id#12,
	   hdr__vlan_tag__pri#3,
	   local_metadata__class_id#8,
	   local_metadata__vrf_id#10,
	   ),
          ({ \ (port_punt#9,) -> out_port := port_punt#9 }),
          {skip}) []
  true -> skip []
fi;  

if ordered
  out_port#9 = 0#9 ->
      standard_metadata__ingress_port := 0#9;
      out_port := 0#9;
      ipv6_dst := 0#128;
      ipv6_src := 0#128;
      ipv6_next_header := 0#8;
      ipv6_hop_count := 0#8;
      hdr__ipv6_base__traffic_class := 0#8;
      hdr__vlan_tag__isValid := 0#1;
      hdr__vlan_tag__vlan_id := 0#12;
      hdr__vlan_tag__pri := 0#3;
      hdr__vlan_tag__cfi := 0#1;
      hdr__ethernet__dst_addr := 0#48;
      hdr__ethernet__src_addr := 0#48;
      hdr__eth_typ__value := 0#16;
      hdr__ipv4__src_addr := 0#32;
      hdr__ipv4__dst_addr := 0#32;
      hdr__ipv4__proto := 0#8;
      hdr__icmp__icmp_type := 0#8;
      hdr__icmp__icmp_code := 0#8;
      hdr__tcp__sport := 0#16;
      hdr__tcp__dport := 0#16;
      hdr__udp__sport := 0#16;
      hdr__udp__dport := 0#16;
      hdr__mpls__isValid := 0#1;
      hdr__packet_in__isValid := 0#1;
      hdr__packet_in__ingress_port := 0#9;
      hdr__packet_out__isValid := 0#1;
      hdr__packet_out__egress_port := 0#9
      []
   true -> skip []
fi