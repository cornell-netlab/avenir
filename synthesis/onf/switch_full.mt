out_port := 0#9;
class_id := 0#32;
drop_flag := 1#1;


hdr__mpls__isValid := 0#1;
hdr__vlan_tag__isValid := 0#1;
hdr__ipv4__isValid := 0#1;
hdr__ipv6__isValid := 1#1;
hdr__packet_in__isValid := 0#1;
hdr__packet_out__isValid := 0#1;
ipv6_next_header := 0#8;


apply(validate,
      (ipv6_hop_count#8,),
      ({ drop_flag := 1#1 }),
      { drop_flag := 0#1 });

apply(ipv6_fib,
      (ipv6_dst#128,),
      ( {\ (n#32) -> class_id := n#32 }),
      {skip});

     
if ordered
   class_id#32 = 1017#32 -> out_port := 17#9 []
   class_id#32 = 1018#32 -> out_port := 18#9 []
   class_id#32 = 1010#32 -> out_port := 10#9 []
   class_id#32 = 1012#32 -> out_port := 12#9 []
   class_id#32 = 1011#32 -> out_port := 11#9 []
   class_id#32 = 1008#32 -> out_port := 08#9 []
   class_id#32 = 1003#32 -> out_port := 03#9 []
   class_id#32 = 1019#32 -> out_port := 19#9 []
   true -> out_port := 0#9 []
fi;

apply(l3_rewrite,
      (ipv6_dst#128,),
      ({\ (o#9,) ->
         if ordered
           ipv6_hop_count#8 = 0#8 -> ipv6_hop_count := 0#8 []
	   true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8 []
	 fi }),
      {skip});

if ordered
   drop_flag#1 = 1#1 -> out_port := 0#9 []
   true -> skip
fi;


if ordered
  out_port#9 = 0#9 ->
      standard_metadata__ingress_port := 0#9;
      ipv6_dst := 0#128;
      ipv6_src := 0#128;
      ipv6_next_header := 0#8;
      ipv6_hop_count := 0#8;
      out_port := 0#9;
      hdr__vlan_tag__isValid := 0#1;
      hdr__vlan_tag__vlan_id := 0#12;
      hdr__vlan_tag__pri := 0#3;
      hdr__vlan_tag__cfi := 0#1;
      hdr__ethernet__dst_addr := 0#48;
      hdr__ethernet__src_adr := 0#48;
      hdr__vlan_tag__vlan_id := 0#12;
      hdr__eth_typ__value := 0#16;
      hdr__ipv4__src_addr := 0#32;
      hdr__ipv4__dst_addr := 0#32;
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