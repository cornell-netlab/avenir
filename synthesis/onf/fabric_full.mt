class_id := 0#32;
out_port := 0#9;
fabric_metadata__skip_forwarding := 0#1;
fabric_metadata__skip_next := 0#1;
fabric_metadata__mpls_label := 0#1;
fabric_metadata__l4_sport := 0#16;
fabric_metadata__l4_dport := 0#16;
fabric_metadata__is_multicast := 0#1;
fabric_metadata__vlan_id := 0#12;
fabric_metadata__is_controller_packet_out := 0#1;


hdr__mpls__isValid := 0#1;
hdr__vlan_tag__isValid := 0#1;
hdr__ipv4__isValid := 0#1;
hdr__ipv4__version := 0#4;
hdr__ipv4__ttl := 0#8;
hdr__ipv4__dst_addr := 0#32;
hdr__ipv4__src_addr := 0#32;
hdr__ipv6__isValid := 1#1;
hdr__packet_in__isValid := 0#1;
hdr__packet_out__isValid := 0#1;
fabric_metadata__vlan_id := 4096#12;
fabric_metadata__eth_type := 34525#16;
ipv6_next_header := 0#8;

fabric_metadata__ip_proto := ipv6_next_header#8;
if ordered
  ipv6_next_header#8 = 6#8
  -> fabric_metadata__l4_sport := hdr__tcp__sport#16;
     fabric_metadata__l4_dport := hdr__tcp__dport#16 []
  ipv6_next_header#8 = 17#8
  -> fabric_metadata__l4_sport := hdr__udp__sport#16;
     fabric_metadata__l4_dport := hdr__udp__dport#16 []
  ipv6_next_header#8 = 1#8 -> skip []
  true -> skip []
fi;


if ordered
   hdr__packet_out__isValid#1 = 1#1 ->
     out_port := hdr__packet_out__egress_port#9;
     hdr__packet_out__isValid := 0#1 []
   true
   -> if ordered 
         hdr__vlan_tag__isValid#1 = 1#1
         -> fabric_metadata__vlan_id := hdr__vlan_tag__vlan_id#12;
            fabric_metadata__vlan_pri := hdr__vlan_tag__pri#3;
            fabric_metadata__vlan_cfi := hdr__vlan_tag__cfi#1 []
         true
         -> fabric_metadata__mpls_ttl := 64#8 + 1#8 []
      fi;

      apply(ingress_port_vlan,
            (standard_metadata__ingress_port#9, hdr__vlan_tag__isValid#1,
             hdr__vlan_tag__vlan_id#12,),
            ({ fabric_metadata__skip_forwarding := 1#1;
               fabric_metadata__skip_next := 1#1 }
             | { skip }
             | {\ (v#12,) -> fabric_metadata__vlan_id := v#12 } ),
            {fabric_metadata__skip_forwarding := 1#1;
               fabric_metadata__skip_next := 1#1 });

      apply(fwd_classifier,
           (standard_metadata__ingress_port#9,
            hdr__ethernet__dst_addr#48,
            hdr__eth_type__value#16,
            fabric_metadata__ip_eth_type#16,),
           ({ \ (f#3,) -> fabric_metadata__fwd_type := f#3 }),
           { fabric_metadata__fwd_type := 0#3});

      if ordered
        fabric_metadata__skip_forwarding#1 = 0#1
        -> if ordered
            fabric_metadata__fwd_type#3 = 0#3 ->
              apply(bridging,
                   (fabric_metadata__vlan_id#32,
                    hdr__ethernet__dst_addr#48,),
                   ( {\ (next_id_bridging#32,) -> class_id := next_id_bridging#32 } ),
                   {skip})
            []
            fabric_metadata__fwd_type#3 = 1#3 ->
              apply(mpls,
	           (fabric_metadata__mpls_label#20,),
                   ({\ (next_id_mpls#32,) -> class_id := next_id_mpls#32} ),
                   {skip})
            []
           fabric_metadata__fwd_type#3 = 2#3 ->
           apply(routing_v4,
	        (hdr__ipv4__dst_addr#32,),
                ({\ (next_id_v4#32,) -> class_id := next_id_v4#32 }
	        | { skip } ),
                {skip})
           []
           fabric_metadata__fwd_type#3 = 3#3 ->
             apply(ipv6,
                  (ipv6_dst#128,),
                  ({ \ (next_id#32,) -> class_id := next_id#32}),
                   {skip}) []
             true -> skip []
           fi []
           true -> skip []
      fi;

      apply(acl,
           (standard_metadata__ingress_port#9,
            fabric_metadata__ip_proto#8,
            fabric_metadata__l4_sport#16,
            fabric_metadata__l4_dport#16,
            hdr__ethernet__dst_addr#48,
            hdr__ethernet__src_addr#48,
            hdr__vlan_tag__vlan_id#12,
            hdr__eth_typ__value#16,
            hdr__ipv4__src_addr#32,
            hdr__ipv4__dst_addr#32,
            hdr__icmp__icmp_type#8,
            hdr__icmp__icmp_code#8,
           ),
           ( {\ (next_acl#32,) -> class_id := next_acl#32 }
           | { out_port := 511#9;
               fabric_metadata__skip_next := 1#1}
           | { skip }
           | { out_port := 0#9 }
           | { skip }),
           {skip});

      if ordered
        fabric_metadata__skip_next#1 = 0#1
        -> apply(simple,
                (class_id#32,),
                ( { \ (onext#9,) -> out_port := onext#9 }
                | { \ (pp#9,ss#48,dd#48,) ->
                    hdr__ethernet__src_addr := smac#48;
                    hdr__ethernet__dst_addr := dmac#48;
                    out_port := p#9 }
	        | { \ (p#9,s#48,d#48,l#20,) ->
                    fabric_metadata__mpls_label := l#20;
                    hdr__ethernet__src_addr := smac#48;
                    hdr__ethernet__dst_addr := dmac#48;
                    out_port := p#9 }
                ),
               {skip});
           apply(multicast,
                (class_id#32,),
                ({ \ (group_id#16,) ->
	           standard_metadata__mcast_grp := group_id#16;
                   fabric_metadata__is_multicast := 1#1 } ),
                {skip});
           apply(next_vlan,
                (class_id#32,),
                ({\ (vlan_id#12,) -> fabric_metadata__vlan_id := vlan_id#12 }),
                {skip})
           []
        true -> skip []
      fi
    []
fi;

if ordered
  fabric_metadata__is_controller_packet_out#1 = 1#1 -> skip []
  out_port#9 = 511#9 ->
    hdr__packet_in__isValid := 1#1;
    hdr__packet_in__ingress_port := standard_metadata__ingress_port#9 []
  true ->
    if ordered
      fabric_metadata__is_multicast#1 = 1#1
      && standard_metadata__ingress_port#9 = out_port#9
      -> out_port := 0#9 []
      true -> skip []
    fi;
    if ordered
      hdr__mpls__isValid#1 = 1#1
      -> if ordered
           hdr__mpls__ttl#8 <= 1#8 -> out_port := 0#9 []
           true -> hdr__mpls__ttl := hdr__mpls__ttl#8 - 1#8 []
         fi []
      hdr__ipv4__isValid#1 = 1#1
      -> if ordered
           hdr__ipv4__ttl#8 <= 1#8 -> out_port := 0#9 []
           true -> hdr__ipv4__ttl := hdr__ipv4__ttl#8 - 1#8 []
         fi []
      hdr__ipv6__isValid#1 = 1#1
      -> if ordered
           ipv6_hop_count#8 <= 1#8 -> out_port := 0#9 []
           true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8  []
         fi []
    fi
  []
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