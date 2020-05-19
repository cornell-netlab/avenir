out_port := 0#9;
l3_metadata__nexthop_index := 0#32;
drop_flag := 1#1;
l3_metadata__vrf := 0#16;

l2_metadata__l2_redirect := 0#1;
acl_metadata__acl_redirect := 0#1;
acl_metadata__racl_redirect := 0#1;
l3_metadata__rmac_hit := 0#1;
l3_metadata__fib_hit := 0#1;
nat_metadata__nat_hit := 0#1;
l2_metadata__lkp_pkt_type := 0#3;
l3_metadata__lkp_ip_type := 0#2;
multicast_metadata__igmp_snooping_enabled := 0#1;
multicast_metadata__mld_snooping_enabled := 0#1;
multicast_metadata__mcast_route_hit := 0#1;
multicast_metadata__mcast_bridge_hit := 0#1;
multicast_metadata__mcast_rfp_group := 0#16;
multicast_metadata__mcast_mode := 0#3;
nexthop_metadata__nexthop_type := 0#1;

acl_metadata__if_label := 0#16;
acl_metadata__bd_label := 0#16;
ingress_metadata__ifindex := 0#16;
l2_metadata__lkp_mac_type := 0#16;
l2_metadata__port_vlan__mapping_miss := 0#1;
security_metadata__ipsg_check_fail := 0#1;
acl_metadata__acl_deny := 0#1;
acl_metadata__racl_deny := 0#1;
l3_metadata__urpf_check_fail := 0#1;
ingress_metadata__drop_flag := 0#1;
l3_metadata__l3_copy := 0#1;
l3_metadata__rmac_hit := 0#1;
l3_metadata__routed := 0#1;
ipv6_metadata__ipv6_src_is_link_local := 0#1;
l2_metadata__same_if_check := 0#16;
tunnel_metadata__tunnel_if_check := 0#16;
l3_metadata__same_bd_check := 0#16;
l3_metadata__lkp_ip_ttl := 0#8;
l2_metadata__stp_state := 0#16;
ingress_metadata__control_frame := 0#1;
ipv4_metadata__ipv4_unicast_enabled := 0#1;
ipv6_metadata__ipv6_unicast_enabled := 0#1;
ingress_metadata__egress_ifindex := 0#16;
fabric_metadata__reason_code := 0#16;

egress_metadata__smac_idx := 0#9;
ingress_metadata__egress_ifindex := 0#16;

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
ipv6_next_header := 0#8;

ingress_metadata__port_type := 0#3;

if ordered
  ingress_metadata__port_type#3 = 1#3 -> skip []
  true ->
    l3_metadata__lkp_ip_type := 2#2;
    l3_metadata__ipv4_unicast_enabled := 1#1;

    if ordered
       hdr__ipv4__isValid#1 = 1#1 ->
         apply(validate_outer_ipv4_packet,
              (hdr__ipv4__version#4,
               hdr__ipv4__ttl#8,
	       hdr__ipv4__src_addr#32,),
	      ( {drop_flag := 1#1} ),
	     { drop_flag := 0#1 })
       []
       hdr__ipv6__isValid#1 = 1#1 ->
         apply(validate_outer_ipv6_packet,
              (hdr__ipv6__version#4,
	       ipv6_hop_count#8,
	       ipv6_src#128,),
              ({ drop_flag := 1#1 }),
              { drop_flag := 0#1 })
       []
       true -> skip []
    fi;   
    if ordered
      hdr__ipv4__isValid#1 = 1#1 ->
        l3_metadata__lkp_ip_ttl := hdr__ipv4__ttl#8 []
      true ->
        l3_metadata__lkp_ip_ttl := ipv6_hop_count#8 []
    fi;      
  
    if ordered
      l3_metadata__lkp_ip_type#2 = 1#2
        && l3_metadata__ipv4_unicast_enabled#1 = 1#1
      ->
        apply(ipv4_fib,
             (l3_metadata__vrf#16, hdr__ipv4__dst_addr#32,),
             ( {\ (n4#32) -> l3_metadata__nexthop_index := n4#32 } | {skip} ),
             {skip})
      []
      true
      -> apply(ipv6_fib,
              (l3_metadata__vrf#16, ipv6_dst#128,),
              ( {\ (n#32) -> l3_metadata__nexthop_index := n#32 } | { skip } ),
              {skip})
         []
    fi;

    if ordered
       nexthop_metadata__nexthop_type#1 = 1#1
       -> apply(ecmp_group, (l3_metadata__nexthop_index#32,), ({skip}), {skip}) []
       nexthop_metadata__nexthop_type#1 = 0#1
       -> apply(nexthop, (l3_metadata__nexthop_index#32,),
                         ({ \ (f#16) -> ingress_metadata__egress_ifindex := f#16 }), {skip}) []
     fi;

    if ordered
       ingress_metadata__egress_ifindex#16 = 65535#16
       -> skip []
       true ->
         apply(process_lag,
	      (ingress_metadata__egress_ifindex#16,),
	      ({\ (op#9) -> out_port := op#9 }), {skip} )
       []
    fi;

    apply(system_acl,
         (acl_metadata__if_label#16,
	  acl_metadata__bd_label#16,
	  ingress_metadata__ifindex#16,
	  l2_metadata__lkp_mac_type#16,
	  l2_metadata__port_vlan__mapping_miss#1,
	  security_metadata__ipsg_check_fail#1,
	  acl_metadata__acl_deny#1,
	  acl_metadata__racl_deny#1,
	  l3_metadata__urpf_check_fail#1,
	  ingress_metadata__drop_flag#1,

	  l3_metadata__l3_copy#1,

	  l3_metadata__rmac_hit#1,

	  l3_metadata__routed#1,
	  ipv6_metadata__ipv6_src_is_link_local#1,
	  l2_metadata__same_if_check#16,
	  tunnel_metadata__tunnel_if_check#16,
	  l3_metadata__same_bd_check#16,
	  l3_metadata__lkp_ip_ttl#8,
	  l2_metadata__stp_state#16,
	  ingress_metadata__control_frame#1,
	  ipv4_metadata__ipv4_unicast_enabled#1,
	  ipv6_metadata__ipv6_unicast_enabled#1,

	  ingress_metadata__egress_ifindex#16,
	  fabric_metadata__reason_code#16,),
	  
	  ( {skip} | {out_port := 511#9} | {out_port := 0#9}),
	  {skip} )
  []
fi;

apply(l3_rewrite,
      (hdr__ipv4__isValid#1,hdr__ipv6__isValid#1, ipv6_dst#128,
       hdr__ipv4__dst_addr#32,
      ),
      ({ if ordered
           ipv6_hop_count#8 = 0#8 -> ipv6_hop_count := 0#8 []
	   true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8 []
	 fi }
       | {if ordered
           hdr__ipv4__ttl#8 = 0#8 -> hdr__ipv4__ttl := 0#8 []
	   true -> hdr__ipv4__ttl := hdr__ipv4__ttl#8 - 1#8 []
	 fi }
       | {skip}),
      {skip});

apply(smac_rewrite,
     (egress_metadata__smac_idx#9,),
     ({\ (smacr#48,) -> hdr__ethernet__src_addr := smacr#48 }),
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
      hdr__ethernet__src_addr := 0#48;
      hdr__vlan_tag__vlan_id := 0#12;
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