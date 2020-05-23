local_metadata__vrf_id := 0#10;
local_metadata__class_id := 0#8;
hdr__vlan_tag__0__pcp := 0#3;
hdr__vlan_tag__0__vid := 0#12;
local_metadata__icmp_code := 0#8;
hdr__ipv4_base__protocol := 0#8;
hdr__ipv4_base__dst_addr := 0#32;
hdr__ipv4_base__src_addr := 0#32;
hdr__ipv4_base__ttl := 0#8;
hdr__ipv4_base__diffserv := 0#8;
hdr__ethernet__ether_type := 0#16;
standard_metadata__ingress_port := 0#9;
punt_table_action_run := 0#1;
return5 := 0#1;
return4 := 0#1;
exit := 0#1;
hdr__ethernet__dst_addr := 0#48;
my_station_table_action_run := 0#1;
return4 := 0#1;
fabric_metadata__ip_eth_type := 0#16;
fabric_metadata__ip_eth_type := 34525#16;
return4 := 1#1;
my_station_table_action_run := 0#1;
if ordered
   true -> skip []
fi;
if ordered
   exit#1 = 0#1
   -> if ordered
         return4#1 = 0#1 ->
	    return5 := 0#1;
	    punt_table_action_run := 0#1;
	    if ordered
               true -> skip []
	    fi
	 []
	 true -> skip []
      fi     
   []
   true -> skip []
fi
