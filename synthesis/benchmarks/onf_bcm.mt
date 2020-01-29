if ordered
  hdr.packet_out.isValid()
  -> standard_metadata.egress_spec = hdr.packet_out.egress_physical_port;
     hdr.packet_out.setInvalid() []
  true -> skip []
fi;
if ordered
  (standard_metadata.egress_spec == 0 || standard_metadata.egress_spec == LOOPBACK_PORT)
    -> apply(my_station_table
            , (hdr.ethernet.dst_addr : ternary)
	    , ( { \ () -> local_metadata.l3_admit = 1 }
	      | { \ () -> skip } )
	    , skip);
       if ordered
         local_metadata.l3_admit == 1
           -> apply(l3_fwd
	           , (local_metadata.vrf_id : exact
		     , hdr.ipv4_base.dst_addr : lpm
		     , hdr.ipv4_base.src_addr : selector
		     , hdr.ipv4_base.protocol : selector
		     , local_metadata.l4_src_port : selector
		     , local_metadata.l4_dst_port : selector)
		   , ( { \ (port, smac, dmac, dst_vlan) ->
		         standard_metadata.egress_spec = port;
			 local_metadata.dst_vlan = dst_vlan
			 hdr.ethernet.src_addr = smac;
			 hdr.ethernet.dst_addr = dmac;
			 hdr.ipv4_base.ttl = hdr.ipv4_base.ttl - 1 }
		       | { \ () -> skip }
		       | { \ () -> mark_to_drop(standard_metadata) } )
		   , skip)
            []
         true
           -> apply(l2_unicast_table
                   , (hdr.ethernet.dst_addr : exact)
		   , ({ \ (port) -> standard_metadata.egress_spec = port })
		   , skip ) []
       fi []
  true -> exit
fi;
apply(punt_table
     , (standard_metadata.ingress_port : ternary
       , standard_metadata.egress_spec : ternary
       , hdr.ethernet.ether_type : ternary
       , hdr.ipv4_base.diffserv : ternary
       , hdr.ipv4_base.ttl : ternary
       , hdr.ipv4_base.src_addr : ternary
       , hdr.ipv4_base.protocol : ternary
       , hdr.ipv4_base.protocol : ternary
       , local_metadata.icmp_code : ternary
       , hdr.vlan_tag[0].vid : ternary
       , hdr.vlan_tag[0].pcp : ternary
       , local_metadata.class_id : ternary
       , local_metadata.vrf_id : ternary)
     , ( { \ (queue_id) ->
             local_metadata.cpu_cos_queue_id = queue_id;
	     local_metadata.egress_spec_at_punt_match = standard_metadata.egress_spec }
       | { \ (queue_id) ->
             local_metadata.cpu_cos_queue_id = queue_id;
	     local_metadata.egress_spec_at_punt_match = standard_metadata.egress_spec;
	     standard_metadata.egress_spec = CPU_PORT }
       | ( \ (port) ->
             local_metadata.egress_spec_at_punt_match = standard_metadata.egress_spec;
	     standard_metadata.egress_spec = port } )
     , skip)	     

if ordered
  standard_metadata.egress_port == CPU_PORT
    -> hdr.packet_in.setValid();
       hdr.packet_in.ingress_physical_port = standard_metadata.ingress_port;
       hdr.packet_in.target_egress_port = local_metadata.egress_spec_at_punt_match;
       exit []
  true -> skip 
fi




  
  
