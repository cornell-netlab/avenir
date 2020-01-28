## packet_io_ingress
if (hdr.packet_out.isValid()) {
  standard_metadata.egress_spec = hdr.packet_out.egress_port;
  hdr.packet_out.setInvalid();
  fabric_metadata.is_controller_packet_out = True;
  GOTO egress
}

if (vlan_tag.isValid()){
  fabric_metadata.vlan_id = hdr.vlan_tag.vlan_id;
  fabric_metadata.vlan_pri = hdr.vlan_tag.vlan_id;
  fabric_metadata.vlan_cfi = hdr.vlan_tag.cfi;
}
if (!hdr.mlps.isValid()){
  fabric_metadata.mpls_ttl = DEFAULT_MPLS_TTL +1;
}

apply(ingress_port_vlan
, (standard_metadata.ingress_port : exact
  , hdr.vlan_tag.isValid() : exact
  , hdr.vlan_tag.vlan_id : ternary
  , hdr.vlan_tag.vlan_id : ternary)
, ( {\ () -> fabric_metadata.skip_forwarding = 1; fabric_metadata.skip_next = 1  }
  , {\ () -> skip }
  , {\ (vlan_id) -> fabric_metadata.vlan_id = vlan_id } )
, {fabric_metadata.skip_forwarding = 1; fabric_metadata.skip_next = 1}
);
apply(fwd_classifier
, ( standard_metadata.ingress_port : exact
  , hdr.ethernet.dst_addr : ternary
  , hdr.eth_type.value : ternary
  , fabric_metadata.ip_eth_type : exact)
, ( {\ (fwd_type) -> fabric_metadata.fwd_typ = fwd_type } )
, fabric_metadata.fwd_typ = FWD_BRIDGING )

if  ordered
  fabric_metadata.skip_forwarding = 0
   -> if
        fabric_metadata.fwd_type = FWD_BRIDGING ->
	  apply(bridging
	       , ( fabric_metadata.vlan_id : exact
                 , hdr.ethernet.dst_addr : ternary )
	       , { \ (next_id) -> fabric_metadata.next_id = next_id }
	       , skip ) []
        fabric_metadata.fwd_type = FWD_MPLS ->
	  apply(mpls
	       , ( fabric_metadata.mpls_label : exact )
	       , ({ \ (next_id) -> fabric_metadata.mpls_label = 0; fabric_metadata.next_id = next_id })
	       , skip ) []
	fabric_metadata.fwd_type = FWD_IPV4_UNICAST ->
	  apply(mpls
	       , ( hdr.ipv4.dst_addr : lpm )
	       , ( { \ (next_id) -> fabric_metadata.next_id = next_id }
	         | { \ () -> skip } )
	       , skip) []
	true -> skip []
   fi
  true -> skip
fi;

apply(acl
     , ( standard_metadata.ingress_port : ternary
       , fabric_metadata.ip_proto : ternary
       , fabric_metadata.l4_sport : ternary
       , fabric_metadata.l4_dport : ternary
       , hdr.ethernet.dst_addr : ternary
       , hdr.ethernet.src_addr : ternary
       , vlan_tag.vlan_id : ternary
       , hdr.eth_type.value : ternary
       , hdr.ipv4.src_addr : ternary
       , hdr.icmp.icmp_type : ternary
       , hdr.icmp.icmp_code : ternary )
     , ( { \ (next_id) ->  fabric_metadata.next_id = next_id }
       | { \ () -> standard_metadata.egress_spec = CPU_PORT; fabric_metadata.skip_next = _TRUE }
       | { \ () -> mark_to_drop(standard_metadata); fabric_metadata.skip_next = _TRUE } 
       | { \ () -> skip } )
     , skip );
     

## packet_io_egress
if (fabric_metadata.is_controller_packet_out == CPU_PORT) {
  DONE
}
if (standard_metadata.egress_port == CPU_PORT) {
  hdr.packet_in.setValid();
  hdr.packet_in.ingress_port = standard_metadata.ingress_port;
  DONE
}

### EGRESS
if ordered
  fabric_metadata.is_multicast == _TRUE && standard_metadata.ingress_port == standard_metadata.egress_port
    -> mark_to_drop(standard_metadata) []
  true -> skip []
fi;
if ordered
  fabric_metadata.mpls_label = 0
    -> if ordered
         hdr.mpls.isValid() -> hdr.mpls.setInvalid(); hdr.eth_type.value = fabric_metadata.ip_eth_type []
         true -> hdr.mpls.setValid();
	         hdr.mpls.label = fabric_metadata.mpls_label;
		 hdr.mpls.tc = 0;
		 hdr.mpls.bos = 1
		 hdr.mpls.ttl = fabric_metadata.mpls_ttl;
		 hdr.eth_type.value = ETHERTYPE_MPLS []
       fi []
  true -> skip []
fi;
if ordered
  hdr.mpls.isValid() -> hdr.mpls.ttl := hdr.mpls.ttl -1;
                        if ordered
			  hdr.ipv4.ttl = 0 -> mark_to_drop(standard_metadata) []
			  true -> skip
			fi []
  hdr.ipv4.isValid() -> hdr.ivp4.ttl := hdr.ipv4.ttl -1;
                        if ordered
			  hdr.ipv4.ttl = 0 -> mark_to_drop(standard_metadata) []
			  true -> skip
			fi []
  true -> skip
fi
