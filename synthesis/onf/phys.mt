if ordered
	((ipv6_dst#128 = 42541956123771093561836879591493372040#128&&0#128 = (ipv6_src#128 & 0#128))&&0#8 = (ipv6_next_header#8 & 0#8)) ->
  	  out_port := 8#9;
	  if ordered
	    ipv6_hop_count#8 = 0#8 -> ipv6_hop_count := 0#8 []
	    true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8 []
	  fi []
	((0#128 = (ipv6_dst#128 & 0#128)&&0#128 = (ipv6_src#128 & 0#128))&&0#8 = (ipv6_next_header#8 & 0#8)) -> out_port := 0#9; if ordered
	    ipv6_hop_count#8 = 0#8 -> ipv6_hop_count := 0#8 []
	    true -> ipv6_hop_count := ipv6_hop_count#8 - 1#8 []
	  fi []
	true -> out_port := 0#9 []
fi; if ordered
	ipv6_hop_count#8 = 0#8 -> out_port := 0#9 []
	true -> skip []
fi; if ordered
	out_port#9 = 0#9 ->
	   ipv6_dst := 0#128;
	   ipv6_src := 0#128;
	   ipv6_next_header := 0#8;
	   ipv6_hop_count := 0#8 []
	true -> skip []
fi