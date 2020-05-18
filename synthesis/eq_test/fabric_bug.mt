fabric_metadata__next_id := 0#32;
simple_action_run := 0#1;
return3 := 0#1;
exit := 0#1;
routing_v6_action_run := 0#1;
return2 := 0#1;
return1 := 0#1;
CPU_PORT := 0#8;
fabric_metadata__ip_eth_type := 0#16;
fabric_metadata__ip_eth_type := 34525#16;
CPU_PORT := 64#8;
return1 := 1#1;
return2 := 0#1;
routing_v6_action_run := 0#1;
if ordered
    hdr__ipv6__dst_addr#128 = 42541956123771093561836879591493372040#128 ->
      routing_v6_action_run := 1#1;
      fabric_metadata__next_id := 1008#32 []
    true -> skip []
fi;
if partial
   exit#1 = 0#1
   -> return3 := 0#1;
      simple_action_run := 0#1;
      if ordered
	 fabric_metadata__next_id#32 = 1017#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 17#9 []
 	 fabric_metadata__next_id#32 = 1018#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 18#9 []
 	 fabric_metadata__next_id#32 = 1010#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 10#9 []
 	 fabric_metadata__next_id#32 = 1012#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 12#9 []
 	 fabric_metadata__next_id#32 = 1011#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 11#9 []
 	 fabric_metadata__next_id#32 = 1008#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 8#9 []
 	 fabric_metadata__next_id#32 = 1003#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 3#9 []
 	 fabric_metadata__next_id#32 = 1019#32 ->
	   simple_action_run := 1#1;
	   standard_metadata__egress_spec := 19#9 []	  	   
         true -> skip []
      fi
   []
fi 
