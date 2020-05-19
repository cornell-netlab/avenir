out := 0#9;

apply(acl,
     (ip_src#32,ip_dst#32, src_port#16, dst_port#16, proto#8,),
     ({\ (o#9,) ->  if ordered
                        proto#8 = 0#8 -> skip []
	                    true -> out := o#9 []
	               fi }),
     { skip })