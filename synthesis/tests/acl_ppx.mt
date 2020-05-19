access := 0#9;

apply(pps,
     (ip_src#32,ip_dst#32, src_port#16, dst_port#16,),
     ({\ (a#9,) -> access := access#9}),
     { skip });

apply (ppx,
       (access#9,proto#8,),
       ({ \ (o#9,) -> if ordered
                        proto#8 <> 0#8 -> out := access#9 []
	                    true -> out := o#9 []
	               fi }),
       { skip })