out := 0#9;

apply(pps,
     (ip_src#32,ip_dst#32, src_port#16, dst_port#16,),
     ({\ (a#9,) -> access := a#9}),
     { skip });

apply (ppx,
       (proto#8,),
       ({ \ (p#9,) -> if ordered
                        proto#8 <> 0#8 -> out := p#9 []
	                    true -> out := access#9 []
	               fi }),
       { skip })