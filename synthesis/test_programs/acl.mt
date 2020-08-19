out := 0#9;

apply(acl,
     (ip_src#32,ip_dst#32, src_port#16, dst_port#16, proto#8,),
     ({\ (o#9,) ->  out := o#9 }),
     { skip })