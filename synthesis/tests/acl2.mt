out := 0#9;
access := 0#9;

apply(acl1,
     (ip_src#32,ip_dst#32, src_port#16, dst_port#16,),
     ({\ (a#9,) -> access := a#9}),
     { skip });

apply (acl2,
       (access#9, proto#8,),
       ({ \ (p#9,) -> out := p#9 }),
       { skip })