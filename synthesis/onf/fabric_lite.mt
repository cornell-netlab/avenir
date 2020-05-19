out:=0#9;
apply(rewrite,
     (ipv4_src#32,),
     ( { \ (s#48) -> smac := s#48 } ),
     {skip});

apply(l3_fwd,
     (ipv4_dst#32,),
     ( {\ (o#9,) -> out := o#9} ),
     {skip});

apply(acl,
     (ipv4_src#32,ipv4_dst#32,),
     ( { out := 511#9 } | { out := 0#9 } ),
     {skip})	  

