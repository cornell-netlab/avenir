out := 0#9;
access := 0#9;

apply(pps,
     (ip_src#32,ip_dst#32, src_port#16,),
     ({\ (a#9,) -> access := a#9}),
     { skip });

apply(mid,
     ( access#9, dst_port#16,),
     ({\ (s#9,) -> access := s#9}),
     { skip });

apply (ppx,
       (access#9, proto#8,),
       ({ \ (p#9,) -> out := p#9 }),
       { skip })