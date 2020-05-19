out:=0#9;
apply(l3,
     (ipv4_src#32,ipv4_dst#32,),
     (  { \ (s#48,o#9,) -> smac := s#48; out := o#9 }
      | { \ (oo#9,) -> out := oo#9 }
     ),
     {skip})