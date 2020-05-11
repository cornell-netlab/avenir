apply(inner_ip,
      (outer_valid#1,
       inner_dst#32,),
      ({\ (o3#9,) -> out := o3#9}
       | {\ (o4#9,d2#32,) -> out := o4#9; outer_valid:=1#1; outer_dst:=d2#32}),
      {skip});

apply(outer_ip,
      (outer_valid#1,
       outer_dst#32,),
      ({\ (o1#9,) -> out := o1#9 }
       | {\ (o2#9,d1#32,) -> out:=o2#9; outer_valid:=1#1; outer_dst := d1#32}),
      {skip})       

