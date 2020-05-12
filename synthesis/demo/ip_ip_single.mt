apply(outer_inner_ip,
      (outer_valid#1,
      outer_dst#32,
      inner_dst#32,),
      ({\ (o1#9,) -> out := o1#9}
       | {\ (o2#9, d#32,) -> out := o2#9; outer_valid := 1#1; outer_dst := d#32}),
      {skip})
       