stupid := 0#9;
apply(physical,
     (x1#32,x2#32,),
     ({ \(o#9,) -> suck := o#9 }),
     {skip});

apply(physical2,
     (suck#9,x2#32,),
     ({ \(s#9,) -> out := s#9 }),
     {skip})