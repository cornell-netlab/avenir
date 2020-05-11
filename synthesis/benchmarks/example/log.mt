apply(obt,
       (tunnel_valid#1, tunnel_id#9,ipdst#32,),
       ( {\ (v#9,t#9,) -> out:=v#9; tunnel_id := t#9}
       | {\ (v#32,) -> out:=v#3}
       {skip})