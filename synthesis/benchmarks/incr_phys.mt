loc := 0;
if ordered
   loc = 0 && dst#2 = ?dst#2 && ?act#2 = 0#2 -> x := 0#2; loc := 0 []
   loc = 0 && dst#2 = ?dst#2 && ?act#2 = 1#2 -> x := 1#2; loc := 0 []
   loc = 0 && dst#2 = ?dst#2 && ?act#2 = 2#2 -> x := 2#2; loc := 0 []
   loc = 0 && dst#2 = ?dst#2 && ?act#2 = 3#2 -> x := 3#2; loc := 0 []
   loc = 0 -> x := 0#2; loc := 0 []
fi;
if ordered
   loc = 0 && x#2 = ?x#2 && ?act2#2 = 0#2 -> out := 0#2; loc := 0 []
   loc = 0 && x#2 = ?x#2 && ?act2#2 = 1#2 -> out := 1#2; loc := 0 []
   loc = 0 && x#2 = ?x#2 && ?act2#2 = 2#2 -> out := 2#2; loc := 0 []
   loc = 0 && x#2 = ?x#2 && ?act2#2 = 3#2 -> out := 3#2; loc := 0 []
   loc = 0 -> out := 0#2; loc := 0 []
fi