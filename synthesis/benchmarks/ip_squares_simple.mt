{ d = ?1 & d = ?3 & d = alpha & & ?33 = sigma & 10 = gamma}
loc := 0;
{loc = 0 & d = ?1 & 1 = 1 & d = ?3 & ?33 = sigma & 10 = gamma }
if total 
   loc = 0 && d = ?1 -> s := ?11; loc := 1 []
   loc = 0 && d = ?2 -> s := ?22; loc := 11 []
fi;
{loc = 1 & d = ?3 & d = alpha ?33 = sigma & 10 = gamma }
if total
   loc = 1 && d = ?3 -> s := ?33; loc := 10 []
   loc = 1 && d = ?4 -> s := ?44; loc := 0 []
fi;
{ d = alpha & s = sigma & loc = gamma  & loc = 10 }
assert (loc = 10)
{ d = alpha & s = sigma & loc = gamma }