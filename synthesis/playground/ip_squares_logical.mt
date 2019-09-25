{ d = ?1 & d = ?3 & d = alpha & & ?33 = sigma & ?333 = delta & 10 = gamma}
loc := 0;
{loc = 0 & d = ?1 & 1 = 1 & d = ?3 & ?33 = sigma & ?333 = delta & 10 = gamma }
if total 
   loc = 0 && d = ?1 -> s := ?11; dm := ?111; loc := 1 []
   loc = 0 && d = ?2 -> s := ?22; dm := ?222 loc := 11 []
fi;
{loc = 1 & d = ?3 & d = alpha &  ?33 = sigma && ?333 = delta & 10 = gamma }
if total
   loc = 1 && d = ?3 -> s := ?33; dm := ?333; loc := 10 []
   loc = 1 && d = ?4 -> s := ?44; dm := ?444; loc := 0 []
fi;
{ d = alpha & s = sigma & dm = delta & loc = gamma  & loc = 10 }
assert (loc = 10)
{ d = alpha & s = sigma & dm = delta & loc = gamma }