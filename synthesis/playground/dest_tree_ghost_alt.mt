{ d = ?1 = ?3 = alpha & !?ghost = 4 = gamma
+ d = ?1 = ?4 = alpha & !?ghost = 5 = gamma
+ d = ?2 = ?3 = alpha & !?ghost = 4 = gamma
+ d = ?2 = ?4 = alpha & !?ghost = 5 = gamma
+ d = ?5 = ?7 = alpha & !?ghost = 6 = gamma
+ d = ?5 = ?8 = alpha & !?ghost = 7 = gamma
+ d = ?6 = ?7 = alpha & !?ghost = 6 = gamma
+ d = ?6 = ?8 = alpha & !?ghost = 7 = gamma }
loc = 1;
if total
  loc = 1 & d = ?1 -> loc := 2 []
  loc = 1 & d = ?2 -> loc := 2 []
  loc = 1 & d = ?5 -> loc := 3 []
  loc = 1 & d = ?6 -> loc := 3 []
fi ;
{ !?ghost = gamma & (loc = 2 & d = ?3 = alpha & 4 = !?ghost
                    + loc = 2 & d = ?4 = alpha & 5 = !?ghost
		    + loc = 3 & d = ?7 = alpha & 6 = !?ghost
		    + loc = 3 & d = ?8 = alpha & 7 = !?ghost) }
if total
  loc = 2 & d = ?3 -> assert (?!ghost = 4); loc := ?!ghost []
  loc = 2 & d = ?4 -> assert (?!ghost = 5); loc := ?!ghost []
  loc = 3 & d = ?7 -> assert (?!ghost = 6); loc := ?!ghost []
  loc = 3 & d = ?8 -> assert (?!ghost = 7); loc := ?!ghost []
fi
{d = alpha & loc = gamma}