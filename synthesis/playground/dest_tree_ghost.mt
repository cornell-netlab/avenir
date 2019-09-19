{ d = ?1 = ?3 = alpha & !?4 = 4 = gamma
+ d = ?1 = ?4 = alpha & !?5 = 5 = gamma
+ d = ?2 = ?3 = alpha & !?4 = 4 = gamma
+ d = ?2 = ?4 = alpha & !?5 = 5 = gamma
+ d = ?5 = ?7 = alpha & !?6 = 6 = gamma
+ d = ?5 = ?8 = alpha & !?7 = 7 = gamma
+ d = ?6 = ?7 = alpha & !?6 = 6 = gamma
+ d = ?6 = ?8 = alpha & !?7 = 7 = gamma }
loc = 1;
if total
  loc = 1 & d = ?1 -> loc := 2 []
  loc = 1 & d = ?2 -> loc := 2 []
  loc = 1 & d = ?5 -> loc := 3 []
  loc = 1 & d = ?6 -> loc := 3 []
fi ;
{ loc = 2 & d = ?3 = alpha & ?!4 = gamma & ?!4 = 4 && ?!5 = 5 && ?!6 = 6 && ?!7 = 7
+ loc = 2 & d = ?4 = alpha & ?!5 = gamma & ?!4 = 4 && ?!5 = 5 && ?!6 = 6 && ?!7 = 7
+ loc = 3 & d = ?7 = alpha & ?!6 = gamma & ?!4 = 4 && ?!5 = 5 && ?!6 = 6 && ?!7 = 7
+ loc = 3 & d = ?8 = alpha & ?!7 = gamma & ?!4 = 4 && ?!5 = 5 && ?!6 = 6 && ?!7 = 7 }
assert (?!4 = 4 && ?!5 = 5 && ?!6 = 6 && ?!7 = 7)
{ loc = 2 & d = ?3 = alpha & ?!4 = gamma
+ loc = 2 & d = ?4 = alpha & ?!5 = gamma
+ loc = 3 & d = ?7 = alpha & ?!6 = gamma
+ loc = 3 & d = ?8 = alpha & ?!7 = gamma }
if total
  loc = 2 & d = ?3 -> loc := ?!4 []
  loc = 2 & d = ?4 -> loc := ?!5 []
  loc = 3 & d = ?7 -> loc := ?!6 []
  loc = 3 & d = ?8 -> loc := ?!7 []
fi
{d = alpha & loc = gamma}