{ alpha = d = 00
  & 0 = ??000 = beta0
  & 0 = ??001 = beta1
  & 00 = gamma
+ alpha = d = 00
  & 0 = ??000 = beta0
  & 1 = ??001 = beta1
  & 01 = gamma
+ alpha = d = 00
  & 1 = ??000 = beta0
  & 0 = ??001 = beta1
  & 10 = gamma
+ alpha = d = 00
  & 1 = ??000 = beta0
  & 1 = ??001 = beta1
  & 11 = gamma
  
+ alpha = d = 01 & 0 = ??010 = beta0 & 0 = ??011 = beta1 & 00 = gamma
+ alpha = d = 01 & 0 = ??010 = beta0 & 1 = ??011 = beta1 & 01 = gamma
+ alpha = d = 01 & 1 = ??010 = beta0 & 0 = ??011 = beta1 & 10 = gamma
+ alpha = d = 01 & 1 = ??010 = beta0 & 1 = ??011 = beta1 & 11 = gamma
+ alpha = d = 10 & 0 = ??100 = beta0 & 0 = ??101 = beta1 & 00 = gamma
+ alpha = d = 10 & 0 = ??100 = beta0 & 1 = ??101 = beta1 & 01 = gamma
+ alpha = d = 10 & 1 = ??100 = beta0 & 0 = ??101 = beta1 & 10 = gamma
+ alpha = d = 10 & 1 = ??100 = beta0 & 1 = ??101 = beta1 & 11 = gamma
+ alpha = d = 11 & 0 = ??110 = beta0 & 0 = ??111 = beta1 & 00 = gamma
+ alpha = d = 11 & 0 = ??110 = beta0 & 1 = ??111 = beta1 & 01 = gamma
+ alpha = d = 11 & 1 = ??110 = beta0 & 0 = ??111 = beta1 & 10 = gamma
+ alpha = d = 11 & 1 = ??110 = beta0 & 1 = ??111 = beta1 & 11 = gamma
}
loc := 0;
if total
  loc = 0 && d = 00 ->      
      { 0 = ??000 = beta0 & 0 = ??001 = beta1 & 4 = gamma & d = alpha
      + 0 = ??000 = beta0 & 1 = ??001 = beta1 & 5 = gamma & d = alpha
      + 1 = ??000 = beta0 & 0 = ??001 = beta1 & 6 = gamma & d = alpha
      + 1 = ??000 = beta0 & 1 = ??001 = beta1 & 7 = gamma & d = alpha }
      h0 := ??000; h1 := ??001; loc = 1 []
  loc = 0 && d = 01 ->
      { 0 = ??010 = beta0 & 0 = ??011 = beta1 & 4 = gamma & d = alpha
      + 0 = ??010 = beta0 & 1 = ??011 = beta1 & 5 = gamma & d = alpha
      + 1 = ??010 = beta0 & 0 = ??011 = beta1 & 6 = gamma & d = alpha
      + 1 = ??010 = beta0 & 1 = ??011 = beta1 & 7 = gamma & d = alpha
      }
      h0 := ??010; h1 := ??011; loc = 1 []
  loc = 0 && d = 10 ->    
      { 0 = ??100 = beta0 & 0 = ??101 = beta1 & 4 = gamma & d = alpha
      + 0 = ??100 = beta0 & 1 = ??101 = beta1 & 5 = gamma & d = alpha
      + 1 = ??100 = beta0 & 0 = ??101 = beta1 & 6 = gamma & d = alpha
      + 1 = ??100 = beta0 & 1 = ??101 = beta1 & 7 = gamma & d = alpha
      }
      h0 := ??100; h1 := ??101; loc = 1 []
  loc = 0 && d = 11 ->
      { 0 = ??110 = beta0 & 0 = ??111 = beta1 & 4 = gamma & d = alpha
      + 0 = ??110 = beta0 & 1 = ??111 = beta1 & 5 = gamma & d = alpha
      + 1 = ??110 = beta0 & 0 = ??111 = beta1 & 6 = gamma & d = alpha
      + 1 = ??110 = beta0 & 1 = ??111 = beta1 & 7 = gamma & d = alpha
      }
      h0 := ??110; h1 := ??111; loc = 1 []
fi ;
{ loc = 1 & 0 = h0 = beta0 & 0 = h1 = beta1 & 4 = gamma & d = alpha +
loc = 1 & 0 = h0 = beta0 & 1 = h1 = beta1 & 5 = gamma & d = alpha +
loc = 1 & 1 = h0 = beta0 & 0 = h1 = beta1 & 6 = gamma & d = alpha +
loc = 1 & 1 = h0 = beta0 & 1 = h1 = beta1 & 7 = gamma & d = alpha }
=
{ loc = 1 & h0 = 0 & 2 = 2 & h1 = 0 & 4 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 1 & h0 = 0 & 2 = 2 & h1 = 1 & 5 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 1 & h0 = 1 & 3 = 3 & h1 = 0 & 6 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 1 & h0 = 1 & 3 = 3 & h1 = 1 & 7 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
}
loc
if total
  loc = 1 && h0 = 0 -> loc := 2 []
  loc = 1 && h0 = 1 -> loc := 3 []
fi;
{ loc = 2 & h1 = 0 & 4 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 2 & h1 = 1 & 5 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 3 & h1 = 0 & 6 = gamma & h0 = beta0 & h1 = beta1 & d = alpha
+ loc = 3 & h1 = 1 & 7 = gamma & h0 = beta0 & h1 = beta1 & d = alpha}

if total
  loc = 2 && h1 = 0 -> loc := 4 []
  loc = 2 && h1 = 1 -> loc := 5 []
  loc = 3 && h1 = 0 -> loc := 6 []
  loc = 3 && h1 = 1 -> loc := 7 []
fi
{d = alpha & h0 = beta0 & h1 = beta1 & loc = gamma }