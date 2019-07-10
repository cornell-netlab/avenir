loc := 0;
while (~ loc = 6 ) {
  if
    loc = 0 & x = 10 -> loc := 1 []
    loc = 1 & y = _hole6 -> loc := 2 []
    loc = 2 -> y:= 1; loc := 6 []
    loc = 5 -> y:= 0; loc := 6 []
    loc = 0 & x = _hole0 -> loc := 3 []
    loc = 3 & x = _hole1 & y = _hole2 -> loc := 5 []
    loc = 3 & x = _hole3 & y = _hole4 -> loc := 4 []
    loc = 4 & y := _hole5 -> loc := 6
  fi
}