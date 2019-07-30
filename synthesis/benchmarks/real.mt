loc := 0;
while (~ loc = 6 ) {
  if
    loc = 0 && x = 5 -> loc := 1 []
    loc = 1 && y = ?hole6 -> loc := 2 []
    loc = 2 -> y:= 1; loc := 6 []
    loc = 5 -> y:= 0; loc := 6 []
    loc = 0 && x = ?hole0 -> loc := 3 []
    loc = 3 && x = ?hole1 && y = ?hole2 -> loc := 5 []
    loc = 3 && x = ?hole3 && y = ?hole4 -> loc := 4 []
    loc = 4 -> y := ?hole5; loc := 6
  fi
}