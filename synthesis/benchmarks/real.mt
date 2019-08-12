loc := 0;
while (~ loc = 6 ) {
  if ordered
    loc = 0 && x < ?hole3 && x > ?hole4 -> loc := 1 []
    loc = 0 -> loc := 3 []
    loc = 1  -> loc := 2 []
    loc = 2 -> y:= 0; loc := 6 []
    loc = 3 && x = ?hole1 -> loc := 4 []
    loc = 3 -> loc := 5 []
    loc = 4 -> y := ?hole5; loc := 6 []
    loc = 5 -> y:= 1; loc := 6 []
  fi
}