loc := 0;
while (~(loc = 4 || loc = 5 || loc = 6 || loc = 7 || loc = 99)) {
  if total
    loc = 0 -> loc := 1 []
    loc = 1 && d = ?_21 -> loc := 2 []
    loc = 1 && d = ?_22 -> loc := 2 []
    loc = 1 && d = ?_31 -> loc := 3 []
    loc = 1 && d = ?_32 -> loc := 3 []
    loc = 2 && d = ?_24 -> loc := 4 []
    loc = 2 && d = ?_25 -> loc := 5 []
    loc = 3 && d = ?_36 -> loc := 6 []
    loc = 3 && d = ?_37 -> loc := 7 []
  fi
}
