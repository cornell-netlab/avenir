loc := 0;
assert (d = 101 || d = 102 || d = 103 || d = 104);
while (~ (loc = 4 || loc = 5 || loc = 6 || loc = 7 || loc = 99)) {
  if ordered
    loc = 0 && d = 101 -> loc := 1; h1 := ?__101_h1 ; h2 := ?__101_h2 []
    loc = 0 && d = 102 -> loc := 1; h1 := ?__102_h1 ; h2 := ?__102_h2 []
    loc = 0 && d = 103 -> loc := 1; h1 := ?__103_h1 ; h2 := ?__103_h2 []
    loc = 0 && d = 104 -> loc := 1; h1 := ?__104_h1 ; h2 := ?__104_h2 []
    loc = 1 && h1 = 0 -> loc := 2 []
    loc = 1 && h1 = 1 -> loc := 3 []
    loc = 2 && h2 = 0 -> loc := 4 []
    loc = 2 && h2 = 1 -> loc := 5 []
    loc = 3 && h2 = 0 -> loc := 6 []
    loc = 3 && h2 = 1 -> loc := 7 []
  fi
}