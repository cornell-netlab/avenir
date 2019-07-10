loc := 0;
while (~loc = 6) {
  if
    loc = 0 && x = 5 -> loc := 1    []
    loc = 0 && ~(x = 5) -> loc := 2 []
    loc = 1 -> y := 0; loc := 6     []
    loc = 2 -> y := 1; loc := 6
  fi
}