loc := 0;
while (~ loc = 1 && ~ loc = -1 ) {
  if ordered
    loc = 0 && pkt = 42 -> loc := 1; pkt := 47 []
    loc = 0 -> loc := -1 []
  fi
}