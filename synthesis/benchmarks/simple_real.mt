loc := 0;
while (~ loc = 1 && ~ loc = -1 ) {
  if
    loc = 0 && pkt = ?_0 -> loc := 1; pkt := ?_1 []
    loc = 0 && ~(pkt = ?_0) -> loc := -1 []
  fi
}