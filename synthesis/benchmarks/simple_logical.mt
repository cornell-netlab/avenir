loc := 0;
if
  loc = 0 && pkt = 42 -> loc := 1; pkt := 47 []
  loc = 0 && ~(pkt = 42) -> loc := -1
fi
