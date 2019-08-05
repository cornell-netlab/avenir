loc := 0;
if
  loc = 0 && pkt = ?_0 -> loc := 1; pkt := ?_1 []
  loc = 0 && ~pkt = ?_2 -> loc := -1
fi