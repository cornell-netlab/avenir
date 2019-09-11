assert (loc = 81 || loc = 82 || loc = 83 || loc = 181 || loc = 182 || loc = 183);
if ordered
  loc = 81 -> loc := 1; in := 1 []
  loc = 82 -> loc := 2; in := 1 []
  loc = 83 -> loc := 3; in := 1 []
  loc = 181 -> loc := 101; in := 1 []
  loc = 182 -> loc := 102; in := 1 []
  loc = 183 -> loc := 103; in := 1 []
fi;
while (~(loc = 81 || loc = 82 || loc = 83 || loc = 181 || loc = 182 || loc = 183)) {
  if ordered
     loc = 1 && dstIP = ?_ip1 && in = 1 -> dstMAC := ?_mac1; pt := ?_pt1; in := 0 []
     loc = 2 && dstIP = ?_ip2 && in = 1 -> dstMAC := ?_mac2; pt := ?_pt2; in := 0 []
     loc = 3 && dstIP = ?_ip3 && in = 1 -> dstMAC := ?_mac3; pt := ?_pt3; in := 0 []
     loc = 4 && dstIP = ?_ip4 && in = 1 -> dstMAC := ?_mac4; pt := ?_pt4; in := 0 []
     loc = 101 && dstIP = ?_ip5 && in = 1 -> dstMAC := ?_mac5; pt := ?_pt5; in := 0 []
     loc = 102 && dstIP = ?_ip6 && in = 1 -> dstMAC := ?_mac6; pt := ?_pt6; in := 0 []
     loc = 103 && dstIP = ?_ip7 && in = 1 -> dstMAC := ?_mac7; pt := ?_pt7; in := 0 []

     loc = 1 && in = 0 && pt = 2 -> loc := 81; in := 1; pt := 1 []
     loc = 1 && in = 0 && pt = 2 -> loc := 2; in := 1; pt := 1 []
     loc = 1 && in = 0 && pt = 3 -> loc := 3; in := 1; pt := 1 []
     loc = 1 && in = 0 && pt = 4 -> loc := 4; in := 1; pt := 1 []
     
     loc = 2 && in = 0 && pt = 1 -> loc := 1; in := 1; pt := 2 []
     loc = 2 && in = 0 && pt = 2 -> loc := 82; in := 1; pt := 2 []
     loc = 2 && in = 0 && pt = 3 -> loc := 3; in := 1; pt := 2 []
     loc = 2 && in = 0 && pt = 4 -> loc := 4; in := 1; pt := 2 []
     loc = 3 && in = 0 && pt = 1 -> loc := 1; in := 1; pt := 3 []
     loc = 3 && in = 0 && pt = 2 -> loc := 2; in := 1; pt := 3 []
     loc = 3 && in = 0 && pt = 3 -> loc := 83; in := 1; pt := 3 []
     loc = 3 && in = 0 && pt = 4 -> loc := 4; in := 1; pt := 3 []
     
     loc = 4 && in = 0 && pt = 1 -> loc := 1; in := 1; pt := 4 []
     loc = 4 && in = 0 && pt = 2 -> loc := 2; in := 1; pt := 4 []
     loc = 4 && in = 0 && pt = 3 -> loc := 3; in := 1; pt := 4 []
     loc = 4 && in = 0 && pt = 101 -> loc := 101; in := 1; pt := 4 []
     loc = 4 && in = 0 && pt = 102 -> loc := 102; in := 1; pt := 4 []
     loc = 4 && in = 0 && pt = 103 -> loc := 103; in := 1; pt := 4 []

     loc = 101 && in = 0 && pt = 101 -> loc := 181; in := 1; pt := 101 []
     loc = 101 && in = 0 && pt = 102 -> loc := 102; in := 1; pt := 101 []
     loc = 101 && in = 0 && pt = 103 -> loc := 103; in := 1; pt := 101 []
     loc = 101 && in = 0 && pt = 104 -> loc := 4; in := 1; pt := 101 []
     loc = 102 && in = 0 && pt = 101 -> loc := 101; in := 1; pt := 102 []
     loc = 102 && in = 0 && pt = 102 -> loc := 182; in := 1; pt := 102 []
     loc = 102 && in = 0 && pt = 103 -> loc := 103; in := 1; pt := 102 []
     loc = 102 && in = 0 && pt = 4 -> loc := 4; in := 1; pt := 102 []
     loc = 103 && in = 0 && pt = 101 -> loc := 101; in := 1; pt := 103 []
     loc = 103 && in = 0 && pt = 102 -> loc := 102; in := 1; pt := 103 []
     loc = 103 && in = 0 && pt = 103 -> loc := 183; in := 1; pt := 103 []
     loc = 103 && in = 0 && pt = 4 -> loc := 4; in := 1; pt := 103 []
   fi
}	