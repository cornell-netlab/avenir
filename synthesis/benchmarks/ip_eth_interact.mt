assert (loc = 81);
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
     loc = 1 && dstIP = ?__ip1 && in = 1 -> loc := 1; srcMAC := ?__mac1; pt := ?__pt1; in := 0 []
     loc = 2 && dstIP = ?__ip1 && in = 1 -> loc := 2; srcMAC := ?__mac2; pt := ?__pt2; in := 0 []
     loc = 3 && dstIP = ?__ip1 && in = 1 -> loc := 3; srcMAC := ?__mac3; pt := ?__pt3; in := 0 []
     loc = 4 && dstIP = ?__ip1 && in = 1 -> loc := 4; srcMAC := ?__mac4; pt := ?__pt4; in := 0 []
     
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
     loc = 4 && in = 0 && pt = 4 -> loc := 50; in := 1; pt := 0 []
     
     loc = 50 && in = 1 && pt = 0 && dstIP = ?__ip50_test -> dstMAC := ?__mac50_set; loc := 104 []
     loc = 50 && in = 1 && pt = 1 && dstMAC = ?__mac50_test -> dstIP := ?__ip50_set; loc := 4 []

     loc = 101 && in = 1 && dstMAC = ?__mac101 -> loc := 101; pt := ?__pt101; in := 0 []
     loc = 102 && in = 1 && dstMAC = ?__mac102 -> loc := 102; pt := ?__pt102; in := 0 []
     loc = 103 && in = 1 && dstMAC = ?__mac103 -> loc := 103; pt := ?__pt103; in := 0 []
     loc = 104 && in = 1 && dstMAC = ?__mac104 -> loc := 104; pt := ?__pt104; in := 0 []

     loc = 101 && in = 0 && pt = 1 -> loc := 181; in := 1; pt := 1 []
     loc = 101 && in = 0 && pt = 2 -> loc := 102; in := 1; pt := 1 []
     loc = 101 && in = 0 && pt = 3 -> loc := 103; in := 1; pt := 1 []
     loc = 101 && in = 0 && pt = 4 -> loc := 104; in := 1; pt := 1 []
     loc = 102 && in = 0 && pt = 1 -> loc := 101; in := 1; pt := 2 []
     loc = 102 && in = 0 && pt = 1 -> loc := 182; in := 1; pt := 2 []
     loc = 102 && in = 0 && pt = 3 -> loc := 103; in := 1; pt := 2 []
     loc = 102 && in = 0 && pt = 4 -> loc := 104; in := 1; pt := 2 []
     loc = 103 && in = 0 && pt = 1 -> loc := 101; in := 1; pt := 3 []
     loc = 103 && in = 0 && pt = 2 -> loc := 102; in := 1; pt := 3 []
     loc = 103 && in = 0 && pt = 3 -> loc := 183; in := 1; pt := 3 []
     loc = 103 && in = 0 && pt = 4 -> loc := 104; in := 1; pt := 3 []
     loc = 104 && in = 0 && pt = 1 -> loc := 101; in := 1; pt := 4 []
     loc = 104 && in = 0 && pt = 2 -> loc := 102; in := 1; pt := 4 []
     loc = 104 && in = 0 && pt = 3 -> loc := 103; in := 1; pt := 4 []
     loc = 104 && in = 0 && pt = 4 -> loc := 50; in := 1; pt := 1 []
   fi
};
assert (in = 1)