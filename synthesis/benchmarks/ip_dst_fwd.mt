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
     loc = 1 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 2 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 3 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 4 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 101 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 102 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []
     loc = 103 && dstIP = ?? && in = 1 -> dstMAC := ??; pt := ??; in := 0 []

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