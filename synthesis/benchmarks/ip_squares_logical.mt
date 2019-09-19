{  ipDst = ??1
 & ipDst = ??3
 & ??3ipDst = alpha
 & ??3srcMac = sigma
 & ??3dstMac = ??5
 & ??3dstMac = delta
 }
loc := 0;
{ loc = 0 && ipDst = ??1 & ipDst = ??3 & ??3srcMac = sigma && ??3dstMac = ??5 && ??3dstMac = delta & ??3ipDst = delta}
if total
   loc = 0 && ipDst = ??1 -> srcMac := ??11; loc := 101 []
   loc = 0 && ipDst = ??2 -> srcMac := ??22; loc := 11 []
fi ;
{loc = 101 & ipDst = ??3 & ??3srcMac = sigma && ??3dstMac = ??5 && ??3dstMac = delta & ??3ipDst = alpha}
if total
   loc = 101 && ipDst = ??3 -> srcMac := ??3srcMac; ipDst := ??3ipDst; dstMac := ??3dstMac; loc := 1 []
   loc = 101 && ipDst = ??4 -> srcMac := ??4srcMac; ipDst := ??4ipDst; dstMac := ??4dstMac; loc := 0 []
fi ;
{ loc = 1 & dstMac = ??5 & dstMac = delta & ipDst = alpha & srcMac = sigma}
if total
   loc = 1 && dstMac = ??5 -> loc := 10 []
   loc = 1 && dstMac = ??6 -> loc := 101 []
fi
assert (loc = 10)
{ ipDst = alpha & srcMac = sigma & loc = gamma & dstMac = delta}