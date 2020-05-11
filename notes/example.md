

Initial Network

++++++++++++++++++++++     
 ipv4.dst |  setnhop          (CONTROL CODE)
++++++++++++++++++++++   ;      if (ipv4.isValid())
   *           nop                 if (ipv4.ttl > 0) ipv4.ttl-- else drop
									
									
									
									
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ipv4.isValid(), ipv4.ttl, ipv4.dst  | {setnhop;ttl_decr, drop}
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(       1          1        *      |     drop  )
        *          *        *             nop
		
		
(Step 1)

++++++++++++++++++++++
 ipv4.dst |  setnhop          (CONTROL CODE)
++++++++++++++++++++++   ;      if (ipv4.isValid())
 10.0.0.1 | setnhop(9)            if (ipv4.ttl > 0) ipv4.ttl-- else drop
    *         miss				
									
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ipv4.isValid(), ipv4.ttl, ipv4.dst  | {setnhop;ttl_decr, URG}
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(   1              0         *      |       URG               )
    1              *       10.0.0.1 |  setnhop(9);ttl_decr
        *          *        *             nop		


Logical:
if ipv4.dst = 10.0.0.1
   setnhop(9)
if ipv4.isValid()
   if ipv4.ttl == 0:
      ipv4.URG = 1
   else
      ipv4.ttl--

Physical:
if ipv4.isValid() && ipv4.ttl == 0
  ipv4.URG = 1
else if
  ipv4.isValid() && ipv4.dst = 10.0.0.1
    setnhop(9);
    ipv4.ttl--
-------------------------------------------------------------

INITIAL
dst           -> out;   src    dst          -> out
10.0.0.10/24     1      

INITIAL
src dst        -> out
*   10.0.0.10/24   1



ADD
dst           -> out;   src    dst          -> out
10.0.0.10/24     1      (7     10.0.0.10/16    CTRLR)

1. DELETE
src dst        -> out


2. INSERT
src dst           -> out
7   10.0.0.10/16     CTRLR


3. INSERT
src dst          ->  out
7   10.0.0.10/16     CTRLR
*   10.0.0.10/24      1
------------------------------------------------------------------------



Initial Network

++++++++++++++++++++++      +++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ipv4.dst |  setnhop            ipv4.valid(), ipv4.ttl   |   {ttldecr, drop, nop*}
++++++++++++++++++++++   ;  +++++++++++++++++++++++++++++++++++++++++++++++++++++++
   *           nop                   *          *              nop


Physical
++++++++++++++++++++++++++++++++++++++++
ipv4.dst | {setnhop;ttldecr, drop, nop*}
++++++++++++++++++++++++++++++++++++++++
   *     |    nop
									
							
--



Step 1

++++++++++++++++++++++      +++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ipv4.dst |  setnhop            ipv4.valid(), ipv4.ttl   |   {ttldecr, drop, nop*}
++++++++++++++++++++++   ;  +++++++++++++++++++++++++++++++++++++++++++++++++++++++
   *           nop            [      1            0      |   drop             ]
                                     *          *              nop


Physical
+++++++++++++++++++++++++++++++++++++++++++++++++++++++
ipv4.valid, ipv4.ttl, ipv4.dst | {setnhop;ttldecr, drop, nop*}
++++++++++++++++++++++++++++++++++++++++++++++++++++++++
[   1           0         *         drop ]
    *           *         *          nop
									
							

Step 1

+++++++++++++++++++++++++      +++++++++++++++++++++++++++++++++++++++++++++++++++++++
 ipv4.dst     |  setnhop            ipv4.valid(), ipv4.ttl   |   {ttldecr, drop, nop*}
+++++++++++++++++++++++++   ;  +++++++++++++++++++++++++++++++++++++++++++++++++++++++
 10.0.0.0/16   setnhop(2)            1          0             drop
    0/0        setnhop(C)       [    1          *             ttldecr   ]
     *         nop                   *          *              nop


Physical
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
ipv4.valid, ipv4.ttl, ipv4.dst   | {setnhop;ttldecr,, drop, nop*}
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    1           0        0/0         drop
    1           *       10../16    setnhop(2);ttldecr 
  [ 1           *         *        setnhop(??); ttldecr ]
    *           *         *          nop
									
							
---------------------------------------------------------------------------------


LOGICAL
OBT

smac dmac | egress
0:1   0:2 | DROP
[*    0:2 | 8 ]



Phys1
dmac         | egress          ;   smac dmac   |   egress
 *              nop                0:1  0:2          DROP
                                    
                                    

Phys2
smac dmac  | egress   ;  dmac | egress
0:1  0:2      DROP           *      nop





-------------------------

ipdst -> out
ipsrc -> smac
dmac -> ipdst
dmac -> out
ttl -> out
--

Log1
ipdst,ipsrc,dmac,ttl -> smac ipdst out

Log2
inport; ipdst, ipsrc, dmac -> smac ipdst out; ttl -> out


Phys1
dmac -> {{out,ipdst},{out},{}};
ipsrc,ipdst -> out,smac;
ttl -> out

Phys2
dmac -> out,ipdst; ipsrc,ipdst,ttl -> out,smac
                    11    10           99			  
                    12    10            5
Phys3
"pathing"                "metadata"
dmac,ipdst -> out,ipdst; ipsrc,ttl -> out,smac
 8    10       99			  
 9    10       5
