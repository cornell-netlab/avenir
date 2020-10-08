// ------------------------------
//     Header Types
// ------------------------------

header_type srcMac_t {
    fields {
        srcMac  : 48;
     }
}

header_type nextHop_t nextHop {
    fields {
    	isValid : 1;
        port : 5;
    }
}

//----------------------------------
//    The Headers (Matching OF 1.0)
//----------------------------------

header_type srcMac_t srcMac;
header_type nextHop_t nextHop[6];


//----------------------------------
//   The Parser
//----------------------------------

parser start {
    extract(srcMac);
    // set counter to 0
    return select(current(0,1)) { //check isValidBit
        0 : ingress;
	1 : parse_nextHop;
    }
}

parser parse_nextHop {
    extract(nextHop[next]);
    // increment counter
    return select(current(0,1)) { // check isValid bit
        0 : ingress;
	1 : parse_nextHop;
    }
}


// -----------------------------
//          Drop
// -----------------------------

action _drop(){
    drop;
}

table drop_table {
   actions { _drop };
   default_action : _drop;
}


// -----------------------------
//            Next Hop
// -----------------------------


action next_hop () {
  modify_field(intrinsic_metadata.egress_port, nextHop.port);
  pop(nextHop,1);
}

table next_hop_table {
    actions { next_hop };
    default_action : next_hop;
}

// ------------------------------
//       Control
// ------------------------------

control ingress {
    if (valid(nextHop[0])) {
        apply(next_hop_table);
    } else {
        apply(drop_table);
   }
}