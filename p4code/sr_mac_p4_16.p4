#include <core.p4>
#include <v1model.p4>

// ------------------------------
//     Header Types
// ------------------------------

header srcMac_h {
    bit<48> srcMAC;
}

header nextHop_h {
    bit<1> isValid;
    bit<7> port;
}

//----------------------------------
//    The Headers (Matching OF 1.0)
//----------------------------------

struct headers {
    srcMac_h srcMac;
    nextHop_h[6] nextHops;
}

struct user_metadata_t {}

//----------------------------------
//   The Parser
//----------------------------------
parser ParserImpl(packet_in pin, out headers hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    state start {
        pin.extract(hdr.srcMac);
        pin.extract(hdr.nextHops.next);
        // set counter to 0
        transition select(hdr.nextHops.last.isValid()) { //check isValidBit
            true : parse_nextHop;
        }
    }

    state parse_nextHop {
        pin.extract(hdr.nextHops.next);
        // increment counter
        transition select(hdr.nextHops.last.isValid()) { // check isValid bit
            false : accept;
            true : parse_nextHop;
        }
    }
}

control ingress(inout headers hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    action next_hop () {
        standard_metadata.egress_spec = (bit<9>)hdr.nextHops[0].port;
        hdr.nextHops.pop_front(1);
    }

    table next_hop_table {
        actions = { 
            next_hop();
        }
        default_action = next_hop();
    }

    apply {
        next_hop_table.apply();
    }
}

control egress(inout headers hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    apply {}
}


control DeparserImpl(packet_out po, in headers hdr) {
    apply {
        po.emit(hdr.srcMac);
        po.emit(hdr.nextHops);
    }
}

control verifyChecksum(inout headers hdr, inout user_metadata_t meta) {
    apply {
    }
}

control computeChecksum(inout headers hdr, inout user_metadata_t meta) {
    apply {
    }
}

V1Switch(ParserImpl(), verifyChecksum(), ingress(), egress(), computeChecksum(), DeparserImpl()) main;
