#include <core.p4>
#include <v1model.p4>

// ------------------------------
//     Header Types
// ------------------------------

header srcMac_h {
    bit<48> srcMAC;
    bit<48> destMAC;
}

header nextHop_h {
    bit<1> val;
    bit<7> outPort;
    bit<40> unused;
}

//----------------------------------
//    The Headers (Matching OF 1.0)
//----------------------------------

struct headers {
    srcMac_h srcMac;
    nextHop_h nextHop;
}

struct user_metadata_t {}

//----------------------------------
//   The Parser
//----------------------------------
parser ParserImpl(packet_in pin, out headers hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    state start {
        pin.extract(hdr.srcMac);
        pin.extract(hdr.nextHop);
        transition accept;
    }

}

control ingress(inout headers hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    action next_hop () {
        standard_metadata.egress_spec = (bit<9>)hdr.nextHop.outPort;
        hdr.nextHop.val = hdr.nextHop.unused[0:0];
        hdr.nextHop.outPort= hdr.nextHop.unused[7:1];
        hdr.nextHop.unused = hdr.nextHop.unused << 8;
        
    }
    action drop_action() {
        mark_to_drop();
    }
    table next_hop_table {
        key = {
            hdr.nextHop.val: exact;
        }
        actions = { 
            next_hop();
            drop_action();
        }
        const entries = {
            1 : next_hop();
        }
        default_action = drop_action();
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
        po.emit(hdr.nextHop);
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
