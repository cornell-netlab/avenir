#include <core.p4>
#include <v1model.p4>

const bit<9> CPU_PORT = 255;

header ethernet_t {
    bit<48> dst_addr;
    bit<48> src_addr;
    bit<16> ether_type;
}

// Packet-in header. Prepended to packets sent to the controller and used to
// carry the original ingress port where the packet was received.
@controller_header("packet_in")
header packet_in_header_t {
    bit<9> ingress_port;
    bit<7> _padding;
}

// Packet-out header. Prepended to packets received by the controller and used
// to tell the switch on which port this packet should be forwarded.
@controller_header("packet_out")
header packet_out_header_t {
    bit<9> egress_port;
    bit<7> _padding;
}

struct headers_t {
    ethernet_t ethernet;
    packet_out_header_t packet_out;
    packet_in_header_t packet_in;
}

struct user_metadata_t {}

parser ParserImpl(packet_in pin, out headers_t hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    state start {
        transition select(standard_metadata.ingress_port) {
            CPU_PORT: parse_packet_out;
            default: parse_ethernet;
        }
    }

    state parse_packet_out {
        pin.extract(hdr.packet_out);
        transition parse_ethernet;
    }

    state parse_ethernet {
        pin.extract(hdr.ethernet);
        transition accept;
    }

}

control ingress(inout headers_t hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    action send_to_cpu() {
        standard_metadata.egress_spec = CPU_PORT;
        hdr.packet_in.setValid();
        hdr.packet_in.ingress_port = standard_metadata.ingress_port;
    }

    action set_out_port(bit<9> port) {
        standard_metadata.egress_spec = port;
    }

    action _drop() {
        mark_to_drop();
    }

    table l2_for_lldp {
        key = {
            standard_metadata.ingress_port  : ternary;
            hdr.ethernet.dst_addr           : ternary;
            hdr.ethernet.src_addr           : ternary;
            hdr.ethernet.ether_type         : ternary;
        }
        actions = {
            set_out_port;
            send_to_cpu;
            _drop;
            NoAction;
        }
        default_action = NoAction();
    }

    apply {
        if (standard_metadata.ingress_port == CPU_PORT) {
            standard_metadata.egress_spec = hdr.packet_out.egress_port;
            hdr.packet_out.setInvalid();
        } else {
            if (hdr.ethernet.ether_type == 0x88cc ||  hdr.ethernet.ether_type == 0x8999) {
                l2_for_lldp.apply();
            } else {
                // core_next_hop.apply();
                if (hdr.ethernet.dst_addr[0:0] == 0b1) {
                    standard_metadata.egress_spec = (bit<9>)hdr.ethernet.dst_addr[7:1];
                    hdr.ethernet.dst_addr = hdr.ethernet.dst_addr << 8;
                } else {
                    _drop();
                }
            }
        }
    }
}

control egress(inout headers_t hdr, inout user_metadata_t meta, inout standard_metadata_t standard_metadata) {
    apply {}
}


control DeparserImpl(packet_out po, in headers_t hdr) {
    apply {
        po.emit(hdr.packet_in);
        po.emit(hdr.ethernet);
    }
}

control verifyChecksum(inout headers_t hdr, inout user_metadata_t meta) {
    apply {
    }
}

control computeChecksum(inout headers_t hdr, inout user_metadata_t meta) {
    apply {
    }
}

V1Switch(ParserImpl(), verifyChecksum(), ingress(), egress(), computeChecksum(), DeparserImpl()) main;
