#include <core.p4>
#define V1MODEL_VERSION 20200408
#include <v1model.p4>

header ethernet_t {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

header header_0_t {
    bit<16> field_0;
    bit<16> field_1;
    bit<16> field_2;
}

header ptp_t {
    bit<4>  transportSpecific;
    bit<4>  messageType;
    bit<4>  reserved;
    bit<4>  versionPTP;
    bit<16> messageLength;
    bit<8>  domainNumber;
    bit<8>  reserved2;
    bit<16> flags;
    bit<64> correction;
    bit<32> reserved3;
    bit<80> sourcePortIdentity;
    bit<16> sequenceId;
    bit<8>  PTPcontrol;
    bit<8>  logMessagePeriod;
    bit<80> originTimestamp;
}

struct metadata {
}

struct headers {
    @name(".ethernet") 
    ethernet_t ethernet;
    @name(".header_0") 
    header_0_t header_0;
    @name(".ptp") 
    ptp_t      ptp;
}

parser ParserImpl(packet_in packet, out headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata) {
    @name(".parse_ethernet") state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.etherType) {
            16w0x88f7: parse_ptp;
            default: accept;
        }
    }
    @name(".parse_header_0") state parse_header_0 {
        packet.extract(hdr.header_0);
        transition select(hdr.header_0.field_0) {
            default: accept;
        }
    }
    @name(".parse_ptp") state parse_ptp {
        packet.extract(hdr.ptp);
        transition select(hdr.ptp.reserved2) {
            8w1: parse_header_0;
            default: accept;
        }
    }
    @name(".start") state start {
        transition parse_ethernet;
    }
}

control ingress(inout headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata) {
    @name(".forward") action forward(bit<9> port) {
        standard_metadata.egress_spec = port;
    }
    @name("._drop") action _drop() {
        mark_to_drop(standard_metadata);
    }
    @name("._nop") action _nop() {
    }
    @name(".mod_headers") action mod_headers() {
        hdr.header_0.field_0 = 16w1;
        hdr.header_0.field_1 = hdr.header_0.field_0;
        hdr.header_0.field_2 = hdr.header_0.field_1;
    }
    @name(".forward_table") table forward_table {
        actions = {
            forward;
            _drop;
        }
        key = {
            hdr.ethernet.dstAddr: exact;
        }
        size = 4;
    }
    @name(".test_tbl") table test_tbl {
        actions = {
            _nop;
            mod_headers;
        }
        key = {
            hdr.ptp.reserved2: exact;
        }
        size = 4;
    }
    apply {
        forward_table.apply();
        test_tbl.apply();
    }
}

control egress(inout headers hdr, inout metadata meta, inout standard_metadata_t standard_metadata) {
    apply {
    }
}

control DeparserImpl(packet_out packet, in headers hdr) {
    apply {
        packet.emit(hdr.ethernet);
        packet.emit(hdr.ptp);
        packet.emit(hdr.header_0);
    }
}

control verifyChecksum(inout headers hdr, inout metadata meta) {
    apply {
    }
}

control computeChecksum(inout headers hdr, inout metadata meta) {
    apply {
    }
}

V1Switch(ParserImpl(), verifyChecksum(), ingress(), egress(), computeChecksum(), DeparserImpl()) main;

