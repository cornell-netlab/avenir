#include <core.p4>
#include <v1model.p4>

const bit<16> TYPE_IPV4 = 0x800;
const bit<16> TYPE_SRCROUTING = 0x1234;

#define MAX_HOPS 8

/*************************************************************************
*********************** H E A D E R S  ***********************************
*************************************************************************/

typedef bit<9>  egressSpec_t;
typedef bit<48> macAddr_t;
typedef bit<32> ip4Addr_t;

header ethernet_t {
    macAddr_t dstAddr;
    macAddr_t srcAddr;
    bit<16>   etherType;
}

header srcRoute_t {
    bit<1>    bos;
    bit<15>   port;
}

header ethType_t {
    bit<16>   etherType;
}

header ipv4_t {
    bit<4>    version;
    bit<4>    ihl;
    bit<8>    diffserv;
    bit<16>   totalLen;
    bit<16>   identification;
    bit<3>    flags;
    bit<13>   fragOffset;
    bit<8>    ttl;
    bit<8>    protocol;
    bit<16>   hdrChecksum;
    ip4Addr_t srcAddr;
    ip4Addr_t dstAddr;
}

struct metadata {
    /* empty */
}

struct headers {
    ethernet_t              ethernet;
    srcRoute_t[MAX_HOPS]    srcRoutes;
    ethType_t               ethType;
    ipv4_t                  ipv4;
}

/*************************************************************************
*********************** P A R S E R  ***********************************
*************************************************************************/

parser MyParser(packet_in packet,
                out headers hdr,
                inout metadata meta,
                inout standard_metadata_t standard_metadata) {

    state start {
        transition parse_ethernet;
    }

    state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.etherType) {
            TYPE_SRCROUTING: parse_srcRouting;
        }
    }

    state parse_srcRouting {
        packet.extract(hdr.srcRoutes.next);
        transition select(hdr.srcRoutes.last.bos) {
            1: parse_ethType;
            default: parse_srcRouting;
        }
    }

    state parse_ethType {
        packet.extract(hdr.ethType);
        transition select(hdr.ethType.etherType) {
            TYPE_IPV4: parse_ipv4;
            default: accept;
        }
    }

    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        transition accept;
    }

}


/*************************************************************************
************   C H E C K S U M    V E R I F I C A T I O N   *************
*************************************************************************/

control MyVerifyChecksum(inout headers hdr, inout metadata meta) {
    apply {
    }
}

/*************************************************************************
**************  I N G R E S S   P R O C E S S I N G   *******************
*************************************************************************/

control MyIngress(inout headers hdr,
                  inout metadata meta,
                  inout standard_metadata_t standard_metadata) {

    bit<1> flagSrcMac = 0;
    bit<1> flagDstMac = 0;
    //bit<1> flagEthType = 0;
    bit<1> flagSrcHdr0 = 0;
    bit<1> flagSrcHdr1 = 0;
    bit<1> flagSrcHdr2 = 0;
    bit<1> flagSrcHdr3 = 0;
    bit<1> flagSrcHdr4 = 0;
    bit<1> flagSrcHdr5 = 0;
    bit<1> flagSrcHdr6 = 0;
    bit<1> flagSrcHdr7 = 0;
    macAddr_t srcMac = 0;
    macAddr_t destMac = 0;
    bit<16> eType = 0;

    bit<15> p0 = 0;
    bit<15> p1 = 0;
    bit<15> p2 = 0;
    bit<15> p3 = 0;
    bit<15> p4 = 0;
    bit<15> p5 = 0;
    bit<15> p6 = 0;
    bit<15> p7 = 0;

    bit<1> b0 = 0;
    bit<1> b1 = 0;
    bit<1> b2 = 0;
    bit<1> b3 = 0;
    bit<1> b4 = 0;
    bit<1> b5 = 0;
    bit<1> b6 = 0;
    bit<1> b7 = 0;

    action drop() {
        mark_to_drop();
    }

    action srcRoute_finish() {
        standard_metadata.egress_spec = (bit<9>)hdr.srcRoutes[0].port;
        hdr.srcRoutes.pop_front(1);
        hdr.ethernet.etherType = hdr.ethType.etherType;
        hdr.ethType.setInvalid();
    }

    action update_ttl(){
        hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
    }

    action get_hdr_data(bit<1> modSrcMac, macAddr_t srcEthAddr,
                          bit<1> modDstMac, macAddr_t dstEthAddr,
                          //bit<1> modEthType, bit<16> etherType,
                          bit<1> valid0, bit<1> bos0, bit<15> port0,
                          bit<1> valid1, bit<1> bos1, bit<15> port1,
                          bit<1> valid2, bit<1> bos2, bit<15> port2,
                          bit<1> valid3, bit<1> bos3, bit<15> port3,
                          bit<1> valid4, bit<1> bos4, bit<15> port4,
                          bit<1> valid5, bit<1> bos5, bit<15> port5,
                          bit<1> valid6, bit<1> bos6, bit<15> port6,
                          bit<1> valid7, bit<1> bos7, bit<15> port7,
                          bit<9> egress_spec) {
        flagSrcMac = modSrcMac;
        flagDstMac = modDstMac;
        srcMac = srcEthAddr;
        destMac = dstEthAddr;
        standard_metadata.egress_spec = egress_spec;

        flagSrcHdr0 = valid0;
        flagSrcHdr1 = valid1;
        flagSrcHdr2 = valid2;
        flagSrcHdr3 = valid3;
        flagSrcHdr4 = valid4;
        flagSrcHdr5 = valid5;
        flagSrcHdr6 = valid6;
        flagSrcHdr7 = valid7;
        b0 = bos0;
        b1 = bos1;
        b2 = bos2;
        b3 = bos3;
        b4 = bos4;
        b5 = bos5;
        b6 = bos6;
        b7 = bos7;
        p0 = port0;
        p1 = port1;
        p2 = port2;
        p3 = port3;
        p4 = port4;
        p5 = port5;
        p6 = port6;
        p7 = port7;
    }

    table encapsulate {
        key = {
            standard_metadata.ingress_port  : ternary;
            hdr.ethernet.dstAddr            : ternary;
            hdr.ethernet.srcAddr            : ternary;
            hdr.ethernet.etherType          : ternary;
            hdr.ipv4.protocol               : ternary;
            hdr.ipv4.srcAddr                : ternary;
            hdr.ipv4.dstAddr                : ternary;
        }
        actions = {
            drop;
            get_hdr_data;
        }
        default_action = drop();
    }

    action push_srcRoute_header(bit<1> bos, bit<15> port) {
        hdr.ethType.setValid();
        hdr.srcRoutes.push_front(1);
        hdr.srcRoutes[0].setValid();
        hdr.srcRoutes[0].bos = bos;
        hdr.srcRoutes[0].port = port;
    }
    apply {
        if (hdr.srcRoutes[0].isValid()){ // core-to-hosts packets
            srcRoute_finish();
        } else { // hosts-to-core packets
            encapsulate.apply();
            if (flagSrcMac == 1)
                hdr.ethernet.srcAddr = srcMac;
            if (flagDstMac == 1)
                hdr.ethernet.dstAddr = destMac;
            if (flagSrcHdr7 == 1)
                push_srcRoute_header(b7, p7);
            if (flagSrcHdr6 == 1)
                push_srcRoute_header(b6, p6);
            if (flagSrcHdr5 == 1)
                push_srcRoute_header(b5, p5);
            if (flagSrcHdr4 == 1)
                push_srcRoute_header(b4, p4);
            if (flagSrcHdr3 == 1)
                push_srcRoute_header(b3, p3);
            if (flagSrcHdr2 == 1)
                push_srcRoute_header(b2, p2);
            if (flagSrcHdr1 == 1)
                push_srcRoute_header(b1, p1);
            if (flagSrcHdr0 == 1)
                push_srcRoute_header(b0, p0);
            if (hdr.ethType.isValid()) {
                hdr.ethType.etherType = hdr.ethernet.etherType;
                hdr.ethernet.etherType = TYPE_SRCROUTING;
            }
        }
    }
}

/*************************************************************************
****************  E G R E S S   P R O C E S S I N G   *******************
*************************************************************************/

control MyEgress(inout headers hdr,
                 inout metadata meta,
                 inout standard_metadata_t standard_metadata) {
    apply {  }
}

/*************************************************************************
*************   C H E C K S U M    C O M P U T A T I O N   **************
*************************************************************************/

control MyComputeChecksum(inout headers  hdr, inout metadata meta) {
    apply {
        update_checksum(hdr.ipv4.isValid() && hdr.ipv4.ihl == 5,
            {
                hdr.ipv4.version,
                hdr.ipv4.ihl,
                hdr.ipv4.diffserv,
                hdr.ipv4.totalLen,
                hdr.ipv4.identification,
                hdr.ipv4.flags,
                hdr.ipv4.fragOffset,
                hdr.ipv4.ttl,
                hdr.ipv4.protocol,
                hdr.ipv4.srcAddr,
                hdr.ipv4.dstAddr
            },hdr.ipv4.hdrChecksum, HashAlgorithm.csum16);
    }
}

/*************************************************************************
***********************  D E P A R S E R  *******************************
*************************************************************************/

control MyDeparser(packet_out packet, in headers hdr) {
    apply {
        packet.emit(hdr.ethernet);
        packet.emit(hdr.srcRoutes);
        packet.emit(hdr.ethType);
        packet.emit(hdr.ipv4);
    }
}

/*************************************************************************
***********************  S W I T C H  *******************************
*************************************************************************/

V1Switch(MyParser(), MyVerifyChecksum(), MyIngress(), MyEgress(), MyComputeChecksum(), MyDeparser()) main;
