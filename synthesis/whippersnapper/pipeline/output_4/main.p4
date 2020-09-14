#define ETHERTYPE_IPV4 0x0800
#define ETHERTYPE_PTP 0x088F7

#define TCP_PROTOCOL 0x06
#define UDP_PROTOCOL 0x11
#define GENERIC_PROTOCOL 0x9091
header_type ethernet_t {
    fields {
        dstAddr : 48;
        srcAddr : 48;
        etherType : 16;
    }
}
header ethernet_t ethernet;

parser start {
    return parse_ethernet;
}

parser parse_ethernet {
    extract(ethernet);
    return select(latest.etherType) {
        ETHERTYPE_IPV4 : parse_ipv4; 
        default : ingress;
    }
}
header_type ipv4_t {
    fields {
        version : 4;
        ihl : 4;
        diffserv : 8;
        totalLen : 16;
        identification : 16;
        flags : 3;
        fragOffset : 13;
        ttl : 8;
        protocol : 8;
        hdrChecksum : 16;
        srcAddr : 32;
        dstAddr : 32;
    }
}
header ipv4_t ipv4;

parser parse_ipv4 {
    extract(ipv4);
    return select(latest.protocol) {
        TCP_PROTOCOL : parse_tcp;
        UDP_PROTOCOL : parse_udp;
        default : ingress;
    }
}
header_type tcp_t {
    fields {
        srcPort : 16;
        dstPort : 16;
        seqNo : 32;
        ackNo : 32;
        dataOffset : 4;
        res : 3;
        ecn : 3;
        ctrl : 6;
        window : 16;
        checksum : 16;
        urgentPtr : 16;
    }
}
header tcp_t tcp;

parser parse_tcp {
    extract(tcp);
    return ingress;
}
header_type udp_t {
    fields {
        srcPort : 16;
        dstPort : 16;
        length_ : 16;
        checksum : 16;
    }
}
header udp_t udp;

parser parse_udp {
    extract(udp);
    return select(latest.dstPort) {
	default : ingress;

    }
}
action _drop() {
    drop();
}

action forward(port) {
    modify_field(standard_metadata.egress_spec, port);
}

table forward_table {
    reads {
        ethernet.dstAddr : exact;
    } actions {
        forward;
        _drop;
    }
    size : 4;
}
action _nop() {

}
action forward1(_port) {
modify_field(standard_metadata.egress_spec, _port);
}
table table_1 {
    reads {
        ethernet.dstAddr : exact;
    } actions {
        forward1;
    }
    size : 16;
}
action forward2(_port) {
modify_field(standard_metadata.egress_spec, _port);
}
table table_2 {
    reads {
        ethernet.dstAddr : exact;
    } actions {
        forward2;
    }
    size : 16;
}
action forward3(_port) {
modify_field(standard_metadata.egress_spec, _port);
}
table table_3 {
    reads {
        ethernet.dstAddr : exact;
    } actions {
        forward3;
    }
    size : 16;
}
control ingress {
    apply(forward_table);
    apply(table_1);
	apply(table_2);
	apply(table_3);
	
}
