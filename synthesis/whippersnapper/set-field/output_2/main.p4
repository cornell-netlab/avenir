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
header_type ptp_t {
    fields {
        transportSpecific : 4;
        messageType       : 4;
        reserved          : 4;
        versionPTP        : 4;
        messageLength     : 16;
        domainNumber      : 8;
        reserved2         : 8;
        flags             : 16;
        correction        : 64;
        reserved3         : 32;
        sourcePortIdentity: 80;
        sequenceId        : 16;
        PTPcontrol        : 8;
        logMessagePeriod  : 8;
        originTimestamp   : 80;
    }
}
parser start { return parse_ethernet; }
header ethernet_t ethernet;

parser parse_ethernet {
    extract(ethernet);
    return select(latest.etherType) {
	ETHERTYPE_PTP: parse_ptp;
	default : ingress;

    }
}
header ptp_t ptp;

parser parse_ptp {
    extract(ptp);
    return select(latest.reserved2) {
	1       : parse_header_0;
	default : ingress;

    }
}
header_type header_0_t {
    fields {
		field_0 : 16;
		field_1 : 16;

    }
}
header header_0_t header_0;

parser parse_header_0 {
    extract(header_0);
    return select(latest.field_0) {
	default : ingress;

    }
}
action _nop() {

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
action mod_headers() {
	modify_field(header_0.field_0, 1);
	modify_field(header_0.field_1, header_0.field_0);

}
table test_tbl {
    reads {
        ptp.reserved2 : exact;
    } actions {
        		_nop;
		mod_headers;
    }
    size : 4;
}
control ingress {
    apply(forward_table);
    apply(test_tbl);

}
