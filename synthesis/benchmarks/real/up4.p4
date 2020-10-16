#include <core.p4>
#define V1MODEL_VERSION 20180101
#include <v1model.p4>

const bit<4> IP_VERSION_4 = 4;
const bit<8> DEFAULT_IPV4_TTL = 64;
typedef bit<9> port_num_t;
typedef bit<48> mac_addr_t;
typedef bit<32> ipv4_addr_t;
const bit<16> UDP_PORT_GTPU = 2152;
const bit<3> GTPU_VERSION = 0x1;
const bit<1> GTP_PROTOCOL_TYPE_GTP = 0x1;
const bit<8> GTP_MESSAGE_TYPE_UPDU = 0xff;
typedef bit<32> far_info_id_t;
typedef bit<32> pdr_id_t;
typedef bit<32> far_id_t;
typedef bit<32> qer_id_t;
typedef bit<32> bar_id_t;
typedef bit<32> qfi_t;
typedef bit<32> net_instance_t;
typedef bit<32> counter_index_t;
typedef bit<32> teid_t;
typedef bit<64> seid_t;
typedef bit<64> fteid_t;
typedef bit<96> fseid_t;
const pdr_id_t DEFAULT_PDR_ID = 0;
const far_id_t DEFAULT_FAR_ID = 0;
const qer_id_t DEFAULT_QER_ID = 0;
const qfi_t DEFAULT_QFI = 0;
const fseid_t DEFAULT_FSEID = 0;
enum bit<8> GTPUMessageType {
    GPDU = 255
}

enum bit<16> EtherType {
    IPV4 = 0x800,
    IPV6 = 0x86dd
}

enum bit<8> IpProtocol {
    ICMP = 1,
    TCP = 6,
    UDP = 17
}

enum bit<16> L4Port {
    DHCP_SERV = 67,
    DHCP_CLIENT = 68,
    GTP_GPDU = 2152,
    IPV4_IN_UDP = 9875
}

enum bit<8> Direction {
    UNKNOWN = 0x0,
    UPLINK = 0x1,
    DOWNLINK = 0x2,
    OTHER = 0x3
}

enum bit<8> InterfaceType {
    UNKNOWN = 0x0,
    ACCESS = 0x1,
    CORE = 0x2,
    N6_LAN = 0x3,
    VN_INTERNAL = 0x4,
    CONTROL_PLANE = 0x5
}

enum bit<8> TunnelType {
    UNKNOWN = 0x0,
    IP = 0x1,
    UDP = 0x2,
    GTPU = 0x3
}

header ethernet_t {
    mac_addr_t dst_addr;
    mac_addr_t src_addr;
    EtherType  ether_type;
}

header ipv4_t {
    bit<4>      version;
    bit<4>      ihl;
    bit<6>      dscp;
    bit<2>      ecn;
    bit<16>     total_len;
    bit<16>     identification;
    bit<3>      flags;
    bit<13>     frag_offset;
    bit<8>      ttl;
    IpProtocol  proto;
    bit<16>     checksum;
    ipv4_addr_t src_addr;
    ipv4_addr_t dst_addr;
}


header tcp_t {
    L4Port  sport;
    L4Port  dport;
    bit<32> seq_no;
    bit<32> ack_no;
    bit<4>  data_offset;
    bit<3>  res;
    bit<3>  ecn;
    bit<6>  ctrl;
    bit<16> window;
    bit<16> checksum;
    bit<16> urgent_ptr;
}

header udp_t {
    L4Port  sport;
    L4Port  dport;
    bit<16> len;
    bit<16> checksum;
}

header icmp_t {
    bit<8>  icmp_type;
    bit<8>  icmp_code;
    bit<16> checksum;
    bit<16> identifier;
    bit<16> sequence_number;
    bit<64> timestamp;
}

header gtpu_t {
    bit<3>          version;
    bit<1>          pt;
    bit<1>          spare;
    bit<1>          ex_flag;
    bit<1>          seq_flag;
    bit<1>          npdu_flag;
    GTPUMessageType msgtype;
    bit<16>         msglen;
    teid_t          teid;
}

@controller_header("packet_out") header packet_out_t {
    port_num_t egress_port;
    bit<7>     _pad;
}

@controller_header("packet_in") header packet_in_t {
    port_num_t ingress_port;
    bit<7>     _pad;
}

struct parsed_headers_t {
    packet_out_t packet_out;
    packet_in_t  packet_in;
    ethernet_t   ethernet;
    ipv4_t       outer_ipv4;
    udp_t        outer_udp;
    gtpu_t       gtpu;
    ipv4_t       ipv4;
    udp_t        udp;
    tcp_t        tcp;
    icmp_t       icmp;
    ipv4_t       inner_ipv4;
    udp_t        inner_udp;
    tcp_t        inner_tcp;
    icmp_t       inner_icmp;
}

struct pdr_metadata_t {
    pdr_id_t        id;
    counter_index_t ctr_idx;
}

struct bar_metadata_t {
    bar_id_t id;
}

struct far_metadata_t {
    far_id_t    id;
    bool        needs_dropping;
    bool        needs_tunneling;
    bool        notify_cp;
    TunnelType  tunnel_out_type;
    ipv4_addr_t tunnel_out_src_ipv4_addr;
    ipv4_addr_t tunnel_out_dst_ipv4_addr;
    L4Port      tunnel_out_udp_dport;
    teid_t      tunnel_out_teid;
    ipv4_addr_t next_hop_ip;
}

struct qos_metadata_t {
    qer_id_t qer_id;
    qfi_t    qfi;
}

struct local_metadata_t {
    Direction      direction;
    teid_t         teid;
    fseid_t        fseid;
    ipv4_addr_t    next_hop_ip;
    bool           needs_gtpu_decap;
    bool           needs_udp_decap;
    bool           needs_vlan_removal;
    InterfaceType  src_iface;
    InterfaceType  dst_iface;
    ipv4_addr_t    ue_addr;
    ipv4_addr_t    inet_addr;
    L4Port         ue_l4_port;
    L4Port         inet_l4_port;
    L4Port         l4_sport;
    L4Port         l4_dport;
    net_instance_t net_instance;
    far_metadata_t far;
    qos_metadata_t qos;
    pdr_metadata_t pdr;
    bar_metadata_t bar;
}

parser ParserImpl(packet_in packet, out parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    state start {
        transition select(std_meta.ingress_port) {
            255: parse_packet_out;
            default: parse_ethernet;
        }
    }
    state parse_packet_out {
        packet.extract(hdr.packet_out);
        transition parse_ethernet;
    }
    state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.ether_type) {
            EtherType.IPV4: parse_ipv4;
            default: accept;
        }
    }
    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        transition select(hdr.ipv4.proto) {
            IpProtocol.UDP: parse_udp;
            IpProtocol.TCP: parse_tcp;
            IpProtocol.ICMP: parse_icmp;
            default: accept;
        }
    }
    state parse_udp {
        packet.extract(hdr.udp);
        local_meta.l4_sport = hdr.udp.sport;
        local_meta.l4_dport = hdr.udp.dport;
        transition select(hdr.udp.dport) {
            L4Port.IPV4_IN_UDP: parse_inner_ipv4;
            L4Port.GTP_GPDU: parse_gtpu;
            default: accept;
        }
    }
    state parse_tcp {
        packet.extract(hdr.tcp);
        local_meta.l4_sport = hdr.tcp.sport;
        local_meta.l4_dport = hdr.tcp.dport;
        transition accept;
    }
    state parse_icmp {
        packet.extract(hdr.icmp);
        transition accept;
    }
    state parse_gtpu {
        packet.extract(hdr.gtpu);
        local_meta.teid = hdr.gtpu.teid;
        transition parse_inner_ipv4;
    }
    state parse_inner_ipv4 {
        packet.extract(hdr.inner_ipv4);
        transition select(hdr.ipv4.proto) {
            IpProtocol.UDP: parse_inner_udp;
            IpProtocol.TCP: parse_inner_tcp;
            IpProtocol.ICMP: parse_inner_icmp;
            default: accept;
        }
    }
    state parse_inner_udp {
        packet.extract(hdr.inner_udp);
        local_meta.l4_sport = hdr.inner_udp.sport;
        local_meta.l4_dport = hdr.inner_udp.dport;
        transition accept;
    }
    state parse_inner_tcp {
        packet.extract(hdr.inner_tcp);
        local_meta.l4_sport = hdr.inner_tcp.sport;
        local_meta.l4_dport = hdr.inner_tcp.dport;
        transition accept;
    }
    state parse_inner_icmp {
        packet.extract(hdr.inner_icmp);
        transition accept;
    }
}

control DeparserImpl(packet_out packet, in parsed_headers_t hdr) {
    apply {
        packet.emit(hdr.packet_in);
        packet.emit(hdr.ethernet);
        packet.emit(hdr.outer_ipv4);
        packet.emit(hdr.outer_udp);
        packet.emit(hdr.gtpu);
        packet.emit(hdr.ipv4);
        packet.emit(hdr.udp);
        packet.emit(hdr.tcp);
        packet.emit(hdr.icmp);
    }
}

control VerifyChecksumImpl(inout parsed_headers_t hdr, inout local_metadata_t meta) {
    apply {
        verify_checksum(hdr.ipv4.isValid(), { hdr.ipv4.version, hdr.ipv4.ihl, hdr.ipv4.dscp, hdr.ipv4.ecn, hdr.ipv4.total_len, hdr.ipv4.identification, hdr.ipv4.flags, hdr.ipv4.frag_offset, hdr.ipv4.ttl, hdr.ipv4.proto, hdr.ipv4.src_addr, hdr.ipv4.dst_addr }, hdr.ipv4.checksum, HashAlgorithm.csum16);
        verify_checksum(hdr.outer_ipv4.isValid(), { hdr.outer_ipv4.version, hdr.outer_ipv4.ihl, hdr.outer_ipv4.dscp, hdr.outer_ipv4.ecn, hdr.outer_ipv4.total_len, hdr.outer_ipv4.identification, hdr.outer_ipv4.flags, hdr.outer_ipv4.frag_offset, hdr.outer_ipv4.ttl, hdr.outer_ipv4.proto, hdr.outer_ipv4.src_addr, hdr.outer_ipv4.dst_addr }, hdr.outer_ipv4.checksum, HashAlgorithm.csum16);
    }
}

control ComputeChecksumImpl(inout parsed_headers_t hdr, inout local_metadata_t local_meta) {
    apply {
        update_checksum(hdr.outer_ipv4.isValid(), { hdr.outer_ipv4.version, hdr.outer_ipv4.ihl, hdr.outer_ipv4.dscp, hdr.outer_ipv4.ecn, hdr.outer_ipv4.total_len, hdr.outer_ipv4.identification, hdr.outer_ipv4.flags, hdr.outer_ipv4.frag_offset, hdr.outer_ipv4.ttl, hdr.outer_ipv4.proto, hdr.outer_ipv4.src_addr, hdr.outer_ipv4.dst_addr }, hdr.outer_ipv4.checksum, HashAlgorithm.csum16);
        update_checksum(hdr.ipv4.isValid(), { hdr.ipv4.version, hdr.ipv4.ihl, hdr.ipv4.dscp, hdr.ipv4.ecn, hdr.ipv4.total_len, hdr.ipv4.identification, hdr.ipv4.flags, hdr.ipv4.frag_offset, hdr.ipv4.ttl, hdr.ipv4.proto, hdr.ipv4.src_addr, hdr.ipv4.dst_addr }, hdr.ipv4.checksum, HashAlgorithm.csum16);
    }
}

control Acl(inout parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    action set_port(port_num_t port) {
        std_meta.egress_spec = port;
    }
    action punt() {
        set_port(255);
    }
    action clone_to_cpu() {
        clone3(CloneType.I2E, 99, { std_meta.ingress_port });
    }
    action drop() {
        mark_to_drop(std_meta);
        exit;
    }
    table acls {
        key = {
            std_meta.ingress_port  : ternary @name("inport") ;
            local_meta.src_iface   : ternary @name("src_iface") ;
            hdr.ethernet.src_addr  : ternary @name("eth_src") ;
            hdr.ethernet.dst_addr  : ternary @name("eth_dst") ;
            hdr.ethernet.ether_type: ternary @name("eth_type") ;
            hdr.ipv4.src_addr      : ternary @name("ipv4_src") ;
            hdr.ipv4.dst_addr      : ternary @name("ipv4_dst") ;
            hdr.ipv4.proto         : ternary @name("ipv4_proto") ;
            local_meta.l4_sport    : ternary @name("l4_sport") ;
            local_meta.l4_dport    : ternary @name("l4_dport") ;
        }
        actions = {
            set_port;
            punt;
            clone_to_cpu;
            drop;
            NoAction;
        }
        const default_action = NoAction;
        @name("acls") counters = direct_counter(CounterType.packets_and_bytes);
    }
    apply {
        acls.apply();
    }
}

control Routing(inout parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    action drop() {
        mark_to_drop(std_meta);
    }
    action route(mac_addr_t dst_mac, port_num_t egress_port) {
        std_meta.egress_spec = egress_port;
        hdr.ethernet.src_addr = hdr.ethernet.dst_addr;
        hdr.ethernet.dst_addr = dst_mac;
    }
    table routes_v4 {
        key = {
            local_meta.next_hop_ip: lpm @name("dst_prefix") ;
            hdr.ipv4.src_addr     : selector;
            hdr.ipv4.proto        : selector;
            local_meta.l4_sport   : selector;
            local_meta.l4_dport   : selector;
        }
        actions = {
            route;
        }
        @name("hashed_selector") implementation = action_selector(HashAlgorithm.crc16, 32w1024, 32w16);
        size = 1024;
    }
    apply {
        if (hdr.outer_ipv4.isValid()) {
            local_meta.next_hop_ip = hdr.outer_ipv4.dst_addr;
            hdr.outer_ipv4.ttl = hdr.outer_ipv4.ttl - 1;
        } else if (hdr.ipv4.isValid()) {
            local_meta.next_hop_ip = hdr.ipv4.dst_addr;
            hdr.ipv4.ttl = hdr.ipv4.ttl - 1;
        }
        if (hdr.ipv4.ttl == 0) {
            drop();
        } else {
            routes_v4.apply();
        }
    }
}

control ExecuteFar(inout parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    @hidden action _udp_encap(ipv4_addr_t src_addr, ipv4_addr_t dst_addr, L4Port udp_sport, L4Port udp_dport, bit<16> ipv4_total_len) {
        hdr.outer_ipv4.setValid();
        hdr.outer_ipv4.version = IP_VERSION_4;
        hdr.outer_ipv4.ihl = 5;
        hdr.outer_ipv4.dscp = 0;
        hdr.outer_ipv4.ecn = 0;
        hdr.outer_ipv4.total_len = ipv4_total_len;
        hdr.outer_ipv4.identification = 0x1513;
        hdr.outer_ipv4.flags = 0;
        hdr.outer_ipv4.frag_offset = 0;
        hdr.outer_ipv4.ttl = DEFAULT_IPV4_TTL;
        hdr.outer_ipv4.proto = IpProtocol.UDP;
        hdr.outer_ipv4.src_addr = src_addr;
        hdr.outer_ipv4.dst_addr = dst_addr;
        hdr.outer_ipv4.checksum = 0;
        hdr.outer_udp.setValid();
        hdr.outer_udp.sport = udp_sport;
        hdr.outer_udp.dport = udp_dport;
        hdr.outer_udp.len = hdr.ipv4.total_len + (8 + 8);
        hdr.outer_udp.checksum = 0;
    }
    @hidden action gtpu_encap(ipv4_addr_t src_addr, ipv4_addr_t dst_addr, L4Port udp_sport, teid_t teid) {
        _udp_encap(src_addr, dst_addr, udp_sport, L4Port.GTP_GPDU, hdr.ipv4.total_len + 20 + 8 + 8);
        hdr.gtpu.setValid();
        hdr.gtpu.version = GTPU_VERSION;
        hdr.gtpu.pt = GTP_PROTOCOL_TYPE_GTP;
        hdr.gtpu.spare = 0;
        hdr.gtpu.ex_flag = 0;
        hdr.gtpu.seq_flag = 0;
        hdr.gtpu.npdu_flag = 0;
        hdr.gtpu.msgtype = GTPUMessageType.GPDU;
        hdr.gtpu.msglen = hdr.ipv4.total_len;
        hdr.gtpu.teid = teid;
    }
    action do_gtpu_tunnel() {
        gtpu_encap(local_meta.far.tunnel_out_src_ipv4_addr, local_meta.far.tunnel_out_dst_ipv4_addr, local_meta.far.tunnel_out_udp_dport, local_meta.far.tunnel_out_teid);
    }
    action do_forward() {
    }
    action do_drop() {
        mark_to_drop(std_meta);
        exit;
    }
    action do_notify_cp() {
        clone3(CloneType.I2E, 99, { std_meta.ingress_port });
    }
    apply {
        if (local_meta.far.notify_cp) {
            do_notify_cp();
        }
        if (local_meta.far.needs_tunneling) {
            if (local_meta.far.tunnel_out_type == TunnelType.GTPU) {
                do_gtpu_tunnel();
            }
        }
        if (local_meta.far.needs_dropping) {
            do_drop();
        } else {
            do_forward();
        }
    }
}

control PreQosPipe(inout parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    counter(1024, CounterType.packets_and_bytes) pre_qos_pdr_counter;
    table my_station {
        key = {
            hdr.ethernet.dst_addr: exact @name("dst_mac") ;
        }
        actions = {
            NoAction;
        }
    }
    action set_source_iface(InterfaceType src_iface, Direction direction) {
        local_meta.src_iface = src_iface;
        local_meta.direction = direction;
    }
    table source_iface_lookup {
        key = {
            hdr.ipv4.dst_addr: lpm @name("ipv4_dst_prefix") ;
        }
        actions = {
            set_source_iface;
        }
        const default_action = set_source_iface(InterfaceType.UNKNOWN, Direction.UNKNOWN);
    }
    @hidden action gtpu_decap() {
        hdr.gtpu.setInvalid();
        hdr.outer_ipv4.setInvalid();
        hdr.outer_udp.setInvalid();
    }
    action set_pdr_attributes(pdr_id_t id, fseid_t fseid, counter_index_t ctr_id, far_id_t far_id, bit<1> needs_gtpu_decap) {
        local_meta.pdr.id = id;
        local_meta.fseid = fseid;
        local_meta.pdr.ctr_idx = ctr_id;
        local_meta.far.id = far_id;
        local_meta.needs_gtpu_decap = (bool)needs_gtpu_decap;
    }
    table pdrs {
        key = {
            local_meta.src_iface   : exact @name("src_iface") ;
            hdr.outer_ipv4.dst_addr: ternary @name("tunnel_ipv4_dst") ;
            local_meta.teid        : ternary @name("teid") ;
            local_meta.ue_addr     : ternary @name("ue_addr") ;
            local_meta.inet_addr   : ternary @name("inet_addr") ;
            local_meta.ue_l4_port  : range @name("ue_l4_port") ;
            local_meta.inet_l4_port: range @name("inet_l4_port") ;
            hdr.ipv4.proto         : ternary @name("ip_proto") ;
        }
        actions = {
            set_pdr_attributes;
        }
    }
    action load_normal_far_attributes(bit<1> needs_dropping, bit<1> notify_cp) {
        local_meta.far.needs_tunneling = false;
        local_meta.far.needs_dropping = (bool)needs_dropping;
        local_meta.far.notify_cp = (bool)notify_cp;
    }
    action load_tunnel_far_attributes(bit<1> needs_dropping, bit<1> notify_cp, TunnelType tunnel_type, ipv4_addr_t src_addr, ipv4_addr_t dst_addr, teid_t teid, L4Port dport) {
        local_meta.far.needs_tunneling = true;
        local_meta.far.needs_dropping = (bool)needs_dropping;
        local_meta.far.notify_cp = (bool)notify_cp;
        local_meta.far.tunnel_out_type = tunnel_type;
        local_meta.far.tunnel_out_src_ipv4_addr = src_addr;
        local_meta.far.tunnel_out_dst_ipv4_addr = dst_addr;
        local_meta.far.tunnel_out_teid = teid;
        local_meta.far.tunnel_out_udp_dport = dport;
    }
    table load_far_attributes {
        key = {
            local_meta.far.id: exact @name("far_id") ;
            local_meta.fseid : exact @name("session_id") ;
        }
        actions = {
            load_normal_far_attributes;
            load_tunnel_far_attributes;
        }
    }
    apply {
        if (!my_station.apply().hit) {
            return;
        }
        source_iface_lookup.apply();
        if (hdr.inner_ipv4.isValid()) {
            hdr.outer_ipv4 = hdr.ipv4;
            hdr.ipv4 = hdr.inner_ipv4;
            hdr.inner_ipv4.setInvalid();
            hdr.outer_udp = hdr.udp;
            if (hdr.inner_udp.isValid()) {
                hdr.udp = hdr.inner_udp;
                hdr.inner_udp.setInvalid();
            } else {
                hdr.udp.setInvalid();
                if (hdr.inner_tcp.isValid()) {
                    hdr.tcp = hdr.inner_tcp;
                    hdr.inner_tcp.setInvalid();
                } else if (hdr.inner_icmp.isValid()) {
                    hdr.icmp = hdr.inner_icmp;
                    hdr.inner_icmp.setInvalid();
                }
            }
        }
        if (local_meta.direction == Direction.UPLINK) {
            local_meta.ue_addr = hdr.ipv4.src_addr;
            local_meta.inet_addr = hdr.ipv4.dst_addr;
            local_meta.ue_l4_port = local_meta.l4_sport;
            local_meta.inet_l4_port = local_meta.l4_dport;
        } else if (local_meta.direction == Direction.DOWNLINK) {
            local_meta.ue_addr = hdr.ipv4.dst_addr;
            local_meta.inet_addr = hdr.ipv4.src_addr;
            local_meta.ue_l4_port = local_meta.l4_dport;
            local_meta.inet_l4_port = local_meta.l4_sport;
        }
        pdrs.apply();
        pre_qos_pdr_counter.count(local_meta.pdr.ctr_idx);
        if (local_meta.needs_gtpu_decap) {
            gtpu_decap();
        }
        load_far_attributes.apply();
        ExecuteFar.apply(hdr, local_meta, std_meta);
        Routing.apply(hdr, local_meta, std_meta);
        Acl.apply(hdr, local_meta, std_meta);
    }
}

control PostQosPipe(inout parsed_headers_t hdr, inout local_metadata_t local_meta, inout standard_metadata_t std_meta) {
    counter(1024, CounterType.packets_and_bytes) post_qos_pdr_counter;
    apply {
        post_qos_pdr_counter.count(local_meta.pdr.ctr_idx);
        if (std_meta.egress_port == 255) {
            hdr.packet_in.setValid();
            hdr.packet_in.ingress_port = std_meta.ingress_port;
            exit;
        }
    }
}

V1Switch(ParserImpl(), VerifyChecksumImpl(), PreQosPipe(), PostQosPipe(), ComputeChecksumImpl(), DeparserImpl()) main;

