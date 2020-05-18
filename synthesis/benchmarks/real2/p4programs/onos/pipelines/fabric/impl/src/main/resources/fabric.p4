/*
 * Copyright 2017-present Open Networking Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// ADDED

#ifndef WITH_IPV6
#define WITH_IPV6
#endif

#ifndef WITH_SIMPLE_NEXT
#define WITH_SIMPLE_NEXT
#endif

// END ADDED

#include <core.p4>
#include <v1model.p4>

#include "include/size.p4"
#include "include/control/filtering.p4"
#include "include/control/forwarding.p4"
#include "include/control/acl.p4"
#include "include/control/next.p4"
#include "include/control/packetio.p4"
#include "include/header.p4"
#include "include/checksum.p4"
#include "include/parser.p4"

#ifdef WITH_PORT_COUNTER
#include "include/control/port_counter.p4"
#endif // WITH_PORT_COUNTER

#ifdef WITH_SPGW
#include "include/spgw.p4"
#endif // WITH_SPGW

#ifdef WITH_BNG
#include "include/bng.p4"
#endif // WITH_BNG

#ifdef WITH_INT
#include "include/int/int_main.p4"
#endif // WITH_INT

// ADDED

const bit<8> CPU_PORT = 64;

// END ADDED

control MyIngress (inout parsed_headers_t hdr,
                       inout fabric_metadata_t fabric_metadata,
                       inout standard_metadata_t standard_metadata) {

    PacketIoIngress() pkt_io_ingress;
    Filtering() filtering;
    Forwarding() forwarding;
    Acl() acl;
    Next() next;
#ifdef WITH_PORT_COUNTER
    PortCountersControl() port_counters_control;
#endif // WITH_PORT_COUNTER

    apply {
        _PRE_INGRESS
        forwarding.apply(hdr, fabric_metadata, standard_metadata);
        next.apply(hdr, fabric_metadata, standard_metadata);

    }
}

control FabricEgress (inout parsed_headers_t hdr,
                      inout fabric_metadata_t fabric_metadata,
                      inout standard_metadata_t standard_metadata) {

    PacketIoEgress() pkt_io_egress;
    EgressNextControl() egress_next;

    apply {
        _PRE_EGRESS
        pkt_io_egress.apply(hdr, fabric_metadata, standard_metadata);
        egress_next.apply(hdr, fabric_metadata, standard_metadata);
#ifdef WITH_SPGW
        spgw_egress.apply(hdr.ipv4, hdr.gtpu_ipv4, hdr.gtpu_udp, hdr.gtpu,
                          fabric_metadata, standard_metadata);
#endif // WITH_SPGW
#ifdef WITH_BNG
        bng_egress.apply(hdr, fabric_metadata, standard_metadata);
#endif // WITH_BNG
#ifdef WITH_INT
        process_int_main.apply(hdr, fabric_metadata, standard_metadata);
#endif
    }
}

V1Switch(
    FabricParser(),
    FabricVerifyChecksum(),
    FabricIngress(),
    FabricEgress(),
    FabricComputeChecksum(),
    FabricDeparser()
) main;
