(declare-const ethernet_action_run$1 (_ BitVec 2))
(declare-const ethernet_action_run$2 (_ BitVec 2))
(declare-const exit$1 (_ BitVec 1))
(declare-const hdr.ethernet.dstAddr$0 (_ BitVec 48))
(declare-const hdr.ethernet.dstAddr$1 (_ BitVec 48))
(declare-const hdr.ethernet.dstAddr$2 (_ BitVec 48))
(declare-const hdr.ethernet.etherType$0 (_ BitVec 16))
(declare-const hdr.ethernet.etherType$1 (_ BitVec 16))
(declare-const hdr.ethernet.srcAddr$0 (_ BitVec 48))
(declare-const hdr.ethernet.srcAddr$1 (_ BitVec 48))
(declare-const hdr.ethernet.srcAddr$2 (_ BitVec 48))
(declare-const hdr.ipv4.diffserv$0 (_ BitVec 8))
(declare-const hdr.ipv4.diffserv$1 (_ BitVec 8))
(declare-const hdr.ipv4.dstAddr$0 (_ BitVec 32))
(declare-const hdr.ipv4.dstAddr$1 (_ BitVec 32))
(declare-const hdr.ipv4.flags$0 (_ BitVec 3))
(declare-const hdr.ipv4.flags$1 (_ BitVec 3))
(declare-const hdr.ipv4.fragOffset$0 (_ BitVec 13))
(declare-const hdr.ipv4.fragOffset$1 (_ BitVec 13))
(declare-const hdr.ipv4.hdrChecksum$0 (_ BitVec 16))
(declare-const hdr.ipv4.hdrChecksum$1 (_ BitVec 16))
(declare-const hdr.ipv4.identification$0 (_ BitVec 16))
(declare-const hdr.ipv4.identification$1 (_ BitVec 16))
(declare-const hdr.ipv4.ihl$0 (_ BitVec 4))
(declare-const hdr.ipv4.ihl$1 (_ BitVec 4))
(declare-const hdr.ipv4.isValid$0 (_ BitVec 1))
(declare-const hdr.ipv4.isValid$1 (_ BitVec 1))
(declare-const hdr.ipv4.protocol$0 (_ BitVec 8))
(declare-const hdr.ipv4.protocol$1 (_ BitVec 8))
(declare-const hdr.ipv4.srcAddr$0 (_ BitVec 32))
(declare-const hdr.ipv4.srcAddr$1 (_ BitVec 32))
(declare-const hdr.ipv4.totalLen$0 (_ BitVec 16))
(declare-const hdr.ipv4.totalLen$1 (_ BitVec 16))
(declare-const hdr.ipv4.ttl$0 (_ BitVec 8))
(declare-const hdr.ipv4.ttl$1 (_ BitVec 8))
(declare-const hdr.ipv4.ttl$2 (_ BitVec 8))
(declare-const hdr.ipv4.version$0 (_ BitVec 4))
(declare-const hdr.ipv4.version$1 (_ BitVec 4))
(declare-const ipv4_action_run$1 (_ BitVec 2))
(declare-const ipv4_action_run$2 (_ BitVec 2))
(declare-const ipv4_action_run$3 (_ BitVec 2))
(declare-const j (_ BitVec 32))
(declare-const k (_ BitVec 9))
(declare-const l (_ BitVec 48))
(declare-const m (_ BitVec 32))
(declare-const phys_ethernet_action_run$1 (_ BitVec 2))
(declare-const phys_ethernet_action_run$2 (_ BitVec 2))
(declare-const phys_exit$1 (_ BitVec 1))
(declare-const phys_hdr.ethernet.dstAddr$0 (_ BitVec 48))
(declare-const phys_hdr.ethernet.dstAddr$1 (_ BitVec 48))
(declare-const phys_hdr.ethernet.dstAddr$2 (_ BitVec 48))
(declare-const phys_hdr.ethernet.etherType$0 (_ BitVec 16))
(declare-const phys_hdr.ethernet.etherType$1 (_ BitVec 16))
(declare-const phys_hdr.ethernet.srcAddr$0 (_ BitVec 48))
(declare-const phys_hdr.ethernet.srcAddr$1 (_ BitVec 48))
(declare-const phys_hdr.ethernet.srcAddr$2 (_ BitVec 48))
(declare-const phys_hdr.ipv4.diffserv$0 (_ BitVec 8))
(declare-const phys_hdr.ipv4.diffserv$1 (_ BitVec 8))
(declare-const phys_hdr.ipv4.dstAddr$0 (_ BitVec 32))
(declare-const phys_hdr.ipv4.dstAddr$1 (_ BitVec 32))
(declare-const phys_hdr.ipv4.flags$0 (_ BitVec 3))
(declare-const phys_hdr.ipv4.flags$1 (_ BitVec 3))
(declare-const phys_hdr.ipv4.fragOffset$0 (_ BitVec 13))
(declare-const phys_hdr.ipv4.fragOffset$1 (_ BitVec 13))
(declare-const phys_hdr.ipv4.hdrChecksum$0 (_ BitVec 16))
(declare-const phys_hdr.ipv4.hdrChecksum$1 (_ BitVec 16))
(declare-const phys_hdr.ipv4.identification$0 (_ BitVec 16))
(declare-const phys_hdr.ipv4.identification$1 (_ BitVec 16))
(declare-const phys_hdr.ipv4.ihl$0 (_ BitVec 4))
(declare-const phys_hdr.ipv4.ihl$1 (_ BitVec 4))
(declare-const phys_hdr.ipv4.isValid$0 (_ BitVec 1))
(declare-const phys_hdr.ipv4.isValid$1 (_ BitVec 1))
(declare-const phys_hdr.ipv4.protocol$0 (_ BitVec 8))
(declare-const phys_hdr.ipv4.protocol$1 (_ BitVec 8))
(declare-const phys_hdr.ipv4.srcAddr$0 (_ BitVec 32))
(declare-const phys_hdr.ipv4.srcAddr$1 (_ BitVec 32))
(declare-const phys_hdr.ipv4.totalLen$0 (_ BitVec 16))
(declare-const phys_hdr.ipv4.totalLen$1 (_ BitVec 16))
(declare-const phys_hdr.ipv4.ttl$0 (_ BitVec 8))
(declare-const phys_hdr.ipv4.ttl$1 (_ BitVec 8))
(declare-const phys_hdr.ipv4.ttl$2 (_ BitVec 8))
(declare-const phys_hdr.ipv4.version$0 (_ BitVec 4))
(declare-const phys_hdr.ipv4.version$1 (_ BitVec 4))
(declare-const phys_ipv4_action_run$1 (_ BitVec 2))
(declare-const phys_ipv4_action_run$2 (_ BitVec 2))
(declare-const phys_ipv4_action_run$3 (_ BitVec 2))
(declare-const phys_meta.nexthop$1 (_ BitVec 32))
(declare-const phys_meta.nexthop$2 (_ BitVec 32))
(declare-const phys_nexthop_action_run$1 (_ BitVec 2))
(declare-const phys_nexthop_action_run$2 (_ BitVec 2))
(declare-const phys_nexthop_action_run$3 (_ BitVec 2))
(declare-const phys_punt_action_run$1 (_ BitVec 1))
(declare-const phys_punt_action_run$2 (_ BitVec 1))
(declare-const phys_return3$1 (_ BitVec 1))
(declare-const phys_return3$2 (_ BitVec 1))
(declare-const phys_return4$1 (_ BitVec 1))
(declare-const phys_return4$2 (_ BitVec 1))
(declare-const phys_standard_metadata.egress_port$0 (_ BitVec 9))
(declare-const phys_standard_metadata.egress_port$1 (_ BitVec 9))
(declare-const phys_standard_metadata.egress_port$2 (_ BitVec 9))
(declare-const phys_standard_metadata.egress_spec$0 (_ BitVec 9))
(declare-const phys_standard_metadata.egress_spec$1 (_ BitVec 9))
(declare-const phys_standard_metadata.egress_spec$2 (_ BitVec 9))
(declare-const punt_action_run$1 (_ BitVec 1))
(declare-const punt_action_run$2 (_ BitVec 1))
(declare-const return1$1 (_ BitVec 1))
(declare-const return1$2 (_ BitVec 1))
(declare-const return2$1 (_ BitVec 1))
(declare-const return2$2 (_ BitVec 1))
(declare-const standard_metadata.egress_port$0 (_ BitVec 9))
(declare-const standard_metadata.egress_port$1 (_ BitVec 9))
(declare-const standard_metadata.egress_port$2 (_ BitVec 9))
(declare-const standard_metadata.egress_spec$0 (_ BitVec 9))
(declare-const standard_metadata.egress_spec$1 (_ BitVec 9))
(declare-const standard_metadata.egress_spec$2 (_ BitVec 9))

(assert
 (and (not (= j (_ bv0 32))) (not (= k (_ bv0 9))) (not (= l (_ bv0 48))) (not (= m (_ bv0 32)))
 (not (=> (and (= ethernet_action_run$1 (_ bv0 2))
		      (= exit$1 (_ bv0 1))
		      (= ipv4_action_run$1 (_ bv0 2))
		      (= punt_action_run$1 (_ bv0 1))
		      (= return1$1 (_ bv0 1))
		      (= return2$1 (_ bv0 1))
		      (= standard_metadata.egress_spec$0 (_ bv0 9))
		      (= return2$2 (_ bv0 1))
		      (= ethernet_action_run$2 (_ bv0 2))
		      (or (and (= hdr.ipv4.isValid$0 (_ bv1 1)) (= ipv4_action_run$2 (_ bv0 2)) (= hdr.ipv4.dstAddr$0 j) (= ipv4_action_run$3 (_ bv1 2)) (= standard_metadata.egress_spec$1 k) (= hdr.ethernet.dstAddr$0 hdr.ethernet.srcAddr$1) (= hdr.ethernet.dstAddr$1 l) (or (and (= return2$2 (_ bv0 1)) (= hdr.ipv4.ttl$1 (bvsub hdr.ipv4.ttl$0 (_ bv1 8)))) (and (not (= return2$2 (_ bv0 1))) (= hdr.ipv4.ttl$0 hdr.ipv4.ttl$1)))) (and (not (= hdr.ipv4.isValid$0 (_ bv1 1))) (= standard_metadata.egress_spec$0 standard_metadata.egress_spec$1) (= ipv4_action_run$1 ipv4_action_run$3) (= hdr.ipv4.ttl$0 hdr.ipv4.ttl$1) (= hdr.ethernet.srcAddr$0 hdr.ethernet.srcAddr$1) (= hdr.ethernet.dstAddr$0 hdr.ethernet.dstAddr$1)))
		      (= punt_action_run$2 (_ bv0 1))
		      (= standard_metadata.egress_port$1 standard_metadata.egress_spec$1)
		      (= return1$2 (_ bv0 1))
		      (or (and (= standard_metadata.egress_spec$1 (_ bv0 9)) (= hdr.ethernet.dstAddr$2 (_ bv0 48)) (= hdr.ethernet.srcAddr$2 (_ bv0 48)) (= hdr.ethernet.etherType$1 (_ bv0 16)) (= hdr.ipv4.isValid$1 (_ bv0 1)) (= hdr.ipv4.version$1 (_ bv0 4)) (= hdr.ipv4.ihl$1 (_ bv0 4)) (= hdr.ipv4.diffserv$1 (_ bv0 8)) (= hdr.ipv4.totalLen$1 (_ bv0 16)) (= hdr.ipv4.identification$1 (_ bv0 16)) (= hdr.ipv4.flags$1 (_ bv0 3)) (= hdr.ipv4.fragOffset$1 (_ bv0 13)) (= hdr.ipv4.ttl$2 (_ bv0 8)) (= hdr.ipv4.protocol$1 (_ bv0 8)) (= hdr.ipv4.hdrChecksum$1 (_ bv0 16)) (= hdr.ipv4.srcAddr$1 (_ bv0 32)) (= hdr.ipv4.dstAddr$1 (_ bv0 32)) (= standard_metadata.egress_spec$2 (_ bv0 9)) (= standard_metadata.egress_port$2 (_ bv0 9))) (and (not (= standard_metadata.egress_spec$1 (_ bv0 9))) (= standard_metadata.egress_spec$1 standard_metadata.egress_spec$2) (= standard_metadata.egress_port$1 standard_metadata.egress_port$2) (= hdr.ipv4.version$0 hdr.ipv4.version$1) (= hdr.ipv4.ttl$1 hdr.ipv4.ttl$2) (= hdr.ipv4.totalLen$0 hdr.ipv4.totalLen$1) (= hdr.ipv4.srcAddr$0 hdr.ipv4.srcAddr$1) (= hdr.ipv4.protocol$0 hdr.ipv4.protocol$1) (= hdr.ipv4.isValid$0 hdr.ipv4.isValid$1) (= hdr.ipv4.ihl$0 hdr.ipv4.ihl$1) (= hdr.ipv4.identification$0 hdr.ipv4.identification$1) (= hdr.ipv4.hdrChecksum$0 hdr.ipv4.hdrChecksum$1) (= hdr.ipv4.fragOffset$0 hdr.ipv4.fragOffset$1) (= hdr.ipv4.flags$0 hdr.ipv4.flags$1) (= hdr.ipv4.dstAddr$0 hdr.ipv4.dstAddr$1) (= hdr.ipv4.diffserv$0 hdr.ipv4.diffserv$1) (= hdr.ethernet.srcAddr$1 hdr.ethernet.srcAddr$2) (= hdr.ethernet.etherType$0 hdr.ethernet.etherType$1) (= hdr.ethernet.dstAddr$1 hdr.ethernet.dstAddr$2)))
		      (= phys_ethernet_action_run$1 (_ bv0 2))
		      (= phys_exit$1 (_ bv0 1))
		      (= phys_ipv4_action_run$1 (_ bv0 2))
		      (= phys_meta.nexthop$1 (_ bv0 32))
		      (= phys_nexthop_action_run$1 (_ bv0 2))
		      (= phys_punt_action_run$1 (_ bv0 1))
		      (= phys_return3$1 (_ bv0 1))
		      (= phys_return4$1 (_ bv0 1))
		      (= phys_standard_metadata.egress_spec$0 (_ bv0 9))
		      (= phys_return4$2 (_ bv0 1))
		      (= phys_ethernet_action_run$2 (_ bv0 2))
		      (or (and (= phys_hdr.ipv4.isValid$0 (_ bv1 1)) (= phys_ipv4_action_run$2 (_ bv0 2)) (= phys_hdr.ipv4.dstAddr$0 j) (= phys_ipv4_action_run$3 (_ bv1 2)) (= phys_meta.nexthop$2 m) (= phys_hdr.ethernet.dstAddr$0 phys_hdr.ethernet.srcAddr$1) (= phys_hdr.ethernet.dstAddr$1 l) (or (and (= phys_return4$2 (_ bv0 1)) (= phys_hdr.ipv4.ttl$1 (bvsub phys_hdr.ipv4.ttl$0 (_ bv1 8)))) (and (not (= phys_return4$2 (_ bv0 1))) (= phys_hdr.ipv4.ttl$0 phys_hdr.ipv4.ttl$1)))) (and (not (= phys_hdr.ipv4.isValid$0 (_ bv1 1))) (= phys_meta.nexthop$1 phys_meta.nexthop$2) (= phys_ipv4_action_run$1 phys_ipv4_action_run$3) (= phys_hdr.ipv4.ttl$0 phys_hdr.ipv4.ttl$1) (= phys_hdr.ethernet.srcAddr$0 phys_hdr.ethernet.srcAddr$1) (= phys_hdr.ethernet.dstAddr$0 phys_hdr.ethernet.dstAddr$1)))
		      (= phys_nexthop_action_run$2 (_ bv0 2))
		      (= phys_meta.nexthop$2 m)
		      (= phys_nexthop_action_run$3 (_ bv1 2))
		      (= phys_standard_metadata.egress_spec$1 k)
		      (= phys_punt_action_run$2 (_ bv0 1))
		      (= phys_standard_metadata.egress_port$1 phys_standard_metadata.egress_spec$1)
		      (= phys_return3$2 (_ bv0 1))
		      (or (and (= phys_standard_metadata.egress_spec$1 (_ bv0 9)) (= phys_hdr.ethernet.dstAddr$2 (_ bv0 48)) (= phys_hdr.ethernet.srcAddr$2 (_ bv0 48)) (= phys_hdr.ethernet.etherType$1 (_ bv0 16)) (= phys_hdr.ipv4.isValid$1 (_ bv0 1)) (= phys_hdr.ipv4.version$1 (_ bv0 4)) (= phys_hdr.ipv4.ihl$1 (_ bv0 4)) (= phys_hdr.ipv4.diffserv$1 (_ bv0 8)) (= phys_hdr.ipv4.totalLen$1 (_ bv0 16)) (= phys_hdr.ipv4.identification$1 (_ bv0 16)) (= phys_hdr.ipv4.flags$1 (_ bv0 3)) (= phys_hdr.ipv4.fragOffset$1 (_ bv0 13)) (= phys_hdr.ipv4.ttl$2 (_ bv0 8)) (= phys_hdr.ipv4.protocol$1 (_ bv0 8)) (= phys_hdr.ipv4.hdrChecksum$1 (_ bv0 16)) (= phys_hdr.ipv4.srcAddr$1 (_ bv0 32)) (= phys_hdr.ipv4.dstAddr$1 (_ bv0 32)) (= phys_standard_metadata.egress_spec$2 (_ bv0 9)) (= phys_standard_metadata.egress_port$2 (_ bv0 9))) (and (not (= phys_standard_metadata.egress_spec$1 (_ bv0 9))) (= phys_standard_metadata.egress_spec$1 phys_standard_metadata.egress_spec$2) (= phys_standard_metadata.egress_port$1 phys_standard_metadata.egress_port$2) (= phys_hdr.ipv4.version$0 phys_hdr.ipv4.version$1) (= phys_hdr.ipv4.ttl$1 phys_hdr.ipv4.ttl$2) (= phys_hdr.ipv4.totalLen$0 phys_hdr.ipv4.totalLen$1) (= phys_hdr.ipv4.srcAddr$0 phys_hdr.ipv4.srcAddr$1) (= phys_hdr.ipv4.protocol$0 phys_hdr.ipv4.protocol$1) (= phys_hdr.ipv4.isValid$0 phys_hdr.ipv4.isValid$1) (= phys_hdr.ipv4.ihl$0 phys_hdr.ipv4.ihl$1) (= phys_hdr.ipv4.identification$0 phys_hdr.ipv4.identification$1) (= phys_hdr.ipv4.hdrChecksum$0 phys_hdr.ipv4.hdrChecksum$1) (= phys_hdr.ipv4.fragOffset$0 phys_hdr.ipv4.fragOffset$1) (= phys_hdr.ipv4.flags$0 phys_hdr.ipv4.flags$1) (= phys_hdr.ipv4.dstAddr$0 phys_hdr.ipv4.dstAddr$1) (= phys_hdr.ipv4.diffserv$0 phys_hdr.ipv4.diffserv$1) (= phys_hdr.ethernet.srcAddr$1 phys_hdr.ethernet.srcAddr$2) (= phys_hdr.ethernet.etherType$0 phys_hdr.ethernet.etherType$1) (= phys_hdr.ethernet.dstAddr$1 phys_hdr.ethernet.dstAddr$2)))
		      (= hdr.ethernet.dstAddr$0 phys_hdr.ethernet.dstAddr$0)
		      (= hdr.ethernet.etherType$0 phys_hdr.ethernet.etherType$0)
		      (= hdr.ethernet.srcAddr$0 phys_hdr.ethernet.srcAddr$0)
		      (= hdr.ipv4.diffserv$0 phys_hdr.ipv4.diffserv$0)
		      (= hdr.ipv4.dstAddr$0 phys_hdr.ipv4.dstAddr$0)
		      (= hdr.ipv4.flags$0 phys_hdr.ipv4.flags$0)
		      (= hdr.ipv4.fragOffset$0 phys_hdr.ipv4.fragOffset$0)
		      (= hdr.ipv4.hdrChecksum$0 phys_hdr.ipv4.hdrChecksum$0)
		      (= hdr.ipv4.identification$0 phys_hdr.ipv4.identification$0)
		      (= hdr.ipv4.ihl$0 phys_hdr.ipv4.ihl$0)
		      (= hdr.ipv4.isValid$0 phys_hdr.ipv4.isValid$0)
		      (= hdr.ipv4.protocol$0 phys_hdr.ipv4.protocol$0)
		      (= hdr.ipv4.srcAddr$0 phys_hdr.ipv4.srcAddr$0)
		      (= hdr.ipv4.totalLen$0 phys_hdr.ipv4.totalLen$0)
		      (= hdr.ipv4.ttl$0 phys_hdr.ipv4.ttl$0)
		      (= hdr.ipv4.version$0 phys_hdr.ipv4.version$0)
		      (= phys_standard_metadata.egress_port$0 standard_metadata.egress_port$0)
		      (= phys_standard_metadata.egress_spec$0 standard_metadata.egress_spec$0))

		 (and (= hdr.ethernet.dstAddr$2 phys_hdr.ethernet.dstAddr$2)
		      (= hdr.ethernet.etherType$1 phys_hdr.ethernet.etherType$1)
		      (= hdr.ethernet.srcAddr$2 phys_hdr.ethernet.srcAddr$2)
		      (= hdr.ipv4.diffserv$1 phys_hdr.ipv4.diffserv$1)
		      (= hdr.ipv4.dstAddr$1 phys_hdr.ipv4.dstAddr$1)
		      (= hdr.ipv4.flags$1 phys_hdr.ipv4.flags$1)
		      (= hdr.ipv4.fragOffset$1 phys_hdr.ipv4.fragOffset$1)
		      (= hdr.ipv4.hdrChecksum$1 phys_hdr.ipv4.hdrChecksum$1)
		      (= hdr.ipv4.identification$1 phys_hdr.ipv4.identification$1)
		      (= hdr.ipv4.ihl$1 phys_hdr.ipv4.ihl$1)
		      (= hdr.ipv4.isValid$1 phys_hdr.ipv4.isValid$1)
		      (= hdr.ipv4.protocol$1 phys_hdr.ipv4.protocol$1)
		      (= hdr.ipv4.srcAddr$1 phys_hdr.ipv4.srcAddr$1)
		      (= hdr.ipv4.totalLen$1 phys_hdr.ipv4.totalLen$1)
		      (= hdr.ipv4.ttl$2 phys_hdr.ipv4.ttl$2)
		      (= hdr.ipv4.version$1 phys_hdr.ipv4.version$1)
		      (= phys_standard_metadata.egress_port$2 standard_metadata.egress_port$2)
		      (= phys_standard_metadata.egress_spec$2 standard_metadata.egress_spec$2))))))

(check-sat)
(get-model)
