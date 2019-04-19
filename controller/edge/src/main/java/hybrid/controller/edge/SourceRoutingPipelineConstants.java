package hybrid.controller.edge;

import org.onosproject.net.pi.model.PiActionId;
import org.onosproject.net.pi.model.PiActionParamId;
import org.onosproject.net.pi.model.PiMatchFieldId;
import org.onosproject.net.pi.model.PiTableId;

public class SourceRoutingPipelineConstants {
    public static PiTableId encapsulateTableId = PiTableId.of("MyIngress.encapsulate");

    public static PiMatchFieldId ingressPortMatchFieldId = PiMatchFieldId.of("standard_metadata.ingress_port");

    public static PiMatchFieldId ethDstAddrMatchFieldId = PiMatchFieldId.of("hdr.ethernet.dstAddr");
    public static PiMatchFieldId ethSrcAddrMatchFieldId = PiMatchFieldId.of("hdr.ethernet.srcAddr");
    public static PiMatchFieldId etherTypeMatchFieldId = PiMatchFieldId.of("hdr.ethernet.etherType");

    public static PiMatchFieldId ipSrcAddrMatchFieldId = PiMatchFieldId.of("hdr.ipv4.srcAddr");
    public static PiMatchFieldId ipDstAddrMatchFieldId = PiMatchFieldId.of("hdr.ipv4.dstAddr");
    public static PiMatchFieldId ipProtocolMatchFieldId = PiMatchFieldId.of("hdr.ipv4.protocol");


    public static PiActionParamId modSrcMacParamId = PiActionParamId.of("modSrcMac");
    public static PiActionParamId modDstMacParamId = PiActionParamId.of("modDstMac");
    public static PiActionParamId modEthTypeParamId = PiActionParamId.of("modEthType");

    public static PiActionParamId srcEthAddrParamId = PiActionParamId.of("srcEthAddr");
    public static PiActionParamId dstEthAddrParamId = PiActionParamId.of("dstEthAddr");
    public static PiActionParamId etherTypeParamId = PiActionParamId.of("etherType");

    public static PiActionParamId egressSpecParamId = PiActionParamId.of("egress_spec");

    public static PiActionId getHdrDataActionId = PiActionId.of("MyIngress.get_hdr_data");

}
