package hybrid.controller.edge;

import org.onlab.util.ImmutableByteSequence;
import org.onosproject.net.flow.criteria.PiCriterion;
import org.onosproject.net.pi.model.PiActionParamId;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;

public class EdgeDevicePI {

    private static byte[] getMask(Boolean valid, Integer size) {
        byte[] mask = new byte[size];
        if (valid)
            Arrays.fill(mask, (byte)0xff);
        else
            Arrays.fill(mask, (byte)0x00);
        return mask;
    }

    public static PiAction createAction(Collection<PiActionParam> params)  {
        return PiAction.builder()
                .withId(SourceRoutingPipelineConstants.getHdrDataActionId)
                .withParameters(params)
                .build();
    }

    public static Collection<PiActionParam> addModEthSrcActionParam(Collection<PiActionParam> actionParams, byte[] ethSrcAddr) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modSrcMacParamId, 1);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.srcEthAddrParamId, ethSrcAddr);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    public static Collection<PiActionParam> addNoModEthSrcAction(Collection<PiActionParam> actionParams) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modSrcMacParamId, 0);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.srcEthAddrParamId, 0);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    public static Collection<PiActionParam> addModEthDstActionParam(Collection<PiActionParam> actionParams, byte[] ethDstAddr) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modDstMacParamId, 1);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.dstEthAddrParamId, ethDstAddr);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    public static Collection<PiActionParam> addNoModEthDstAction(Collection<PiActionParam> actionParams) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modDstMacParamId, 0);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.dstEthAddrParamId, 0);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    public static Collection<PiActionParam> addEgressSpecActionParam(Collection<PiActionParam> actionParams, short egressSpec) {
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.egressSpecParamId , egressSpec);
        actionParams.add(valueParam);
        return actionParams;
    }

    public static Collection<PiActionParam> addValidSrcRouteParams(Collection<PiActionParam> actionParams, ArrayList<Short> ports) {
        byte bos = 1;
        for (Integer i = ports.size()-1; i>=0; i--){
            PiActionParamId validParamId = PiActionParamId.of("valid"+i.toString());
            PiActionParamId bosParamId = PiActionParamId.of("bos"+i.toString());
            PiActionParamId portParamId = PiActionParamId.of("port"+i.toString());
            PiActionParam validParam = new PiActionParam(validParamId, 1);
            PiActionParam bosParam = new PiActionParam(bosParamId, bos);
            PiActionParam portParam = new PiActionParam(portParamId, ports.get(i));
            actionParams.add(validParam);
            actionParams.add(bosParam);
            actionParams.add(portParam);
            bos = 0;
        }
        return actionParams;
    }

    public static Collection<PiActionParam> addRemainingSrcRouteParams(Collection<PiActionParam> actionParams, Integer from) {
        for (Integer i = from; i<8; i++){
            PiActionParamId validParamId = PiActionParamId.of("valid"+i.toString());
            PiActionParamId bosParamId = PiActionParamId.of("bos"+i.toString());
            PiActionParamId portParamId = PiActionParamId.of("port"+i.toString());
            PiActionParam validParam = new PiActionParam(validParamId, 0);
            PiActionParam bosParam = new PiActionParam(bosParamId, 0);
            PiActionParam portParam = new PiActionParam(portParamId, 0);
            actionParams.add(validParam);
            actionParams.add(bosParam);
            actionParams.add(portParam);
        }
        return actionParams;
    }

    public static PiCriterion.Builder addIngressPortMatchField(PiCriterion.Builder matchBuilder, short port) {
        byte[] mask = getMask(true, 2);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ingressPortMatchFieldId,
                ImmutableByteSequence.copyFrom(port).asArray(), mask);
    }

    public static PiCriterion.Builder addEthDstMatchField(PiCriterion.Builder matchBuilder, byte[] ethDstAddr) {
        byte[] mask = getMask(true, ethDstAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethDstAddrMatchFieldId, ethDstAddr, mask);
    }
    public static PiCriterion.Builder addEthDstMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 6);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethDstAddrMatchFieldId, mask, mask);
    }

    public static PiCriterion.Builder addEthSrcMatchField(PiCriterion.Builder matchBuilder, byte[] ethSrcAddr) {
        byte[] mask = getMask(true, ethSrcAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethSrcAddrMatchFieldId, ethSrcAddr, mask);
    }
    public static PiCriterion.Builder addEthSrcMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 6);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethSrcAddrMatchFieldId, mask, mask);
    }

    public static PiCriterion.Builder addEtherTypeMatchField(PiCriterion.Builder matchBuilder, byte[] etherType) {
        byte[] mask = getMask(true, etherType.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.etherTypeMatchFieldId, etherType, mask);
    }

    public static PiCriterion.Builder addEtherTypeMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 2);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.etherTypeMatchFieldId, mask, mask);
    }

    public static PiCriterion.Builder addIpProtocolMatchField(PiCriterion.Builder matchBuilder, byte[] ipProto) {
        byte[] mask = getMask(true, ipProto.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipProtocolMatchFieldId, ipProto, mask);
    }

    public static PiCriterion.Builder addIpProtocolMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 1);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipProtocolMatchFieldId, mask, mask);
    }

    public static PiCriterion.Builder addIpSrcAddrMatchField(PiCriterion.Builder matchBuilder, byte[] ipSrcAddr) {
        byte[] mask = getMask(true, ipSrcAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipSrcAddrMatchFieldId, ipSrcAddr, mask);
    }

    public static PiCriterion.Builder addIpSrcAddrMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 4);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipSrcAddrMatchFieldId, mask, mask);
    }

    private static PiCriterion.Builder addIpDstAddrMatchField(PiCriterion.Builder matchBuilder, byte[] ipDstAddr) {
        byte[] mask = getMask(true, ipDstAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipDstAddrMatchFieldId, ipDstAddr, mask);
    }

    public static PiCriterion.Builder addIpDstAddrMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 4);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipDstAddrMatchFieldId, mask, mask);
    }
}
