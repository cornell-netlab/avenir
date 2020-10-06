package hybrid.p4pipelines.sourcerouting;

import com.google.common.collect.BiMap;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import org.onlab.packet.DeserializationException;
import org.onlab.packet.Ethernet;
import org.onlab.util.ImmutableByteSequence;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.DeviceId;
import org.onosproject.net.Port;
import org.onosproject.net.PortNumber;
import org.onosproject.net.device.DeviceService;
import org.onosproject.net.driver.AbstractHandlerBehaviour;
import org.onosproject.net.flow.TrafficTreatment;
import org.onosproject.net.flow.criteria.Criterion;
import org.onosproject.net.flow.instructions.Instruction;
import org.onosproject.net.flow.instructions.Instructions;
import org.onosproject.net.packet.DefaultInboundPacket;
import org.onosproject.net.packet.InboundPacket;
import org.onosproject.net.packet.OutboundPacket;
import org.onosproject.net.pi.model.*;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;
import org.onosproject.net.pi.runtime.PiPacketMetadata;
import org.onosproject.net.pi.runtime.PiPacketOperation;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static java.lang.String.format;
import static org.onlab.util.ImmutableByteSequence.copyFrom;
import static org.onosproject.net.PortNumber.CONTROLLER;
import static org.onosproject.net.PortNumber.FLOOD;
import static org.onosproject.net.flow.instructions.Instruction.Type.OUTPUT;
import static org.onosproject.net.pi.model.PiPacketOperationType.PACKET_OUT;

public class SourceRoutingPipelineInterpreterEdge extends AbstractHandlerBehaviour
        implements PiPipelineInterpreter {

    private static final String DOT = ".";
    private static final String HDR = "hdr";
    private static final String INGRESS = "MyIngress";
    private static final String ENCAPSULATE = "encapsulate";
    private static final String EGRESS_PORT = "egress_port";
    private static final String INGRESS_PORT = "ingress_port";
    private static final String ETHERNET = "ethernet";
    private static final String STANDARD_METADATA = "standard_metadata";
    private static final int PORT_FIELD_BITWIDTH = 9;

    private static final PiMatchFieldId INGRESS_PORT_ID =
            PiMatchFieldId.of(STANDARD_METADATA + DOT + "ingress_port");
    private static final PiMatchFieldId ETH_DST_ID =
            PiMatchFieldId.of(HDR + DOT + ETHERNET + DOT + "dstAddr");
    private static final PiMatchFieldId ETH_SRC_ID =
            PiMatchFieldId.of(HDR + DOT + ETHERNET + DOT + "srcAddr");
    private static final PiMatchFieldId ETH_TYPE_ID =
            PiMatchFieldId.of(HDR + DOT + ETHERNET + DOT + "etherType");

    private static final PiTableId TABLE_ENCAPSULATE =
            PiTableId.of(INGRESS + DOT + ENCAPSULATE);

    private static final PiActionId ACT_ID_NOP =
            PiActionId.of("NoAction");
    private static final PiActionId ACT_ID_SEND_TO_CPU =
            PiActionId.of(INGRESS + DOT + "send_to_cpu");
    private static final PiActionId ACT_ID_SET_EGRESS_PORT =
            PiActionId.of(INGRESS + DOT + "set_out_port");

    private static final PiActionParamId ACT_PARAM_ID_PORT =
            PiActionParamId.of("port");

    private static final BiMap<Integer, PiTableId> TABLE_MAP =
            new ImmutableBiMap.Builder<Integer, PiTableId>()
//                    .put(0, TABLE_ENCAPSULATE)
                    .build();

    private static final BiMap<Criterion.Type, PiMatchFieldId> CRITERION_MAP =
            new ImmutableBiMap.Builder<Criterion.Type, PiMatchFieldId>()
                    .put(Criterion.Type.IN_PORT, INGRESS_PORT_ID)
//                    .put(Criterion.Type.ETH_DST, ETH_DST_ID)
//                    .put(Criterion.Type.ETH_SRC, ETH_SRC_ID)
//                    .put(Criterion.Type.ETH_TYPE, ETH_TYPE_ID)
                    .build();

    @Override
    public Optional<PiMatchFieldId> mapCriterionType(Criterion.Type type) {
        return Optional.ofNullable(CRITERION_MAP.get(type));
    }

    @Override
    public Optional<Criterion.Type> mapPiMatchFieldId(PiMatchFieldId headerFieldId) {
        return Optional.ofNullable(CRITERION_MAP.inverse().get(headerFieldId));
    }

    @Override
    public Optional<PiTableId> mapFlowRuleTableId(int flowRuleTableId) {
        return Optional.ofNullable(TABLE_MAP.get(flowRuleTableId));
    }

    @Override
    public Optional<Integer> mapPiTableId(PiTableId piTableId) {
        return Optional.ofNullable(TABLE_MAP.inverse().get(piTableId));
    }


    @Override
    public PiAction mapTreatment(TrafficTreatment treatment, PiTableId piTableId)
            throws PiPipelineInterpreter.PiInterpreterException {
        throw new PiPipelineInterpreter.PiInterpreterException("no treatment is mapped");
    }


    @Override
    public Collection<PiPacketOperation> mapOutboundPacket(OutboundPacket packet)
            throws PiPipelineInterpreter.PiInterpreterException {
        throw new PiPipelineInterpreter.PiInterpreterException("no mapping for OutboundPacket");

    }

    @Override
    public InboundPacket mapInboundPacket(PiPacketOperation packetIn, DeviceId deviceId)
            throws PiPipelineInterpreter.PiInterpreterException {
        throw new PiPipelineInterpreter.PiInterpreterException("no mapping for InBoundPacket");
    }
}
