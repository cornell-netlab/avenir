
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

package hybrid.p4pipelines.srcmac;

import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
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
import org.onosproject.net.packet.DefaultInboundPacket;
import org.onosproject.net.packet.InboundPacket;
import org.onosproject.net.packet.OutboundPacket;
import org.onosproject.net.pi.model.PiActionId;
import org.onosproject.net.pi.model.PiMatchFieldId;
import org.onosproject.net.pi.model.PiPipelineInterpreter;
import org.onosproject.net.pi.model.PiTableId;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;
import org.onosproject.net.pi.runtime.PiPacketMetadata;
import org.onosproject.net.pi.runtime.PiPacketOperation;

import java.nio.ByteBuffer;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import static java.lang.String.format;
import static java.util.stream.Collectors.toList;
import static org.onlab.util.ImmutableByteSequence.copyFrom;
import static org.onosproject.net.PortNumber.CONTROLLER;
import static org.onosproject.net.PortNumber.FLOOD;
import static org.onosproject.net.flow.instructions.Instruction.Type.OUTPUT;
import static org.onosproject.net.flow.instructions.Instructions.OutputInstruction;
import static org.onosproject.net.pi.model.PiPacketOperationType.PACKET_OUT;


/**
 * Interpreter implementation for sr_mac_p4_16.p4.
 */
public class SrcMacPipelineInterpreter extends AbstractHandlerBehaviour
        implements PiPipelineInterpreter {

    private static final int PORT_BITWIDTH = 9;

    private static final ImmutableBiMap<Integer, PiTableId> TABLE_MAP =
            new ImmutableBiMap.Builder<Integer, PiTableId>()
                    .put(0, PipelineConstants.INGRESS_TABLE0_CONTROL_TABLE0)
                    .build();
    private static final ImmutableBiMap<Criterion.Type, PiMatchFieldId> CRITERION_MAP =
            new ImmutableBiMap.Builder<Criterion.Type, PiMatchFieldId>()
                    .put(Criterion.Type.ETH_DST, PipelineConstants.HDR_HDR_ETHERNET_DST_ADDR)
                    .build();


    @Override
    public PiAction mapTreatment(TrafficTreatment treatment, PiTableId piTableId)
            throws PiInterpreterException {
        throw new PiInterpreterException("Treatment has multiple instructions");
//        if (treatment.allInstructions().isEmpty()) {
//            // No actions means drop.
//            return PiAction.builder().withId(DROP).build();
//        } else if (treatment.allInstructions().size() > 1) {
//            // We understand treatments with only 1 instruction.
//            throw new PiInterpreterException("Treatment has multiple instructions");
//        }

//        Instruction instruction = treatment.allInstructions().get(0);
//        switch (instruction.type()) {
//            case OUTPUT:
//                if (piTableId.equals(INGRESS_TABLE0_CONTROL_TABLE0)) {
//                    return outputPiAction((OutputInstruction) instruction, INGRESS_TABLE0_CONTROL_SET_EGRESS_PORT);
//                } else if (piTableId.equals(INGRESS_WCMP_CONTROL_WCMP_TABLE)) {
//                    return outputPiAction((OutputInstruction) instruction, INGRESS_WCMP_CONTROL_SET_EGRESS_PORT);
//                } else {
//                    throw new PiInterpreterException(
//                            "Output instruction not supported in table " + piTableId);
//                }
//            case NOACTION:
//                return PiAction.builder().withId(NO_ACTION).build();
//            default:
//                throw new PiInterpreterException(format(
//                        "Instruction type '%s' not supported", instruction.type()));
//        }
    }

    private PiAction outputPiAction(OutputInstruction outInstruction, PiActionId piActionId)
            throws PiInterpreterException {
        PortNumber port = outInstruction.port();
//        if (!port.isLogical()) {
//            return PiAction.builder()
//                    .withId(piActionId)
//                    .withParameter(new PiActionParam(PORT, port.toLong()))
//                    .build();
//        } else if (port.equals(CONTROLLER)) {
//            return PiAction.builder().withId(INGRESS_TABLE0_CONTROL_SEND_TO_CPU).build();
//        } else {
//            throw new PiInterpreterException(format(
//                    "Egress on logical port '%s' not supported", port));
//        }
//
        throw new PiInterpreterException(format(
                "Egress on logical port '%s' not supported", port));
    }

    @Override
    public Collection<PiPacketOperation> mapOutboundPacket(OutboundPacket packet)
            throws PiInterpreterException {
        TrafficTreatment treatment = packet.treatment();

        throw new PiInterpreterException("Treatment not supported: " + treatment);

//        // basic.p4 supports only OUTPUT instructions.
//        List<OutputInstruction> outInstructions = treatment
//                .allInstructions()
//                .stream()
//                .filter(i -> i.type().equals(OUTPUT))
//                .map(i -> (OutputInstruction) i)
//                .collect(toList());
//
//        if (treatment.allInstructions().size() != outInstructions.size()) {
//            // There are other instructions that are not of type OUTPUT.
//            throw new PiInterpreterException("Treatment not supported: " + treatment);
//        }
//
//        ImmutableList.Builder<PiPacketOperation> builder = ImmutableList.builder();
//        for (OutputInstruction outInst : outInstructions) {
//            if (outInst.port().isLogical() && !outInst.port().equals(FLOOD)) {
//                throw new PiInterpreterException(format(
//                        "Output on logical port '%s' not supported", outInst.port()));
//            } else if (outInst.port().equals(FLOOD)) {
//                // Since basic.p4 does not support flooding, we create a packet
//                // operation for each switch port.
//                final DeviceService deviceService = handler().get(DeviceService.class);
//                for (Port port : deviceService.getPorts(packet.sendThrough())) {
//                    builder.add(createPiPacketOperation(packet.data(), port.number().toLong()));
//                }
//            } else {
//                builder.add(createPiPacketOperation(packet.data(), outInst.port().toLong()));
//            }
//        }
//        return builder.build();
    }

    @Override
    public InboundPacket mapInboundPacket(PiPacketOperation packetIn, DeviceId deviceId)
            throws PiInterpreterException {
        // Assuming that the packet is ethernet, which is fine since basic.p4
        // can deparse only ethernet packets.

        throw new PiInterpreterException("");
//        Ethernet ethPkt;
//        try {
//            ethPkt = Ethernet.deserializer().deserialize(packetIn.data().asArray(), 0,
//                                                         packetIn.data().size());
//        } catch (DeserializationException dex) {
//            throw new PiInterpreterException(dex.getMessage());
//        }
//
//        // Returns the ingress port packet metadata.
//        Optional<PiPacketMetadata> packetMetadata = packetIn.metadatas()
//                .stream().filter(m -> m.id().equals(INGRESS_PORT))
//                .findFirst();
//
//        if (packetMetadata.isPresent()) {
//            ImmutableByteSequence portByteSequence = packetMetadata.get().value();
//            short s = portByteSequence.asReadOnlyBuffer().getShort();
//            ConnectPoint receivedFrom = new ConnectPoint(deviceId, PortNumber.portNumber(s));
//            ByteBuffer rawData = ByteBuffer.wrap(packetIn.data().asArray());
//            return new DefaultInboundPacket(receivedFrom, ethPkt, rawData);
//        } else {
//            throw new PiInterpreterException(format(
//                    "Missing metadata '%s' in packet-in received from '%s': %s",
//                    INGRESS_PORT, deviceId, packetIn));
//        }
    }




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
}
