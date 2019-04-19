
package hybrid.controller.edge;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.onosproject.core.ApplicationId;
import org.onosproject.core.CoreService;
import org.onosproject.net.Device;
import org.onosproject.net.DeviceId;
import org.onosproject.net.Host;
import org.onosproject.net.PortNumber;
import org.onosproject.net.device.DeviceEvent;
import org.onosproject.net.device.DeviceListener;
import org.onosproject.net.device.DeviceService;
import org.onosproject.net.flow.*;
import org.onosproject.net.flow.criteria.PiCriterion;
import org.onosproject.net.host.HostEvent;
import org.onosproject.net.host.HostListener;
import org.onosproject.net.host.HostService;
import org.onosproject.net.pi.model.PiActionId;
import org.onosproject.net.pi.model.PiActionParamId;
import org.onosproject.net.pi.model.PiMatchFieldId;
import org.onosproject.net.pi.model.PiTableId;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;
import org.onosproject.net.topology.Topology;
import org.onosproject.net.topology.TopologyService;
import org.osgi.service.component.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Skeletal ONOS application component.
 */
@Component(immediate = true)
public class EdgeController {

    private static final String APP_NAME = "hybrid.controller.edge";


    private static final int FLOW_RULE_PRIORITY = 200;

    private final Logger log = LoggerFactory.getLogger(getClass());

    private ApplicationId appId;
    private final DeviceListener deviceListener = new InternalDeviceListener();

    //--------------------------------------------------------------------------
    // ONOS core services needed by this application.
    //--------------------------------------------------------------------------

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private FlowRuleService flowRuleService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private CoreService coreService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private DeviceService deviceService;


    @Activate
    protected void activate() {
        log.info("Starting...");
        appId = coreService.registerApplication(APP_NAME);
        deviceService.addListener(deviceListener);
        log.info("STARTED", appId.id());
    }

    @Deactivate
    protected void deactivate() {
        log.info("Stopping...");
        deviceService.removeListener(deviceListener);
        flowRuleService.removeFlowRulesById(appId);
        log.info("STOPPED");
    }


    private class InternalDeviceListener implements DeviceListener {

        @Override
        public void event(DeviceEvent event) {
            Device device = event.subject();
            log.info("Device: "+device.toString() +" DeviceEvent "+event.toString());
            synchronized (this) {
                if (event.type() == DeviceEvent.Type.DEVICE_ADDED) {
                    log.info(device.id().toString());
                    if (device.id().toString() == "device:bmv2:e1") {
                        log.info("inserting flow on e1");
                        insert_entry_edge_1(device.id());
                    }
                    if (device.id().toString() == "device:bmv2:e2") {
                        log.info("inserting flow on e2");
                        insert_entry_edge_2(device.id());
                    }
                }
                if (event.type() == DeviceEvent.Type.DEVICE_ADDED) {
                    log.info(device.id().toString());
                    if (device.id().toString().equals("device:bmv2:e1")) {
                        log.info("inserting flow on e1");
                        insert_entry_edge_1(device.id());
                    }
                    if (device.id().toString().equals("device:bmv2:e2")) {
                        log.info("inserting flow on e2");
                        insert_entry_edge_2(device.id());
                    }
                }

            }

        }
    }

    private void insert_entry_edge_1(DeviceId deviceId){
        Collection<PiActionParam> actionParams = new HashSet<>();
        byte[] ethSrcAddr =  {0,0,0,0,0,1};
        addModEthSrcActionParam(actionParams, ethSrcAddr);
        byte[] ethDstAddr = {0,0,0,0,0,2};
        addModEthDstActionParam(actionParams, ethDstAddr);
        short[] ports = {3,2,1};
        addValidSrcRouteParams(actionParams, ports);
        addEgressSpecActionParam(actionParams, (short)3);
        addRemainingSrcRouteParams(actionParams, ports.length);

        PiAction action = createAction(actionParams);
        PiCriterion.Builder piCriterionBuilder = PiCriterion.builder();
        addEthDstMatchField(piCriterionBuilder, ethDstAddr);
        addEthSrcMatchField(piCriterionBuilder, ethSrcAddr);
        PiCriterion piCriterion = piCriterionBuilder.build();

        log.info(deviceId.toString()+" ----------------> " + action.toString());
        insertPiFlowRule(deviceId, SourceRoutingPipelineConstants.encapsulateTableId, piCriterion, action);
    }


    private void insert_entry_edge_2(DeviceId deviceId){
        Collection<PiActionParam> actionParams = new HashSet<>();
        byte[] ethSrcAddr =  {0,0,0,0,0,2};
        addModEthSrcActionParam(actionParams, ethSrcAddr);
        byte[] ethDstAddr = {0,0,0,0,0,1};
        addModEthDstActionParam(actionParams, ethDstAddr);
        short[] ports = {1, 1};
        addValidSrcRouteParams(actionParams, ports);
        addEgressSpecActionParam(actionParams, (short)3);
        addRemainingSrcRouteParams(actionParams, ports.length);

        PiAction action = createAction(actionParams);


        PiCriterion.Builder piCriterionBuilder = PiCriterion.builder();

        addEthDstMatchField(piCriterionBuilder, ethDstAddr);
        addEthSrcMatchField(piCriterionBuilder, ethSrcAddr);

        PiCriterion piCriterion = piCriterionBuilder.build();
        log.info(deviceId.toString()+" ----------------> " +action.toString());

        insertPiFlowRule(deviceId, SourceRoutingPipelineConstants.encapsulateTableId, piCriterion, action);
    }


    private void insertPiFlowRule(DeviceId switchId, PiTableId tableId, PiCriterion piCriterion, PiAction piAction) {
        FlowRule rule = DefaultFlowRule.builder()
                .forDevice(switchId)
                .forTable(tableId)
                .fromApp(appId)
                .withPriority(FLOW_RULE_PRIORITY)
                .makePermanent()
                .withSelector(DefaultTrafficSelector.builder()
                        .matchPi(piCriterion).build())
                .withTreatment(DefaultTrafficTreatment.builder()
                        .piTableAction(piAction).build())
                .build();
        flowRuleService.applyFlowRules(rule);
    }

    private byte[] getMask(Boolean valid, Integer size) {
        byte[] mask = new byte[size];
        if (valid)
            Arrays.fill(mask, (byte)0xff);
        else
            Arrays.fill(mask, (byte)0x00);
        return mask;
    }

    private PiAction createAction(Collection<PiActionParam> params)  {
        return PiAction.builder()
                .withId(SourceRoutingPipelineConstants.getHdrDataActionId)
                .withParameters(params)
                .build();
    }

    private Collection<PiActionParam> addModEthSrcActionParam(Collection<PiActionParam> actionParams, byte[] ethSrcAddr) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modSrcMacParamId, 1);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.srcEthAddrParamId, ethSrcAddr);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    private Collection<PiActionParam> addModEthDstActionParam(Collection<PiActionParam> actionParams, byte[] ethDstAddr) {
        PiActionParam modParam = new PiActionParam(SourceRoutingPipelineConstants.modDstMacParamId, 1);
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.dstEthAddrParamId, ethDstAddr);
        actionParams.add(modParam);
        actionParams.add(valueParam);
        return actionParams;
    }

    private Collection<PiActionParam> addEgressSpecActionParam(Collection<PiActionParam> actionParams, short egressSpec) {
        PiActionParam valueParam = new PiActionParam(SourceRoutingPipelineConstants.egressSpecParamId , egressSpec);
        actionParams.add(valueParam);
        return actionParams;
    }

    private Collection<PiActionParam> addValidSrcRouteParams(Collection<PiActionParam> actionParams, short[] ports) {
        byte bos = 1;
        for (Integer i = ports.length-1; i>=0; i--){
            PiActionParamId validParamId = PiActionParamId.of("valid"+i.toString());
            PiActionParamId bosParamId = PiActionParamId.of("bos"+i.toString());
            PiActionParamId portParamId = PiActionParamId.of("port"+i.toString());
            PiActionParam validParam = new PiActionParam(validParamId, 1);
            PiActionParam bosParam = new PiActionParam(bosParamId, bos);
            PiActionParam portParam = new PiActionParam(portParamId, ports[i]);
            actionParams.add(validParam);
            actionParams.add(bosParam);
            actionParams.add(portParam);
            bos = 0;
        }
        return actionParams;
    }

    private Collection<PiActionParam> addRemainingSrcRouteParams(Collection<PiActionParam> actionParams, Integer from) {
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

    private PiCriterion.Builder addEthDstMatchField(PiCriterion.Builder matchBuilder, byte[] ethDstAddr) {
        byte[] mask = getMask(true, ethDstAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethDstAddrMatchFieldId, ethDstAddr, mask);
    }
    private PiCriterion.Builder addEthDstMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 6);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethDstAddrMatchFieldId, mask, mask);
    }

    private PiCriterion.Builder addEthSrcMatchField(PiCriterion.Builder matchBuilder, byte[] ethSrcAddr) {
        byte[] mask = getMask(true, ethSrcAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethSrcAddrMatchFieldId, ethSrcAddr, mask);
    }
    private PiCriterion.Builder addEthSrcMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 6);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ethSrcAddrMatchFieldId, mask, mask);
    }

    private PiCriterion.Builder addEtherTypeMatchField(PiCriterion.Builder matchBuilder, byte[] etherType) {
        byte[] mask = getMask(true, etherType.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.etherTypeMatchFieldId, etherType, mask);
    }

    private PiCriterion.Builder addEtherTypeMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 2);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.etherTypeMatchFieldId, mask, mask);
    }

    private PiCriterion.Builder addIpProtocolMatchField(PiCriterion.Builder matchBuilder, byte[] ipProto) {
        byte[] mask = getMask(true, ipProto.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipProtocolMatchFieldId, ipProto, mask);
    }

    private PiCriterion.Builder addIpProtocolMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 1);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipProtocolMatchFieldId, mask, mask);
    }

    private PiCriterion.Builder addIpSrcAddrMatchField(PiCriterion.Builder matchBuilder, byte[] ipSrcAddr) {
        byte[] mask = getMask(true, ipSrcAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipSrcAddrMatchFieldId, ipSrcAddr, mask);
    }

    private PiCriterion.Builder addIpSrcAddrMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 4);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipSrcAddrMatchFieldId, mask, mask);
    }

    private PiCriterion.Builder addIpDstAddrMatchField(PiCriterion.Builder matchBuilder, byte[] ipDstAddr) {
        byte[] mask = getMask(true, ipDstAddr.length);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipDstAddrMatchFieldId, ipDstAddr, mask);
    }

    private PiCriterion.Builder addIpDstAddrMatchField(PiCriterion.Builder matchBuilder) {
        byte[] mask = getMask(false, 4);
        return matchBuilder.matchTernary(SourceRoutingPipelineConstants.ipDstAddrMatchFieldId, mask, mask);
    }
}
