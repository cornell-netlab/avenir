
package hybrid.controller.edge;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;

import org.onosproject.codec.CodecService;
import org.onosproject.core.ApplicationId;
import org.onosproject.core.CoreService;
import org.onosproject.net.Device;
import org.onosproject.net.DeviceId;

import org.onosproject.net.config.*;
import org.onosproject.net.config.basics.SubjectFactories;
import org.onosproject.net.device.DeviceEvent;
import org.onosproject.net.device.DeviceListener;
import org.onosproject.net.device.DeviceService;
import org.onosproject.net.flow.*;
import org.onosproject.net.flow.criteria.PiCriterion;

import org.onosproject.net.pi.model.PiActionParamId;

import org.onosproject.net.pi.model.PiTableId;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;

import org.osgi.service.component.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Skeletal ONOS application component.
 */
@Component(immediate = true)
public class EdgeController {

    private static final String APP_NAME = "hybrid.controller.edge";

    private static final Class<EdgeControllerConfig> CONFIG_CLASS = EdgeControllerConfig.class;
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
    protected NetworkConfigRegistry registry;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private CoreService coreService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private DeviceService deviceService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private CodecService codecService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private NetworkConfigService networkConfigService;


    private final InternalNetworkConfigListener configListener = new InternalNetworkConfigListener();


    private EdgeRules edgeRules = null;

    private ConfigFactory<ApplicationId, EdgeControllerConfig>
            edgeRoutingConfigFactory =
            new ConfigFactory<ApplicationId, EdgeControllerConfig>(
                    SubjectFactories.APP_SUBJECT_FACTORY,
                    EdgeControllerConfig.class, "edgeController") {
                @Override
                public EdgeControllerConfig createConfig() {
                    return new EdgeControllerConfig();
                }
            };

    @Activate
    protected void activate() {
        log.info("Starting...");
        appId = coreService.registerApplication(APP_NAME);
        deviceService.addListener(deviceListener);
        networkConfigService.addListener(configListener);
        registry.registerConfigFactory(edgeRoutingConfigFactory);
        log.info("STARTED", appId.id());
    }

    @Deactivate
    protected void deactivate() {
        log.info("Stopping...");
        deviceService.removeListener(deviceListener);
        registry.unregisterConfigFactory(edgeRoutingConfigFactory);
        networkConfigService.removeListener(configListener);
        flowRuleService.removeFlowRulesById(appId);
        log.info("STOPPED");
    }


    private void insertEdgeFlowOnDevice(DeviceId deviceId) {
        for (EdgeRule er : edgeRules.edgeRuleArrayList) {
            String erDid = "device:bmv2:"+er.deviceId.toString();
            if (erDid.equals(deviceId.toString())) {
                log.info("------------------------------------------------");
                log.info("inserting table entry  in device "+ deviceId.toString());
                log.info(er.trafficSelector.toString());
                log.info(er.piAction.toString());
                log.info("------------------------------------------------");
                insertPiFlowRule(deviceId, SourceRoutingPipelineConstants.encapsulateTableId, er.trafficSelector, er.piAction);
                log.info("-------------------inserted---------------------");
            }
        }
    }

    private class InternalDeviceListener implements DeviceListener {

        @Override
        public void event(DeviceEvent event) {
            Device device = event.subject();
            log.info("Device: "+device.toString() +" DeviceEvent "+event.toString());
            synchronized (this) {
                if (event.type() == DeviceEvent.Type.DEVICE_ADDED) {
                    log.info(" id  "+device.id().toString());
                    insertEdgeFlowOnDevice(device.id());
                }

            }

        }
    }

    private class InternalNetworkConfigListener implements NetworkConfigListener {
        @Override
        public void event(NetworkConfigEvent event) {
            if (event.configClass() != CONFIG_CLASS) {
                return;
            }
            synchronized (this) {
                switch (event.type()) {
                    case CONFIG_UNREGISTERED:
                        break;
                    case CONFIG_REGISTERED:
                    case CONFIG_ADDED:
                    case CONFIG_UPDATED:
                        EdgeControllerConfig config = networkConfigService.getConfig(
                                coreService.registerApplication(APP_NAME), CONFIG_CLASS);
                        if (config != null) {
                            // log.info("--------------  " + config.toString());
                            edgeRules = config.getEdgeRules();
                            for (EdgeRule e : edgeRules.edgeRuleArrayList) {
                                log.info("Device ID:  " + e.deviceId.toString());
                                log.info("Criterion:  " + e.trafficSelector.toString());
                                log.info("Action:  " + e.piAction.toString());
                            }
                        }
                        break;
                    case CONFIG_REMOVED:
                        if (event.configClass() == CONFIG_CLASS) {
                             log.info(" Not implemented config removed ");
                        }
                        break;
                    default:
                        break;
                }
            }
        }
    }

//    private void insert_entry_edge_1(DeviceId deviceId){
//        Collection<PiActionParam> actionParams = new HashSet<>();
//        byte[] ethSrcAddr =  {0,0,0,0,0,1};
//        EdgeDevicePI.addModEthSrcActionParam(actionParams, ethSrcAddr);
//        byte[] ethDstAddr = {0,0,0,0,0,2};
//        EdgeDevicePI.addModEthDstActionParam(actionParams, ethDstAddr);
//        ArrayList<Short> ports = new ArrayList<>();
//        ports.add((short)3);
//        ports.add((short)2);
//        ports.add((short)1);
//
//        EdgeDevicePI.addValidSrcRouteParams(actionParams, ports);
//        EdgeDevicePI.addEgressSpecActionParam(actionParams, (short)3);
//        EdgeDevicePI.addRemainingSrcRouteParams(actionParams, ports.size());
//
//        PiAction action = EdgeDevicePI.createAction(actionParams);
//
//        PiCriterion.Builder piCriterionBuilder = PiCriterion.builder();
//        EdgeDevicePI.addEthDstMatchField(piCriterionBuilder, ethDstAddr);
//        EdgeDevicePI.addEthSrcMatchField(piCriterionBuilder, ethSrcAddr);
//        PiCriterion piCriterion = piCriterionBuilder.build();
//
//        log.info(deviceId.toString()+" ----------------> " + action.toString());
//        insertPiFlowRule(deviceId, SourceRoutingPipelineConstants.encapsulateTableId, piCriterion, action);
//    }
//
//
//    private void insert_entry_edge_2(DeviceId deviceId){
//        Collection<PiActionParam> actionParams = new HashSet<>();
//        byte[] ethSrcAddr =  {0,0,0,0,0,2};
//        EdgeDevicePI.addModEthSrcActionParam(actionParams, ethSrcAddr);
//        byte[] ethDstAddr = {0,0,0,0,0,1};
//        EdgeDevicePI.addModEthDstActionParam(actionParams, ethDstAddr);
//        ArrayList<Short> ports = new ArrayList<>();
//        ports.add((short)1);
//        ports.add((short)1);
//        EdgeDevicePI.addValidSrcRouteParams(actionParams, ports);
//        EdgeDevicePI.addEgressSpecActionParam(actionParams, (short)3);
//        EdgeDevicePI.addRemainingSrcRouteParams(actionParams, ports.size());
//
//        PiAction action = EdgeDevicePI.createAction(actionParams);
//
//
//        PiCriterion.Builder piCriterionBuilder = PiCriterion.builder();
//
//        EdgeDevicePI.addEthDstMatchField(piCriterionBuilder, ethDstAddr);
//        EdgeDevicePI.addEthSrcMatchField(piCriterionBuilder, ethSrcAddr);
//
//        PiCriterion piCriterion = piCriterionBuilder.build();
//        log.info(deviceId.toString()+" ----------------> " +action.toString());
//
//        insertPiFlowRule(deviceId, SourceRoutingPipelineConstants.encapsulateTableId, piCriterion, action);
//    }


    private void insertPiFlowRule(DeviceId switchId, PiTableId tableId, TrafficSelector trafficSelector, PiAction piAction) {
        FlowRule rule = DefaultFlowRule.builder()
                .forDevice(switchId)
                .forTable(tableId)
                .fromApp(appId)
                .withPriority(FLOW_RULE_PRIORITY)
                .makePermanent()
                .withSelector(trafficSelector)
                .withTreatment(DefaultTrafficTreatment.builder()
                        .piTableAction(piAction).build())
                .build();
        flowRuleService.applyFlowRules(rule);
    }


}
