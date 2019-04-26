package hybrid.controller.edge;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ArrayNode;
import org.onlab.packet.MacAddress;
import org.onosproject.net.DeviceId;
import org.onosproject.net.Port;
import org.onosproject.net.PortNumber;
import org.onosproject.net.flow.DefaultTrafficSelector;
import org.onosproject.net.flow.TrafficSelector;
import org.onosproject.net.flow.criteria.PiCriterion;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;

public class EdgeRule {


    private static String SW = "switch";

    private static String PRECONDITION = "precondition";
    private static String PORT = "port";
    private static String PORTS = "ports";
    private static String FINAL_PACKET = "final_packet";
    private static String ETH_DST = "ethdst";




    public DeviceId deviceId;

    public TrafficSelector trafficSelector;
    public PiAction piAction;

    private EdgeRule() {

    }

    public static EdgeRule getEdgeRule(JsonNode jsonNode) {
        EdgeRule edgeRule = new EdgeRule();
        String did = jsonNode.get(SW).asText();
        edgeRule.deviceId = DeviceId.deviceId(did);

        JsonNode preCondJson = jsonNode.get(PRECONDITION);
        edgeRule.trafficSelector = getPiCriterion(preCondJson);


        JsonNode portsJson = jsonNode.get(PORTS);

        ArrayList<Short> ports = new ArrayList<>();
        portsJson.forEach(portNode -> {
            ports.add(0, (short)portNode.asInt());
        });

        JsonNode finalPktJson = jsonNode.get(FINAL_PACKET);
        edgeRule.piAction = getPiAction(finalPktJson, ports);


        return edgeRule;
    }

    private static TrafficSelector getPiCriterion(JsonNode preCondJson) {
        TrafficSelector.Builder selectorBuilder = DefaultTrafficSelector.builder();

        PiCriterion.Builder piCriterionBuilder = PiCriterion.builder();

        JsonNode ethDstAddrJson = preCondJson.get(ETH_DST);
        JsonNode ingressPortJson = preCondJson.get(PORT);
        if (ingressPortJson != null) {
//            EdgeDevicePI.addIngressPortMatchField(piCriterionBuilder, (short)ingressPortJson.asInt());
            selectorBuilder.matchInPort(PortNumber.portNumber(ingressPortJson.asLong()));
        }
        if (ethDstAddrJson != null) {
            MacAddress mac = MacAddress.valueOf(ethDstAddrJson.asLong());
            EdgeDevicePI.addEthDstMatchField(piCriterionBuilder, mac.toBytes());
        }

        PiCriterion piCriterion = null;
        TrafficSelector trafficSelector = null;
        try {
            piCriterion = piCriterionBuilder.build();
            trafficSelector = selectorBuilder.matchPi(piCriterion).build();
        }catch (Exception e) {

        }
        return  trafficSelector;
    }

    private static PiAction getPiAction(JsonNode finalPacketJson, ArrayList<Short> ports) {
        Collection<PiActionParam> actionParams = new HashSet<>();

        JsonNode ethDstAddrJson = finalPacketJson.get(ETH_DST);
        if (ethDstAddrJson != null) {
            MacAddress mac = MacAddress.valueOf(ethDstAddrJson.asLong());
            EdgeDevicePI.addModEthDstActionParam(actionParams, mac.toBytes());
            System.out.println(ethDstAddrJson.asText());
        } else {
            EdgeDevicePI.addNoModEthDstAction(actionParams);
        }

        EdgeDevicePI.addNoModEthSrcAction(actionParams);

        EdgeDevicePI.addEgressSpecActionParam(actionParams, ports.get(0));
        ports.remove(0);
        EdgeDevicePI.addValidSrcRouteParams(actionParams, ports);

        EdgeDevicePI.addRemainingSrcRouteParams(actionParams, ports.size());

        return EdgeDevicePI.createAction(actionParams);
    }


}
