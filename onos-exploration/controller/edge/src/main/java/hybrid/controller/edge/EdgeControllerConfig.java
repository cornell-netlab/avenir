package hybrid.controller.edge;

import com.fasterxml.jackson.databind.JsonNode;
import org.onosproject.core.ApplicationId;
import org.onosproject.net.config.Config;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;

public class EdgeControllerConfig extends Config<ApplicationId> {

    private static final String RULES = "rules";
    private final Logger log = LoggerFactory.getLogger(getClass());

    public EdgeControllerConfig(){

    }

    public EdgeRules getEdgeRules() {
        JsonNode rulesNode = object.get(RULES);
        log.info(" parsed "+ rulesNode.toString());
        ArrayList<EdgeRule> edgeRules = new ArrayList<>();
        if (rulesNode == null) {
            return new EdgeRules(new ArrayList<>());
        }
        rulesNode.forEach(jsonNode -> {
            EdgeRule edgeRule = EdgeRule.getEdgeRule(jsonNode);
            log.info(" parsed "+edgeRule.deviceId);
            edgeRules.add(edgeRule);
        });
        return new EdgeRules(edgeRules);
    }

}
