
package hybrid.p4pipelines.sourcerouting;

import org.onosproject.driver.pipeline.DefaultSingleTablePipeline;
import org.onosproject.net.behaviour.Pipeliner;
import org.onosproject.net.device.PortStatisticsDiscovery;
import org.onosproject.net.pi.model.DefaultPiPipeconf;
import org.onosproject.net.pi.model.PiPipeconf;
import org.onosproject.net.pi.model.PiPipeconfId;
import org.onosproject.net.pi.model.PiPipelineInterpreter;
import org.onosproject.net.pi.model.PiPipelineModel;
import org.onosproject.net.pi.service.PiPipeconfService;
import org.onosproject.p4runtime.model.P4InfoParser;
import org.onosproject.p4runtime.model.P4InfoParserException;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;

import static org.onosproject.net.pi.model.PiPipeconf.ExtensionType.BMV2_JSON;
import static org.onosproject.net.pi.model.PiPipeconf.ExtensionType.P4_INFO_TEXT;

/**
 * Skeletal ONOS application component.
 */
@Component(immediate = true)
public final class SourceRoutingPipeconfEdge {
    public static final PiPipeconfId PIPECONF_ID = new PiPipeconfId("sourcerouting-pipeconf-edge");
    private static final URL P4INFO_URL = SourceRoutingPipeconfEdge.class.getResource("/source_routing_edge_p4info.txt");
    private static final URL BMV2_JSON_URL = SourceRoutingPipeconfEdge.class.getResource("/source_routing_edge.json");
    private final Logger log = LoggerFactory.getLogger(getClass());

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private PiPipeconfService piPipeconfService;

    @Activate
    protected void activate() {
        log.info("Started");
        try {
            piPipeconfService.register(buildPipeconf());
        } catch (P4InfoParserException e) {
            log.error("Fail to register {} - Exception: {} - Cause: {}",
                    PIPECONF_ID, e.getMessage(), e.getCause().getMessage());
        }
    }

    @Deactivate
    protected void deactivate() {
        log.info("Stopped");
        try {
            piPipeconfService.remove(PIPECONF_ID);
        } catch (IllegalStateException e) {
            log.warn("{} haven't been registered", PIPECONF_ID);
        }
    }

    private PiPipeconf buildPipeconf() throws P4InfoParserException {

        final PiPipelineModel pipelineModel = P4InfoParser.parse(P4INFO_URL);

        return DefaultPiPipeconf.builder()
                .withId(PIPECONF_ID)
                .withPipelineModel(pipelineModel)
                .addBehaviour(PiPipelineInterpreter.class, SourceRoutingPipelineInterpreterEdge.class)
                .addBehaviour(Pipeliner.class, DefaultSingleTablePipeline.class)
                .addExtension(P4_INFO_TEXT, P4INFO_URL)
                .addExtension(BMV2_JSON, BMV2_JSON_URL)
                .build();
    }

}
