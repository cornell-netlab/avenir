/*
 * Copyright 2019-present Open Networking Foundation
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

package org.stratumproject.fabricdemo.pipeconf;

import org.onosproject.net.DeviceId;
import org.onosproject.net.PortNumber;
import org.onosproject.net.behaviour.NextGroup;
import org.onosproject.net.behaviour.Pipeliner;
import org.onosproject.net.behaviour.PipelinerContext;
import org.onosproject.net.driver.AbstractHandlerBehaviour;
import org.onosproject.net.flow.DefaultFlowRule;
import org.onosproject.net.flow.DefaultTrafficTreatment;
import org.onosproject.net.flow.FlowRule;
import org.onosproject.net.flow.FlowRuleService;
import org.onosproject.net.flow.instructions.Instructions;
import org.onosproject.net.flowobjective.FilteringObjective;
import org.onosproject.net.flowobjective.ForwardingObjective;
import org.onosproject.net.flowobjective.NextObjective;
import org.onosproject.net.flowobjective.ObjectiveError;
import org.onosproject.net.group.GroupDescription;
import org.onosproject.net.group.GroupService;
import org.onosproject.net.pi.model.PiActionId;
import org.onosproject.net.pi.model.PiActionParamId;
import org.onosproject.net.pi.model.PiTableId;
import org.onosproject.net.pi.runtime.PiAction;
import org.onosproject.net.pi.runtime.PiActionParam;
import org.slf4j.Logger;
import org.stratumproject.fabricdemo.common.Utils;

import java.util.Collections;
import java.util.List;

import static org.onosproject.net.flow.instructions.Instruction.Type.OUTPUT;
import static org.slf4j.LoggerFactory.getLogger;
import static org.stratumproject.fabricdemo.AppConstants.CPU_CLONE_SESSION_ID;

/**
 * Pipeliner implementation that maps all forwarding objectives to the ACL
 * table. All other types of objectives are not supported.
 */
public class PipelinerImpl extends AbstractHandlerBehaviour implements Pipeliner {

    // From the P4Info file
    private static final String PUNT_TABLE = "ingress.punt.punt_table";
    private static final String CLONE_TO_CPU = "ingress.punt.set_queue_and_clone_to_cpu";
    private static final String SEND_TO_CPU = "ingress.punt.set_queue_and_send_to_cpu";
    private static final String QUEUE_ID = "queue_id";
    private static final int DEFAULT_QUEUE = 0;

    private final Logger log = getLogger(getClass());

    private FlowRuleService flowRuleService;
    private GroupService groupService;
    private DeviceId deviceId;


    @Override
    public void init(DeviceId deviceId, PipelinerContext context) {
        this.deviceId = deviceId;
        this.flowRuleService = context.directory().get(FlowRuleService.class);
        this.groupService = context.directory().get(GroupService.class);
    }

    @Override
    public void filter(FilteringObjective obj) {
        obj.context().ifPresent(c -> c.onError(obj, ObjectiveError.UNSUPPORTED));
    }

    @Override
    public void forward(ForwardingObjective obj) {
        if (obj.treatment() == null) {
            obj.context().ifPresent(c -> c.onError(obj, ObjectiveError.UNSUPPORTED));
        }

        // Whether this objective specifies an OUTPUT:CONTROLLER instruction.
        final boolean hasCpuAction = obj.treatment()
                .allInstructions().stream()
                .filter(i -> i.type().equals(OUTPUT))
                .map(i -> (Instructions.OutputInstruction) i)
                .anyMatch(i -> i.port().equals(PortNumber.CONTROLLER));

        if (!hasCpuAction) {
            // We support only objectives for clone to CPU behaviours (e.g. for
            // host and link discovery)
            obj.context().ifPresent(c -> c.onError(obj, ObjectiveError.UNSUPPORTED));
        }

        final String actionId = obj.treatment().clearedDeferred()
                ? SEND_TO_CPU : CLONE_TO_CPU;

        // Create an equivalent FlowRule with same selector and clone_to_cpu action.
        final PiAction cpuAction = PiAction.builder()
                .withId(PiActionId.of(actionId))
                .withParameter(new PiActionParam(
                        PiActionParamId.of(QUEUE_ID), DEFAULT_QUEUE))
                .build();

        final FlowRule.Builder ruleBuilder = DefaultFlowRule.builder()
                .forTable(PiTableId.of(PUNT_TABLE))
                .forDevice(deviceId)
                .withSelector(obj.selector())
                .fromApp(obj.appId())
                .withPriority(obj.priority())
                .withTreatment(DefaultTrafficTreatment.builder()
                                       .piTableAction(cpuAction).build());

        if (obj.permanent()) {
            ruleBuilder.makePermanent();
        } else {
            ruleBuilder.makeTemporary(obj.timeout());
        }

        final GroupDescription cloneGroup;
        if (actionId.equals(CLONE_TO_CPU)) {
            cloneGroup = Utils.buildCloneGroup(
                    obj.appId(),
                    deviceId,
                    CPU_CLONE_SESSION_ID,
                    // Ports where to clone the packet.
                    // Just controller in this case.
                    Collections.singleton(PortNumber.CONTROLLER));
        } else {
            cloneGroup = null;
        }

        switch (obj.op()) {
            case ADD:
                flowRuleService.applyFlowRules(ruleBuilder.build());
                if (cloneGroup != null) {
                    groupService.addGroup(cloneGroup);
                }
                break;
            case REMOVE:
                flowRuleService.removeFlowRules(ruleBuilder.build());
                // Do not remove the clone group, it could be used by other flows.
                break;
            default:
                log.warn("Unknown operation {}", obj.op());
        }

        obj.context().ifPresent(c -> c.onSuccess(obj));
    }

    @Override
    public void next(NextObjective obj) {
        obj.context().ifPresent(c -> c.onError(obj, ObjectiveError.UNSUPPORTED));
    }

    @Override
    public List<String> getNextMappings(NextGroup nextGroup) {
        // We do not use nextObjectives or groups.
        return Collections.emptyList();
    }
}
