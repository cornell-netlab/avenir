/*
 * Copyright 2018-present Open Networking Foundation
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

package org.onosproject.segmentrouting.cli;

import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.cluster.NodeId;
import org.onosproject.net.DeviceId;
import org.onosproject.segmentrouting.SegmentRoutingService;

import java.util.Map;
import java.util.Set;

/**
 * Display current shouldProgram map.
 */
@Service
@Command(scope = "onos", name = "sr-should-program",
        description = "Display current shouldProgram map")
public class ShouldProgramCommand extends AbstractShellCommand {
    @Override
    protected void doExecute() {
        SegmentRoutingService srService = AbstractShellCommand.get(SegmentRoutingService.class);
        Map<Set<DeviceId>, NodeId> shouldProgram = srService.getShouldProgram();
        Map<DeviceId, Boolean> shouldProgramCache = srService.getShouldProgramCache();

        print("shouldProgram");
        shouldProgram.forEach((k, v) -> print("%s -> %s", k, v));

        print("shouldProgramCache");
        shouldProgramCache.forEach((k, v) -> print("%s -> %s", k, v));
    }
}