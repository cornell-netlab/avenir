/*
 * Copyright 2016-present Open Networking Foundation
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
package org.onosproject.newoptical.cli;

import org.apache.karaf.shell.api.action.Argument;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.Completion;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onlab.util.Bandwidth;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.cli.net.ConnectPointCompleter;
import org.onosproject.newoptical.api.OpticalConnectivityId;
import org.onosproject.newoptical.api.OpticalPathService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.DeviceId;
import org.onosproject.net.PortNumber;

@Service
@Command(scope = "onos", name = "add-optical-connectivity",
        description = "Configure optical domain connectivity")
public class AddOpticalConnectivityCommand extends AbstractShellCommand {

    private final Logger log = LoggerFactory.getLogger(getClass());

    // for OSGi import workaround
    ConnectPointCompleter portCompleter;

    @Argument(index = 0, name = "ingress", description = "Ingress connect point",
            required = true, multiValued = false)
    @Completion(ConnectPointCompleter.class)
    String ingressStr = null;

    @Argument(index = 1, name = "egress", description = "Egress connect point",
            required = true, multiValued = false)
    @Completion(ConnectPointCompleter.class)
    String egressStr = null;

    @Argument(index = 2, name = "bandwidth", description = "Bandwidth",
            required = false, multiValued = false)
    String bandwidthStr = null;

    // not supported yet
    @Argument(index = 3, name = "latency", description = "Latency",
            required = false, multiValued = false)
    String latencyStr = null;


    @Override
    protected void doExecute() {
        OpticalPathService opticalPathService = get(OpticalPathService.class);

        ConnectPoint ingress = readConnectPoint(ingressStr);
        ConnectPoint egress = readConnectPoint(egressStr);
        if (ingress == null || egress == null) {
            print("Invalid connect points: %s, %s", ingressStr, egressStr);
            return;
        }

        Bandwidth bandwidth = (bandwidthStr == null || bandwidthStr.isEmpty()) ? null :
                Bandwidth.bps(Long.valueOf(bandwidthStr));

        print("Trying to setup connectivity between %s and %s.", ingress, egress);
        OpticalConnectivityId id = opticalPathService.setupConnectivity(ingress, egress, bandwidth, null);
        if (id == null) {
            print("Failed. See ONOS log for more details.");
            print(" log:set TRACE org.onosproject.newoptical.OpticalPathProvisioner");
            return;
        }
        // FIXME This is the last chance to know the Optical path ID.
        //       there's no other way to know existing Optical Path ID
        print("Optical path ID : %s", id.id());
        log.info("Optical path ID {} for connectivity between {} and {}",
                 id.id(), ingress, egress);
    }

    private ConnectPoint readConnectPoint(String str) {
        String[] strings = str.split("/");
        if (strings.length != 2) {
            return null;
        }

        DeviceId devId = DeviceId.deviceId(strings[0]);
        PortNumber port = PortNumber.portNumber(strings[1]);

        return new ConnectPoint(devId, port);
    }

}
