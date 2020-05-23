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

package org.onosproject.evpnopenflow.rsc.cli;

import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.evpnopenflow.rsc.VpnPort;
import org.onosproject.evpnopenflow.rsc.vpnport.VpnPortService;

import java.util.Collection;

import static org.onosproject.evpnopenflow.rsc.EvpnConstants.FORMAT_VPN_PORT;

/**
 * Support for displaying EVPN VPN ports.
 */
@Service
@Command(scope = "onos", name = "evpn-port-list", description = "Lists all" +
        "EVPN ports")
public class VpnPortListCommand extends AbstractShellCommand {

    @Override
    protected void doExecute() {
        VpnPortService portService = get(VpnPortService.class);
        Collection<VpnPort> ports = portService.getPorts();
        ports.forEach(port -> {
            print(FORMAT_VPN_PORT, port.id(), port.vpnInstanceId());
        });
    }

}
