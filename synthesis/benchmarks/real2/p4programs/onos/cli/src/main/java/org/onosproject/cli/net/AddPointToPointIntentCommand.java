/*
 * Copyright 2014-present Open Networking Foundation
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
package org.onosproject.cli.net;

import org.apache.karaf.shell.api.action.Argument;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.Completion;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.apache.karaf.shell.api.action.Option;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.FilteredConnectPoint;
import org.onosproject.net.flow.TrafficSelector;
import org.onosproject.net.flow.TrafficTreatment;
import org.onosproject.net.intent.Constraint;
import org.onosproject.net.intent.Intent;
import org.onosproject.net.intent.IntentService;
import org.onosproject.net.intent.PointToPointIntent;
import org.onosproject.net.intent.constraint.ProtectedConstraint;

import static org.onosproject.net.intent.constraint.ProtectionConstraint.protection;

import java.util.List;

/**
 * Installs point-to-point connectivity intents.
 */
@Service
@Command(scope = "onos", name = "add-point-intent",
         description = "Installs point-to-point connectivity intent")
public class AddPointToPointIntentCommand extends ConnectivityIntentCommand {

    @Argument(index = 0, name = "ingressDevice",
              description = "Ingress Device/Port Description",
              required = true, multiValued = false)
    @Completion(ConnectPointCompleter.class)
    String ingressDeviceString = null;

    @Argument(index = 1, name = "egressDevice",
              description = "Egress Device/Port Description",
              required = true, multiValued = false)
    @Completion(ConnectPointCompleter.class)
    String egressDeviceString = null;

    private boolean backup = false;

    /**
     * Option to consume protected path.
     */
    @Option(name = "--useProtected",
            description = "use protected links only")
    private boolean useProtected = false;

    @Override
    protected void doExecute() {
        IntentService service = get(IntentService.class);

        ConnectPoint ingress = ConnectPoint.deviceConnectPoint(ingressDeviceString);

        ConnectPoint egress = ConnectPoint.deviceConnectPoint(egressDeviceString);

        TrafficSelector selector = buildTrafficSelector();
        TrafficTreatment treatment = buildTrafficTreatment();

        List<Constraint> constraints = buildConstraints();
        if (backup) {
            constraints.add(protection());
        }

        if (useProtected) {
            constraints.add(ProtectedConstraint.useProtectedLink());
        }

        Intent intent = PointToPointIntent.builder()
                .appId(appId())
                .key(key())
                .selector(selector)
                .treatment(treatment)
                .filteredIngressPoint(new FilteredConnectPoint(ingress))
                .filteredEgressPoint(new FilteredConnectPoint(egress))
                .constraints(constraints)
                .priority(priority())
                .resourceGroup(resourceGroup())
                .build();
        service.submit(intent);
        print("Point to point intent submitted:\n%s", intent.toString());
    }
}
