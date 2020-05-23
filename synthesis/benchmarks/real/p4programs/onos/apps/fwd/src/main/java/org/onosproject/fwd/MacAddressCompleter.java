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
package org.onosproject.fwd;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.apache.karaf.shell.api.console.CommandLine;
import org.apache.karaf.shell.api.console.Completer;
import org.apache.karaf.shell.api.console.Session;
import org.apache.karaf.shell.support.completers.StringsCompleter;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.store.service.EventuallyConsistentMap;
import java.util.List;
import java.util.SortedSet;
import org.onlab.packet.MacAddress;

/**
 * Sample reactive forwarding application.
 */
@Service
public class MacAddressCompleter implements Completer {
    @Override
    public int complete(Session session, CommandLine commandLine, List<String> candidates) {
        // Delegate string completer
        StringsCompleter delegate = new StringsCompleter();
        EventuallyConsistentMap<MacAddress, ReactiveForwardMetrics> macAddress;
        // Fetch our service and feed it's offerings to the string completer
        ReactiveForwarding reactiveForwardingService = AbstractShellCommand.get(ReactiveForwarding.class);
        macAddress = reactiveForwardingService.getMacAddress();
        SortedSet<String> strings = delegate.getStrings();
        for (MacAddress key : macAddress.keySet()) {
            strings.add(key.toString());
        }
        // Now let the completer do the work for figuring out what to offer.
        return delegate.complete(session, commandLine, candidates);
    }
}
