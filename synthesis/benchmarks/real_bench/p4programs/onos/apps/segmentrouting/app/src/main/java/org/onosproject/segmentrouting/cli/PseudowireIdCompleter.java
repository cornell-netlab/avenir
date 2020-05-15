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
package org.onosproject.segmentrouting.cli;

import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.apache.karaf.shell.api.console.CommandLine;
import org.apache.karaf.shell.api.console.Completer;
import org.apache.karaf.shell.api.console.Session;
import org.apache.karaf.shell.support.completers.StringsCompleter;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.segmentrouting.SegmentRoutingService;
import org.onosproject.segmentrouting.pwaas.L2Tunnel;

import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.stream.Collectors;

/**
 * Device ID completer.
 */
@Service
public class PseudowireIdCompleter implements Completer {
    @Override
    public int complete(Session session, CommandLine commandLine, List<String> candidates) {
        // Delegate string completer
        StringsCompleter delegate = new StringsCompleter();

        SegmentRoutingService srService =
                AbstractShellCommand.get(SegmentRoutingService.class);


        List<L2Tunnel> tunnels = srService.getL2Tunnels();

        // combine polices and tunnels to pseudowires
        Iterator<String> pseudowires = tunnels.stream()
                .map(l2Tunnel -> Long.toString(l2Tunnel.tunnelId()))
                .collect(Collectors.toList()).iterator();

        SortedSet<String> strings = delegate.getStrings();
        while (pseudowires.hasNext()) {
            strings.add(pseudowires.next());
        }

        // Now let the completer do the work for figuring out what to offer.
        return delegate.complete(session, commandLine, candidates);
    }

}
