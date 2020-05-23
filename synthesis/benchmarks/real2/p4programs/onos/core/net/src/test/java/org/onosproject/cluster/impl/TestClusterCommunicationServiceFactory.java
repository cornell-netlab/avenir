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
package org.onosproject.cluster.impl;

import java.util.Map;

import com.google.common.collect.Maps;
import org.onosproject.cluster.NodeId;
import org.onosproject.store.cluster.messaging.ClusterCommunicationService;

/**
 * Test cluster communication service factory.
 */
public class TestClusterCommunicationServiceFactory {
    private final Map<NodeId, TestClusterCommunicationService> nodes = Maps.newConcurrentMap();

    /**
     * Creates a new cluster communication service for the given node.
     *
     * @param localNodeId the node for which to create the service
     * @return the communication service for the given node
     */
    public ClusterCommunicationService newCommunicationService(NodeId localNodeId) {
        return new TestClusterCommunicationService(localNodeId, nodes);
    }
}
