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

package org.onosproject.incubator.net.virtual.impl;

import org.onosproject.common.DefaultTopology;
import org.onosproject.incubator.net.virtual.NetworkId;
import org.onosproject.incubator.net.virtual.VirtualNetworkService;
import org.onosproject.incubator.net.virtual.event.AbstractVirtualListenerManager;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.Device;
import org.onosproject.net.DeviceId;
import org.onosproject.net.DisjointPath;
import org.onosproject.net.Link;
import org.onosproject.net.Path;
import org.onosproject.net.topology.ClusterId;
import org.onosproject.net.topology.DefaultGraphDescription;
import org.onosproject.net.topology.LinkWeigher;
import org.onosproject.net.topology.Topology;
import org.onosproject.net.topology.TopologyCluster;
import org.onosproject.net.topology.TopologyEvent;
import org.onosproject.net.topology.TopologyGraph;
import org.onosproject.net.topology.TopologyListener;
import org.onosproject.net.topology.TopologyService;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;
import static org.onosproject.incubator.net.virtual.DefaultVirtualLink.PID;

/**
 * Topology service implementation built on the virtual network service.
 */
public class VirtualNetworkTopologyManager
        extends AbstractVirtualListenerManager<TopologyEvent, TopologyListener>
        implements TopologyService {

    private static final String TOPOLOGY_NULL = "Topology cannot be null";
    private static final String DEVICE_ID_NULL = "Device ID cannot be null";
    private static final String CLUSTER_ID_NULL = "Cluster ID cannot be null";
    private static final String CLUSTER_NULL = "Topology cluster cannot be null";
    private static final String CONNECTION_POINT_NULL = "Connection point cannot be null";
    private static final String LINK_WEIGHT_NULL = "Link weight cannot be null";

    /**
     * Creates a new VirtualNetworkTopologyService object.
     *
     * @param virtualNetworkManager virtual network manager service
     * @param networkId a virtual network identifier
     */
    public VirtualNetworkTopologyManager(VirtualNetworkService virtualNetworkManager,
                                         NetworkId networkId) {
        super(virtualNetworkManager, networkId, TopologyEvent.class);
    }

    @Override
    public Topology currentTopology() {
        Iterable<Device> devices = manager.getVirtualDevices(networkId())
                .stream()
                .collect(Collectors.toSet());
        Iterable<Link> links = manager.getVirtualLinks(networkId())
                .stream()
                .collect(Collectors.toSet());

        DefaultGraphDescription graph =
                new DefaultGraphDescription(System.nanoTime(),
                                            System.currentTimeMillis(),
                                            devices, links);
        return new DefaultTopology(PID, graph);
    }

    @Override
    public boolean isLatest(Topology topology) {
        Topology currentTopology = currentTopology();
        return defaultTopology(topology).getGraph()
                .equals(defaultTopology(currentTopology).getGraph());
    }

    @Override
    public TopologyGraph getGraph(Topology topology) {
        return defaultTopology(topology).getGraph();
    }

    // Validates the specified topology and returns it as a default
    private DefaultTopology defaultTopology(Topology topology) {
        checkNotNull(topology, TOPOLOGY_NULL);
        checkArgument(topology instanceof DefaultTopology,
                      "Topology class %s not supported", topology.getClass());
        return (DefaultTopology) topology;
    }

    @Override
    public Set<TopologyCluster> getClusters(Topology topology) {
        return defaultTopology(topology).getClusters();
    }

    @Override
    public TopologyCluster getCluster(Topology topology, ClusterId clusterId) {
        checkNotNull(clusterId, CLUSTER_ID_NULL);
        return defaultTopology(topology).getCluster(clusterId);
    }

    @Override
    public Set<DeviceId> getClusterDevices(Topology topology, TopologyCluster cluster) {
        checkNotNull(cluster, CLUSTER_NULL);
        return defaultTopology(topology).getClusterDevices(cluster);
    }

    @Override
    public Set<Link> getClusterLinks(Topology topology, TopologyCluster cluster) {
        checkNotNull(cluster, CLUSTER_NULL);
        return defaultTopology(topology).getClusterLinks(cluster);
    }

    @Override
    public Set<Path> getPaths(Topology topology, DeviceId src, DeviceId dst) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        return defaultTopology(topology).getPaths(src, dst);
    }

    @Override
    public Set<Path> getPaths(Topology topology, DeviceId src, DeviceId dst,
                              LinkWeigher weigher) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        checkNotNull(weigher, LINK_WEIGHT_NULL);
        return defaultTopology(topology).getPaths(src, dst, weigher);
    }

    @Override
    public Set<DisjointPath> getDisjointPaths(Topology topology, DeviceId src,
                                              DeviceId dst) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        return defaultTopology(topology).getDisjointPaths(src, dst);
    }

    @Override
    public Set<DisjointPath> getDisjointPaths(Topology topology, DeviceId src,
                                              DeviceId dst,
                                              LinkWeigher weigher) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        checkNotNull(weigher, LINK_WEIGHT_NULL);
        return defaultTopology(topology).getDisjointPaths(src, dst, weigher);
    }

    @Override
    public Set<DisjointPath> getDisjointPaths(Topology topology, DeviceId src,
                                              DeviceId dst,
                                              Map<Link, Object> riskProfile) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        return defaultTopology(topology).getDisjointPaths(src, dst, riskProfile);
    }

    @Override
    public Set<DisjointPath> getDisjointPaths(Topology topology, DeviceId src,
                                              DeviceId dst,
                                              LinkWeigher weigher,
                                              Map<Link, Object> riskProfile) {
        checkNotNull(src, DEVICE_ID_NULL);
        checkNotNull(dst, DEVICE_ID_NULL);
        checkNotNull(weigher, LINK_WEIGHT_NULL);
        return defaultTopology(topology).getDisjointPaths(src, dst, weigher,
                riskProfile);
    }

    @Override
    public boolean isInfrastructure(Topology topology, ConnectPoint connectPoint) {
        checkNotNull(connectPoint, CONNECTION_POINT_NULL);
        return defaultTopology(topology).isInfrastructure(connectPoint);
    }

    @Override
    public boolean isBroadcastPoint(Topology topology, ConnectPoint connectPoint) {
        checkNotNull(connectPoint, CONNECTION_POINT_NULL);
        return defaultTopology(topology).isBroadcastPoint(connectPoint);
    }
}
