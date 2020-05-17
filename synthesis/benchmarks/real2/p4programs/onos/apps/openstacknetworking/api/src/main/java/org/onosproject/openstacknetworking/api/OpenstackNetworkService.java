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
package org.onosproject.openstacknetworking.api;

import org.onlab.packet.IpAddress;
import org.onlab.packet.IpPrefix;
import org.onlab.packet.MacAddress;
import org.onosproject.event.ListenerService;
import org.onosproject.openstacknetworking.api.OpenstackNetwork.Type;
import org.openstack4j.model.network.ExternalGateway;
import org.openstack4j.model.network.Network;
import org.openstack4j.model.network.Port;
import org.openstack4j.model.network.Subnet;

import java.util.Set;

/**
 * Service for interacting with the inventory of OpenStack network and port.
 */
public interface OpenstackNetworkService
        extends ListenerService<OpenstackNetworkEvent, OpenstackNetworkListener> {
    /**
     * Returns the network with the supplied network ID.
     *
     * @param networkId network id
     * @return openstack network
     */
    Network network(String networkId);

    /**
     * Returns the network type with the supplied network ID.
     *
     * @param networkId network id
     * @return openstack network type
     */
    Type networkType(String networkId);

    /**
     * Returns all networks registered in the service.
     *
     * @return set of networks
     */
    Set<Network> networks();

    /**
     * Returns the subnet with the supplied subnet ID.
     *
     * @param subnetId subnet id
     * @return subnet
     */
    Subnet subnet(String subnetId);

    /**
     * Returns all subnets registered in the service.
     *
     * @return set of subnet
     */
    Set<Subnet> subnets();

    /**
     * Returns all subnets associated with the supplied network.
     *
     * @param networkId network id
     * @return set of subnet
     */
    Set<Subnet> subnets(String networkId);

    /**
     * Returns the OpenStack port with the supplied port ID.
     *
     * @param portId openstack port id
     * @return openstack port
     */
    Port port(String portId);

    /**
     * Returns the OpenStack port with the supplied ONOS port.
     *
     * @param port onos port
     * @return openstack port
     */
    Port port(org.onosproject.net.Port port);

    /**
     * Returns all OpenStack ports registered in the service.
     *
     * @return set of ports
     */
    Set<Port> ports();

    /**
     * Returns all OpenStack ports associated with supplied network.
     *
     * @param networkId network id
     * @return set of ports
     */
    Set<Port> ports(String networkId);

    /**
     * Obtains a set of fixed IP addresses managed by the given network type.
     *
     * @param type network type (FLAT, VXLAN, VLAN, etc.)
     * @return a set of fixed IP addresses
     */
    Set<IpPrefix> getFixedIpsByNetworkType(String type);

    /**
     * Returns external router mac with supplied external gateway.
     *
     * @param externalGateway external gateway information
     * @return mac address
     */
    MacAddress externalPeerRouterMac(ExternalGateway externalGateway);

    /**
     * Returns external peer router with supplied ip address.
     *
     * @param ipAddress ip address
     * @return external peer router
     */
    ExternalPeerRouter externalPeerRouter(IpAddress ipAddress);

    /**
     * Returns external router with supplied external gateway.
     *
     * @param externalGateway external gateway information
     * @return external router
     */
    ExternalPeerRouter externalPeerRouter(ExternalGateway externalGateway);

    /**
     * Returns external peer router list.
     *
     * @return external peer router list
     */
    Set<ExternalPeerRouter> externalPeerRouters();

    /**
     * Returns ip prefix with supplied port ID.
     *
     * @param portId openstack port id
     * @return ip prefix
     */
    IpPrefix ipPrefix(String portId);

    /**
     * Returns gateway ip address with supplied port ID.
     *
     * @param portId openstack port id
     * @return gateway ip address
     */
    String gatewayIp(String portId);

    /**
     * Returns network segment ID with supplied network ID.
     *
     * @param netId openstack network ID
     * @return network segment ID
     */
    String segmentId(String netId);
}
