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
package org.onosproject.k8snetworking.api;

import org.onlab.packet.IpAddress;

/**
 * Representation of kubernetes network.
 */
public interface K8sNetwork {

    /**
     * Lists of network type.
     */
    enum Type {

        /**
         * VXLAN typed virtual network.
         */
        VXLAN,

        /**
         * GRE typed virtual network.
         */
        GRE,

        /**
         * GENEVE typed virtual network.
         */
        GENEVE,
    }

    /**
     * Returns the kubernetes network ID.
     *
     * @return kubernetes network ID
     */
    String networkId();

    /**
     * Returns kubernetes network type.
     *
     * @return kubernetes network type
     */
    Type type();

    /**
     * Returns kubernetes network name.
     *
     * @return kubernetes network name
     */
    String name();

    /**
     * Returns maximum transmission unit (MTU) value to address fragmentation.
     *
     * @return maximum transmission unit (MTU) value to address fragmentation
     */
    Integer mtu();

    /**
     * Returns segmentation ID.
     *
     * @return segmentation ID
     */
    String segmentId();

    /**
     * Returns gateway IP address.
     *
     * @return gateway IP address
     */
    IpAddress gatewayIp();

    /**
     * Returns network CIDR.
     *
     * @return network CIDR
     */
    String cidr();

    /**
     * Builder of new network.
     */
    interface Builder {

        /**
         * Builds an immutable network instance.
         *
         * @return kubernetes network
         */
        K8sNetwork build();

        /**
         * Returns network builder with supplied network ID.
         *
         * @param networkId network ID
         * @return network builder
         */
        Builder networkId(String networkId);

        /**
         * Returns network builder with supplied network name.
         *
         * @param name network name
         * @return network builder
         */
        Builder name(String name);

        /**
         * Returns network builder with supplied network type.
         *
         * @param type network type
         * @return network builder
         */
        Builder type(Type type);

        /**
         * Returns network builder with supplied MTU.
         *
         * @param mtu maximum transmission unit
         * @return network builder
         */
        Builder mtu(Integer mtu);

        /**
         * Returns network builder with supplied segment ID.
         *
         * @param segmentId segment ID
         * @return network builder
         */
        Builder segmentId(String segmentId);

        /**
         * Returns network builder with supplied gateway IP address.
         *
         * @param ipAddress gateway IP address
         * @return network builder
         */
        Builder gatewayIp(IpAddress ipAddress);

        /**
         * Returns network builder with supplied network CIDR.
         *
         * @param cidr Classless Inter-Domain Routing
         * @return network builder
         */
        Builder cidr(String cidr);
    }
}
