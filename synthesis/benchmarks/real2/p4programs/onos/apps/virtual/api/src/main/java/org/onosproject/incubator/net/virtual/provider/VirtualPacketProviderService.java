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
package org.onosproject.incubator.net.virtual.provider;

import org.onosproject.net.packet.PacketContext;

/**
 * Entity capable of processing inbound packets for virtual networks.
 */
public interface VirtualPacketProviderService
        extends VirtualProviderService<VirtualPacketProvider> {

    /**
     * Submits inbound packet context for processing to virtual network core.
     * This processing will be done synchronously, i.e. run-to-completion.
     * This context is translated with virtual concepts.
     *
     * @param context inbound packet context
     */
    void processPacket(PacketContext context);

}
