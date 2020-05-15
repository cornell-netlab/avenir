/*
 * Copyright 2015-present Open Networking Foundation
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
package org.onosproject.pcep.server;

import org.onosproject.incubator.net.tunnel.DefaultLabelStack;
import org.onosproject.incubator.net.tunnel.LabelStack;
import org.onosproject.incubator.net.tunnel.Tunnel;
import org.onosproject.net.Path;
import org.onosproject.pcepio.protocol.PcepMessage;
import org.onosproject.pcepio.types.PcepValueType;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Abstraction of an Pcep client controller. Serves as a one stop
 * shop for obtaining Pcep devices and (un)register listeners
 * on pcep events
 */
public interface PcepClientController {

    /**
     * Returns list of pcc clients connected to this Pcep controller.
     *
     * @return list of PcepClient elements
     */
    Collection<PcepClient> getClients();

    /**
     * Returns the actual pcc client for the given ip address.
     *
     * @param pccId the id of the pcc client to fetch
     * @return the interface to this pcc client
     */
    PcepClient getClient(PccId pccId);

    /**
     * Register a listener for meta events that occur to pcep
     * devices.
     *
     * @param listener the listener to notify
     */
    void addListener(PcepClientListener listener);

    /**
     * Unregister a listener.
     *
     * @param listener the listener to unregister
     */
    void removeListener(PcepClientListener listener);

    /**
     * Register a listener for PCEP msg events.
     *
     * @param listener the listener to notify
     */
    void addEventListener(PcepEventListener listener);

    /**
     * Unregister a listener.
     *
     * @param listener the listener to unregister
     */
    void removeEventListener(PcepEventListener listener);

    /**
     * Register a listener for PCEP msg events[carrying node descriptor details].
     *
     * @param listener the listener to notify
     */
    void addNodeListener(PcepNodeListener listener);

    /**
     * Unregister a listener.
     *
     * @param listener the listener to be unregistered
     */
    void removeNodeListener(PcepNodeListener listener);

    /**
     * Send a message to a particular pcc client.
     *
     * @param pccId the id of the client to send message.
     * @param msg the message to send
     */
    void writeMessage(PccId pccId, PcepMessage msg);

    /**
     * Process a message and notify the appropriate listeners.
     *
     * @param pccId id of the client the message arrived on
     * @param msg the message to process.
     */
    void processClientMessage(PccId pccId, PcepMessage msg);

    /**
     * Close all connected PCC clients.
     */
    void closeConnectedClients();

    /**
     * Create label stack from the given path.
     *
     * @param path from which label stack is to be computed
     * @return the label stack
     */
    public LabelStack computeLabelStack(Path path);

    /**
     * Allocates and downloads local labels for the given LSP.
     *
     * @param tunnel for which local labels have to be assigned and downloaded
     * @return success or failure
     */
    public boolean allocateLocalLabel(Tunnel tunnel);

    /**
     * Creates label stack for ERO object from network resource.
     *
     * @param labelStack label stack
     * @param path (hop list)
     * @return list of ERO sub-objects
     */
    public LinkedList<PcepValueType> createPcepLabelStack(DefaultLabelStack labelStack, Path path);

    /**
     * Returns list of PCEP exceptions.
     *
     * @return PcepExceptions
     */
    public Map<String, List<String>> getPcepExceptions();

    /**
     * Returns all the pcep error messages received .
     *
     * @return PcepErrorMsg
     */
    public Map<Integer, Integer> getPcepErrorMsg();

    /**
     * Returns the pcep session details.
     *
     * @return PcepSession
     */
    public Map<String, String> getPcepSessionMap();

    /**
     * Returns the pcep sessionid information.
     *
     * @return PcepSessionId
     */
    public Map<String, Byte> getPcepSessionIdMap();

    /**
     * Creates detailed information about pcep error value and type per peer.
     *
     * @param peerId id of the peer which sent the error message
     * @param errorType the error type of the error message received
     * @param errValue the error value of the error message received
     */
    void peerErrorMsg(String peerId, Integer errorType, Integer errValue);
}
