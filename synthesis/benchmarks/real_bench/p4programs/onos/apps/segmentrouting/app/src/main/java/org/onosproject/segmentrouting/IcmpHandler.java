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
package org.onosproject.segmentrouting;

import org.onlab.packet.Ethernet;
import org.onlab.packet.ICMP;
import org.onlab.packet.ICMP6;
import org.onlab.packet.IPv4;
import org.onlab.packet.IPv6;
import org.onlab.packet.Ip4Address;
import org.onlab.packet.Ip6Address;
import org.onlab.packet.IpAddress;
import org.onlab.packet.MPLS;
import org.onlab.packet.MacAddress;
import org.onlab.packet.VlanId;
import org.onlab.packet.ndp.NeighborSolicitation;
import org.onosproject.net.neighbour.NeighbourMessageContext;
import org.onosproject.net.neighbour.NeighbourMessageType;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.DeviceId;
import org.onosproject.net.intf.Interface;
import org.onosproject.net.flow.DefaultTrafficTreatment;
import org.onosproject.net.flow.TrafficTreatment;
import org.onosproject.net.host.HostService;
import org.onosproject.net.packet.DefaultOutboundPacket;
import org.onosproject.net.packet.OutboundPacket;
import org.onosproject.segmentrouting.config.DeviceConfigNotFoundException;
import org.onosproject.segmentrouting.config.SegmentRoutingAppConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Handler of ICMP packets that responses or forwards ICMP packets that
 * are sent to the controller.
 */
public class IcmpHandler extends SegmentRoutingNeighbourHandler {

    private static Logger log = LoggerFactory.getLogger(IcmpHandler.class);

    /**
     * Creates an IcmpHandler object.
     *
     * @param srManager SegmentRoutingManager object
     */
    public IcmpHandler(SegmentRoutingManager srManager) {
        super(srManager);
    }

    /**
     * Utility function to send packet out.
     *
     * @param outport the output port
     * @param payload the packet to send
     * @param destSid the segment id of the dest device
     * @param destIpAddress the destination ip address
     * @param allowedHops the hop limit/ttl
     */
    private void sendPacketOut(ConnectPoint outport,
                               Ethernet payload,
                               int destSid,
                               IpAddress destIpAddress,
                               byte allowedHops) {
        int origSid;
        try {
            if (destIpAddress.isIp4()) {
                origSid = config.getIPv4SegmentId(outport.deviceId());
            } else {
                origSid = config.getIPv6SegmentId(outport.deviceId());
            }
        } catch (DeviceConfigNotFoundException e) {
            log.warn(e.getMessage() + " Aborting sendPacketOut");
            return;
        }

        if (destSid == -1 || origSid == destSid ||
                srManager.interfaceService.isConfigured(outport)) {
            TrafficTreatment treatment = DefaultTrafficTreatment.builder().
                    setOutput(outport.port()).build();
            OutboundPacket packet = new DefaultOutboundPacket(outport.deviceId(),
                                                              treatment, ByteBuffer.wrap(payload.serialize()));
            log.trace("Sending packet {} to {}", payload, outport);
            srManager.packetService.emit(packet);
        } else {
            TrafficTreatment treatment = DefaultTrafficTreatment.builder()
                    .setOutput(outport.port())
                    .build();

            payload.setEtherType(Ethernet.MPLS_UNICAST);
            MPLS mplsPkt = new MPLS();
            mplsPkt.setLabel(destSid);
            mplsPkt.setTtl(allowedHops);
            mplsPkt.setPayload(payload.getPayload());
            payload.setPayload(mplsPkt);
            OutboundPacket packet = new DefaultOutboundPacket(outport.deviceId(),
                                                              treatment, ByteBuffer.wrap(payload.serialize()));
            log.trace("Sending packet {} to {}", payload, outport);
            srManager.packetService.emit(packet);
        }
    }

    private IpAddress selectRouterIpAddress(IpAddress destIpAddress, ConnectPoint outPort,
                                            Set<ConnectPoint> connectPoints) {
        IpAddress routerIpAddress;
        // Let's get first the online connect points
        Set<ConnectPoint> onlineCps =  connectPoints.stream()
                .filter(connectPoint -> srManager.deviceService.isAvailable(connectPoint.deviceId()))
                .collect(Collectors.toSet());
        // Check if ping is local
        if (onlineCps.contains(outPort)) {
            routerIpAddress = config.getRouterIpAddress(destIpAddress, outPort.deviceId());
            log.trace("Local ping received from {} - send to {}", destIpAddress, routerIpAddress);
            return routerIpAddress;
        }
        // Check if it comes from a remote device. Loopbacks are sorted comparing byte by byte
        // FIXME if we lose both links from the chosen leaf to spine - ping will fail
        routerIpAddress = onlineCps.stream()
                .filter(onlineCp -> !onlineCp.deviceId().equals(outPort.deviceId()))
                .map(selectedCp -> config.getRouterIpAddress(destIpAddress, selectedCp.deviceId()))
                .filter(Objects::nonNull)
                .sorted()
                .findFirst().orElse(null);
        if (routerIpAddress != null) {
            log.trace("Remote ping received from {} - send to {}", destIpAddress, routerIpAddress);
        } else {
            log.warn("Not found a valid loopback for ping coming from {} - {}", destIpAddress, outPort);
        }
        return routerIpAddress;
    }

    private Ip4Address selectRouterIp4Address(IpAddress destIpAddress, ConnectPoint outPort,
                                              Set<ConnectPoint> connectPoints) {
        IpAddress routerIpAddress = selectRouterIpAddress(destIpAddress, outPort, connectPoints);
        return routerIpAddress != null ? routerIpAddress.getIp4Address() : null;
    }

    private Ip6Address selectRouterIp6Address(IpAddress destIpAddress, ConnectPoint outPort,
                                              Set<ConnectPoint> connectPoints) {
        IpAddress routerIpAddress = selectRouterIpAddress(destIpAddress, outPort, connectPoints);
        return routerIpAddress != null ? routerIpAddress.getIp6Address() : null;
    }

    //////////////////////////////////////
    //     ICMP Echo/Reply Protocol     //
    //////////////////////////////////////

    /**
     * Process incoming ICMP packet.
     * If it is an ICMP request to router, then sends an ICMP response.
     * Otherwise ignore the packet.
     *
     * @param eth inbound ICMP packet
     * @param inPort the input port
     */
    public void processIcmp(Ethernet eth, ConnectPoint inPort) {
        DeviceId deviceId = inPort.deviceId();
        IPv4 ipv4Packet = (IPv4) eth.getPayload();
        ICMP icmp = (ICMP) ipv4Packet.getPayload();
        Ip4Address destinationAddress = Ip4Address.valueOf(ipv4Packet.getDestinationAddress());
        Set<IpAddress> gatewayIpAddresses = config.getPortIPs(deviceId);
        IpAddress routerIp;

        // Only proceed with echo request
        if (icmp.getIcmpType() != ICMP.TYPE_ECHO_REQUEST) {
            return;
        }

        try {
            routerIp = config.getRouterIpv4(deviceId);
        } catch (DeviceConfigNotFoundException e) {
            log.warn(e.getMessage() + " Aborting processPacketIn.");
            return;
        }

        // Get pair ip - if it exists
        IpAddress pairRouterIp;
        try {
            DeviceId pairDeviceId = config.getPairDeviceId(deviceId);
            pairRouterIp = pairDeviceId != null ? config.getRouterIpv4(pairDeviceId) : null;
        } catch (DeviceConfigNotFoundException e) {
            pairRouterIp = null;
        }

        // ICMP to the router IP or gateway IP
        if (destinationAddress.equals(routerIp.getIp4Address()) ||
                (pairRouterIp != null && destinationAddress.equals(pairRouterIp.getIp4Address())) ||
                gatewayIpAddresses.contains(destinationAddress)) {
            sendIcmpResponse(eth, inPort);
        } else {
            log.trace("Ignore ICMP that targets for {}", destinationAddress);
        }
    }

    /**
     * Sends an ICMP reply message.
     *
     * @param icmpRequest the original ICMP request
     * @param outport the output port where the ICMP reply should be sent to
     */
    private void sendIcmpResponse(Ethernet icmpRequest, ConnectPoint outport) {
        Ethernet icmpReplyEth = ICMP.buildIcmpReply(icmpRequest);
        IPv4 icmpRequestIpv4 = (IPv4) icmpRequest.getPayload();
        IPv4 icmpReplyIpv4 = (IPv4) icmpReplyEth.getPayload();
        Ip4Address destIpAddress = Ip4Address.valueOf(icmpRequestIpv4.getSourceAddress());

        // Get the available connect points
        Set<ConnectPoint> destConnectPoints = config.getConnectPointsForASubnetHost(destIpAddress);
        // Select a router
        Ip4Address destRouterAddress = selectRouterIp4Address(destIpAddress, outport, destConnectPoints);

        // Note: Source IP of the ICMP request doesn't belong to any configured subnet.
        //       The source might be an indirectly attached host (e.g. behind a router)
        //       Lookup the route store for the nexthop instead.
        if (destRouterAddress == null) {
            Optional<DeviceId> deviceId = srManager.routeService
                    .longestPrefixLookup(destIpAddress).map(srManager::nextHopLocations)
                    .flatMap(locations -> locations.stream().findFirst())
                    .map(ConnectPoint::deviceId);
            if (deviceId.isPresent()) {
                try {
                    destRouterAddress = config.getRouterIpv4(deviceId.get());
                } catch (DeviceConfigNotFoundException e) {
                    log.warn("Device config for {} not found. Abort ICMP processing", deviceId);
                    return;
                }
            }
        }

        int destSid = config.getIPv4SegmentId(destRouterAddress);
        if (destSid < 0) {
            log.warn("Failed to lookup SID of the switch that {} attaches to. " +
                    "Unable to process ICMP request.", destIpAddress);
            return;
        }
        sendPacketOut(outport, icmpReplyEth, destSid, destIpAddress, icmpReplyIpv4.getTtl());
    }

    ///////////////////////////////////////////
    //      ICMPv6 Echo/Reply Protocol       //
    ///////////////////////////////////////////

    /**
     * Process incoming ICMPv6 packet.
     * If it is an ICMPv6 request to router, then sends an ICMPv6 response.
     * Otherwise ignore the packet.
     *
     * @param eth the incoming ICMPv6 packet
     * @param inPort the input port
     */
    public void processIcmpv6(Ethernet eth, ConnectPoint inPort) {
        DeviceId deviceId = inPort.deviceId();
        IPv6 ipv6Packet = (IPv6) eth.getPayload();
        ICMP6 icmp6 = (ICMP6) ipv6Packet.getPayload();
        Ip6Address destinationAddress = Ip6Address.valueOf(ipv6Packet.getDestinationAddress());
        Set<IpAddress> gatewayIpAddresses = config.getPortIPs(deviceId);
        IpAddress routerIp;

        // Only proceed with echo request
        if (icmp6.getIcmpType() != ICMP6.ECHO_REQUEST) {
            return;
        }

        try {
            routerIp = config.getRouterIpv6(deviceId);
        } catch (DeviceConfigNotFoundException e) {
            log.warn(e.getMessage() + " Aborting processPacketIn.");
            return;
        }

        // Get pair ip - if it exists
        IpAddress pairRouterIp;
        try {
            DeviceId pairDeviceId = config.getPairDeviceId(deviceId);
            pairRouterIp = pairDeviceId != null ? config.getRouterIpv6(pairDeviceId) : null;
        } catch (DeviceConfigNotFoundException e) {
            pairRouterIp = null;
        }

        Optional<Ip6Address> linkLocalIp = getLinkLocalIp(inPort);
        // Ensure ICMP to the router IP, EUI-64 link-local IP, or gateway IP
        if (destinationAddress.equals(routerIp.getIp6Address()) ||
                (linkLocalIp.isPresent() && destinationAddress.equals(linkLocalIp.get())) ||
                (pairRouterIp != null && destinationAddress.equals(pairRouterIp.getIp6Address())) ||
                gatewayIpAddresses.contains(destinationAddress)) {
            sendIcmpv6Response(eth, inPort);
        } else {
            log.trace("Ignore ICMPv6 that targets for {}", destinationAddress);
        }
    }

    /**
     * Sends an ICMPv6 reply message.
     *
     * @param ethRequest the original ICMP request
     * @param outport the output port where the ICMP reply should be sent to
     */
    private void sendIcmpv6Response(Ethernet ethRequest, ConnectPoint outport) {
        int destSid = -1;
        Ethernet ethReply = ICMP6.buildIcmp6Reply(ethRequest);
        IPv6 icmpRequestIpv6 = (IPv6) ethRequest.getPayload();
        IPv6 icmpReplyIpv6 = (IPv6) ethRequest.getPayload();
        // Source IP of the echo "reply"
        Ip6Address srcIpAddress = Ip6Address.valueOf(icmpRequestIpv6.getDestinationAddress());
        // Destination IP of the echo "reply"
        Ip6Address destIpAddress = Ip6Address.valueOf(icmpRequestIpv6.getSourceAddress());
        Optional<Ip6Address> linkLocalIp = getLinkLocalIp(outport);

        // Fast path if the echo request targets the link-local address of switch interface
        if (linkLocalIp.isPresent() && srcIpAddress.equals(linkLocalIp.get())) {
            sendPacketOut(outport, ethReply, destSid, destIpAddress, icmpReplyIpv6.getHopLimit());
            return;
        }

        // Get the available connect points
        Set<ConnectPoint> destConnectPoints = config.getConnectPointsForASubnetHost(destIpAddress);
        // Select a router
        Ip6Address destRouterAddress = selectRouterIp6Address(destIpAddress, outport, destConnectPoints);

        // Note: Source IP of the ICMP request doesn't belong to any configured subnet.
        //       The source might be an indirect host behind a router.
        //       Lookup the route store for the nexthop instead.
        if (destRouterAddress == null) {
            Optional<DeviceId> deviceId = srManager.routeService
                    .longestPrefixLookup(destIpAddress).map(srManager::nextHopLocations)
                    .flatMap(locations -> locations.stream().findFirst())
                    .map(ConnectPoint::deviceId);
            if (deviceId.isPresent()) {
                try {
                    destRouterAddress = config.getRouterIpv6(deviceId.get());
                } catch (DeviceConfigNotFoundException e) {
                    log.warn("Device config for {} not found. Abort ICMPv6 processing", deviceId);
                    return;
                }
            }
        }

        destSid = config.getIPv6SegmentId(destRouterAddress);
        if (destSid < 0) {
            log.warn("Failed to lookup SID of the switch that {} attaches to. " +
                    "Unable to process ICMPv6 request.", destIpAddress);
            return;
        }
        sendPacketOut(outport, ethReply, destSid, destIpAddress, icmpReplyIpv6.getHopLimit());
    }

    ///////////////////////////////////////////
    //  ICMPv6 Neighbour Discovery Protocol  //
    ///////////////////////////////////////////

    /**
     * Process incoming NDP packet.
     *
     * If it is an NDP request for the router or for the gateway, then sends a NDP reply.
     * If it is an NDP request to unknown host flood in the subnet.
     * If it is an NDP packet to known host forward the packet to the host.
     *
     * FIXME If the NDP packets use link local addresses we fail.
     *
     * @param pkt inbound packet
     * @param hostService the host service
     */
    public void processPacketIn(NeighbourMessageContext pkt, HostService hostService) {
        // First we validate the ndp packet
        SegmentRoutingAppConfig appConfig = srManager.cfgService
                .getConfig(srManager.appId, SegmentRoutingAppConfig.class);
        if (appConfig != null && appConfig.suppressSubnet().contains(pkt.inPort())) {
            // Ignore NDP packets come from suppressed ports
            pkt.drop();
            return;
        }

        if (pkt.type() == NeighbourMessageType.REQUEST) {
            handleNdpRequest(pkt, hostService);
        } else {
            handleNdpReply(pkt, hostService);
        }

    }

    /**
     * Helper method to handle the ndp requests.
     * @param pkt the ndp packet request and context information
     * @param hostService the host service
     */
    private void handleNdpRequest(NeighbourMessageContext pkt, HostService hostService) {
        // ND request for the gateway. We have to reply on behalf of the gateway.
        if (isNdpForGateway(pkt)) {
            log.trace("Sending NDP reply on behalf of gateway IP for pkt: {}", pkt.target());
            MacAddress routerMac = config.getRouterMacForAGatewayIp(pkt.target());
            if (routerMac == null) {
                log.warn("Router MAC of {} is not configured. Cannot handle NDP request from {}",
                        pkt.inPort().deviceId(), pkt.sender());
                return;
            }
            sendResponse(pkt, routerMac, hostService, true);
        } else {

            // Process NDP targets towards EUI-64 address.
            try {
                DeviceId deviceId = pkt.inPort().deviceId();

                Optional<Ip6Address> linkLocalIp = getLinkLocalIp(pkt.inPort());
                if (linkLocalIp.isPresent() && pkt.target().equals(linkLocalIp.get())) {
                    MacAddress routerMac = config.getDeviceMac(deviceId);
                    sendResponse(pkt, routerMac, hostService, true);
                }
            } catch (DeviceConfigNotFoundException e) {
                log.warn(e.getMessage() + " Unable to handle NDP packet to {}. Aborting.", pkt.target());
                return;
            }

            // NOTE: Ignore NDP packets except those target for the router
            //       We will reconsider enabling this when we have host learning support
            /*
            // ND request for an host. We do a search by Ip.
            Set<Host> hosts = hostService.getHostsByIp(pkt.target());
            // Possible misconfiguration ? In future this case
            // should be handled we can have same hosts in different VLANs.
            if (hosts.size() > 1) {
                log.warn("More than one host with IP {}", pkt.target());
            }
            Host targetHost = hosts.stream().findFirst().orElse(null);
            // If we know the host forward to its attachment point.
            if (targetHost != null) {
                log.debug("Forward NDP request to the target host");
                pkt.forward(targetHost.location());
            } else {
                // Flood otherwise.
                log.debug("Flood NDP request to the target subnet");
                flood(pkt);
            }
            */
        }
    }

    /**
     * Helper method to handle the ndp replies.
     *
     * @param pkt the ndp packet reply and context information
     * @param hostService the host service
     */
    private void handleNdpReply(NeighbourMessageContext pkt, HostService hostService) {
        if (isNdpForGateway(pkt)) {
            log.debug("Forwarding all the ip packets we stored");
            Ip6Address hostIpAddress = pkt.sender().getIp6Address();
            srManager.ipHandler.forwardPackets(pkt.inPort().deviceId(), hostIpAddress);
        } else {
            // NOTE: Ignore NDP packets except those target for the router
            //       We will reconsider enabling this when we have host learning support
            /*
            HostId hostId = HostId.hostId(pkt.dstMac(), pkt.vlan());
            Host targetHost = hostService.getHost(hostId);
            if (targetHost != null) {
                log.debug("Forwarding the reply to the host");
                pkt.forward(targetHost.location());
            } else {
                // We don't have to flood towards spine facing ports.
                if (pkt.vlan().equals(SegmentRoutingManager.INTERNAL_VLAN)) {
                    return;
                }
                log.debug("Flooding the reply to the subnet");
                flood(pkt);
            }
            */
        }
    }

    /**
     * Utility to verify if the ND are for the gateway.
     *
     * @param pkt the ndp packet
     * @return true if the ndp is for the gateway. False otherwise
     */
    private boolean isNdpForGateway(NeighbourMessageContext pkt) {
        DeviceId deviceId = pkt.inPort().deviceId();
        Set<IpAddress> gatewayIpAddresses = null;

        try {
            if (pkt.target().equals(config.getRouterIpv6(deviceId))) {
                return true;
            }
            gatewayIpAddresses = config.getPortIPs(deviceId);
        } catch (DeviceConfigNotFoundException e) {
            log.warn(e.getMessage() + " Aborting check for router IP in processing ndp");
            return false;
        }
        return gatewayIpAddresses != null && gatewayIpAddresses.stream()
                .filter(IpAddress::isIp6)
                .anyMatch(gatewayIp -> gatewayIp.equals(pkt.target()) ||
                        Arrays.equals(IPv6.getSolicitNodeAddress(gatewayIp.toOctets()),
                                pkt.target().toOctets()));
    }

    /**
     * Sends a NDP request for the target IP address to all ports except in-port.
     *
     * @param deviceId Switch device ID
     * @param targetAddress target IP address for ARP
     * @param inPort in-port
     */
    public void sendNdpRequest(DeviceId deviceId, IpAddress targetAddress, ConnectPoint inPort) {
        byte[] senderMacAddress = new byte[MacAddress.MAC_ADDRESS_LENGTH];
        byte[] senderIpAddress = new byte[Ip6Address.BYTE_LENGTH];
        // Retrieves device info.
        if (!getSenderInfo(senderMacAddress, senderIpAddress, deviceId, targetAddress)) {
            log.warn("Aborting sendNdpRequest, we cannot get all the information needed");
            return;
        }
        // We have to compute the dst mac address and dst ip address.
        byte[] dstIp = IPv6.getSolicitNodeAddress(targetAddress.toOctets());
        byte[] dstMac = IPv6.getMCastMacAddress(dstIp);
        // Creates the request.
        Ethernet ndpRequest = NeighborSolicitation.buildNdpSolicit(
                targetAddress.getIp6Address(),
                Ip6Address.valueOf(senderIpAddress),
                Ip6Address.valueOf(dstIp),
                MacAddress.valueOf(senderMacAddress),
                MacAddress.valueOf(dstMac),
                VlanId.NONE
        );
        flood(ndpRequest, inPort, targetAddress);
    }

    /**
     * Returns link-local IP of given connect point.
     *
     * @param cp connect point
     * @return optional link-local IP
     */
    private Optional<Ip6Address> getLinkLocalIp(ConnectPoint cp) {
        return srManager.interfaceService.getInterfacesByPort(cp)
                .stream()
                .map(Interface::mac)
                .map(MacAddress::toBytes)
                .map(IPv6::getLinkLocalAddress)
                .map(Ip6Address::valueOf)
                .findFirst();
    }
}
