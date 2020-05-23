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
package org.onosproject.rest.resources;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.onosproject.net.Host;
import org.onosproject.net.HostId;
import org.onosproject.net.SparseAnnotations;
import org.onosproject.net.host.DefaultHostDescription;
import org.onosproject.net.host.HostAdminService;
import org.onosproject.net.host.HostProvider;
import org.onosproject.net.host.HostProviderRegistry;
import org.onosproject.net.host.HostProviderService;
import org.onosproject.net.host.HostService;
import org.onosproject.net.provider.ProviderId;
import org.onosproject.rest.AbstractWebResource;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;

import static org.onlab.util.Tools.nullIsNotFound;
import static org.onlab.util.Tools.readTreeFromStream;
import static org.onosproject.net.HostId.hostId;

/**
 * Manage inventory of end-station hosts.
 */
@Path("hosts")
public class HostsWebResource extends AbstractWebResource {

    @Context
    private UriInfo uriInfo;
    private static final String HOST_NOT_FOUND = "Host is not found";

    /**
     * Get all end-station hosts.
     * Returns array of all known end-station hosts.
     *
     * @return 200 OK with array of all known end-station hosts.
     * @onos.rsModel Hosts
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getHosts() {
        final Iterable<Host> hosts = get(HostService.class).getHosts();
        final ObjectNode root = encodeArray(Host.class, "hosts", hosts);
        return ok(root).build();
    }

    /**
     * Get details of end-station host.
     * Returns detailed properties of the specified end-station host.
     *
     * @param id host identifier
     * @return 200 OK with detailed properties of the specified end-station host
     * @onos.rsModel Host
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{id}")
    public Response getHostById(@PathParam("id") String id) {
        final Host host = nullIsNotFound(get(HostService.class).getHost(hostId(id)),
                                         HOST_NOT_FOUND);
        final ObjectNode root = codec(Host.class).encode(host, this);
        return ok(root).build();
    }

    /**
     * Get details of end-station host with MAC/VLAN.
     * Returns detailed properties of the specified end-station host.
     *
     * @param mac  host MAC address
     * @param vlan host VLAN identifier
     * @return 200 OK with detailed properties of the specified end-station host
     * @onos.rsModel Host
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @Path("{mac}/{vlan}")
    public Response getHostByMacAndVlan(@PathParam("mac") String mac,
                                        @PathParam("vlan") String vlan) {
        final Host host = nullIsNotFound(get(HostService.class).getHost(hostId(mac + "/" + vlan)),
                                         HOST_NOT_FOUND);
        final ObjectNode root = codec(Host.class).encode(host, this);
        return ok(root).build();
    }

    /**
     * Creates a new host based on JSON input and adds it to the current
     * host inventory.
     *
     * @param stream input JSON
     * @return status of the request - CREATED if the JSON is correct,
     * BAD_REQUEST if the JSON is invalid
     * @onos.rsModel HostPut
     */
    @POST
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    public Response createAndAddHost(InputStream stream) {
        URI location;
        HostProviderRegistry hostProviderRegistry = get(HostProviderRegistry.class);
        InternalHostProvider hostProvider = new InternalHostProvider();
        try {
            // Parse the input stream
            ObjectNode root = readTreeFromStream(mapper(), stream);

            HostProviderService hostProviderService = hostProviderRegistry.register(hostProvider);
            hostProvider.setHostProviderService(hostProviderService);
            HostId hostId = hostProvider.parseHost(root);

            UriBuilder locationBuilder = uriInfo.getBaseUriBuilder()
                    .path("hosts")
                    .path(hostId.mac().toString())
                    .path(hostId.vlanId().toString());
            location = locationBuilder.build();
        } catch (IOException ex) {
            throw new IllegalArgumentException(ex);
        } finally {
            hostProviderRegistry.unregister(hostProvider);
        }

        return Response
                .created(location)
                .build();
    }

    /**
     * Removes infrastructure device.
     * Administratively deletes the specified device from the inventory of
     * known devices.
     *
     * @param mac  host MAC address
     * @param vlan host VLAN identifier
     * @return 204 OK
     * @onos.rsModel Host
     */
    @DELETE
    @Path("{mac}/{vlan}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response removeHost(@PathParam("mac") String mac,
                               @PathParam("vlan") String vlan) {
        get(HostAdminService.class).removeHost(hostId(mac + "/" + vlan));
        return Response.noContent().build();
    }

    /**
     * Internal host provider that provides host events.
     */
    private final class InternalHostProvider implements HostProvider {
        private final ProviderId providerId =
                new ProviderId("host", "org.onosproject.rest");
        private HostProviderService hostProviderService;

        /**
         * Prevents from instantiation.
         */
        private InternalHostProvider() {
        }

        public void triggerProbe(Host host) {
            // Not implemented since there is no need to check for hosts on network
        }

        public void setHostProviderService(HostProviderService service) {
            this.hostProviderService = service;
        }

        /*
         * Return the ProviderId of "this"
         */
        public ProviderId id() {
            return providerId;
        }

        /**
         * Creates and adds new host based on given data and returns its host ID.
         *
         * @param node JsonNode containing host information
         * @return host ID of new host created
         */
        private HostId parseHost(JsonNode node) {
            Host host = codec(Host.class).decode((ObjectNode) node, HostsWebResource.this);

            HostId hostId = host.id();
            DefaultHostDescription desc = new DefaultHostDescription(
                    host.mac(), host.vlan(), host.locations(), host.ipAddresses(), host.innerVlan(),
                    host.tpid(), host.configured(), (SparseAnnotations) host.annotations());
            hostProviderService.hostDetected(hostId, desc, false);

            return hostId;
        }
    }
}

