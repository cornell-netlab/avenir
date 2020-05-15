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
package org.onosproject.net.device.impl;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Multimap;
import org.onlab.util.KryoNamespace;
import org.onlab.util.Tools;
import org.onosproject.cluster.ClusterService;
import org.onosproject.cluster.NodeId;
import org.onosproject.mastership.MastershipEvent;
import org.onosproject.mastership.MastershipListener;
import org.onosproject.mastership.MastershipService;
import org.onosproject.mastership.MastershipTerm;
import org.onosproject.mastership.MastershipTermService;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.Device;
import org.onosproject.net.Device.Type;
import org.onosproject.net.DeviceId;
import org.onosproject.net.MastershipRole;
import org.onosproject.net.Port;
import org.onosproject.net.PortNumber;
import org.onosproject.net.config.Config;
import org.onosproject.net.config.NetworkConfigEvent;
import org.onosproject.net.config.NetworkConfigListener;
import org.onosproject.net.config.NetworkConfigService;
import org.onosproject.net.config.PortConfigOperator;
import org.onosproject.net.config.PortConfigOperatorRegistry;
import org.onosproject.net.config.basics.BasicDeviceConfig;
import org.onosproject.net.config.basics.DeviceAnnotationConfig;
import org.onosproject.net.config.basics.PortAnnotationConfig;
import org.onosproject.net.config.basics.PortDescriptionsConfig;
import org.onosproject.net.device.DefaultPortDescription;
import org.onosproject.net.device.DeviceAdminService;
import org.onosproject.net.device.DeviceDescription;
import org.onosproject.net.device.DeviceEvent;
import org.onosproject.net.device.DeviceListener;
import org.onosproject.net.device.DeviceProvider;
import org.onosproject.net.device.DeviceProviderRegistry;
import org.onosproject.net.device.DeviceProviderService;
import org.onosproject.net.device.DeviceService;
import org.onosproject.net.device.DeviceStore;
import org.onosproject.net.device.DeviceStoreDelegate;
import org.onosproject.net.device.PortDescription;
import org.onosproject.net.device.PortStatistics;
import org.onosproject.net.provider.AbstractListenerProviderRegistry;
import org.onosproject.net.provider.AbstractProviderService;
import org.onosproject.net.provider.Provider;
import org.onosproject.net.provider.ProviderId;
import org.onosproject.store.cluster.messaging.ClusterCommunicationService;
import org.onosproject.store.cluster.messaging.MessageSubject;
import org.onosproject.store.serializers.KryoNamespaces;
import org.onosproject.store.service.Serializer;
import org.onosproject.upgrade.UpgradeService;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.slf4j.Logger;

import java.time.Instant;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;
import static com.google.common.base.Preconditions.checkState;
import static com.google.common.collect.Multimaps.newListMultimap;
import static com.google.common.collect.Multimaps.synchronizedListMultimap;
import static java.util.concurrent.Executors.newSingleThreadExecutor;
import static java.util.concurrent.Executors.newSingleThreadScheduledExecutor;
import static org.onlab.util.Tools.groupedThreads;
import static org.onosproject.net.MastershipRole.MASTER;
import static org.onosproject.net.MastershipRole.NONE;
import static org.onosproject.net.MastershipRole.STANDBY;
import static org.onosproject.security.AppGuard.checkPermission;
import static org.onosproject.security.AppPermission.Type.DEVICE_READ;
import static org.slf4j.LoggerFactory.getLogger;

/**
 * Provides implementation of the device SB &amp; NB APIs.
 */
@Component(immediate = true,
           service = {DeviceService.class, DeviceAdminService.class,
                      DeviceProviderRegistry.class, PortConfigOperatorRegistry.class })
public class DeviceManager
        extends AbstractListenerProviderRegistry<DeviceEvent, DeviceListener, DeviceProvider, DeviceProviderService>
        implements DeviceService, DeviceAdminService, DeviceProviderRegistry, PortConfigOperatorRegistry {

    private static final String DEVICE_ID_NULL = "Device ID cannot be null";
    private static final String PORT_NUMBER_NULL = "Port number cannot be null";
    private static final String DEVICE_DESCRIPTION_NULL = "Device description cannot be null";
    private static final String PORT_DESCRIPTION_NULL = "Port description cannot be null";
    private static final String PORT_DESC_LIST_NULL = "Port description list cannot be null";
    private static final String EVENT_NON_MASTER = "Non-master node cannot handle this event";

    private final Logger log = getLogger(getClass());

    private final DeviceStoreDelegate delegate = new InternalStoreDelegate();

    private final MastershipListener mastershipListener = new InternalMastershipListener();
    private NodeId localNodeId;

    private ScheduledExecutorService backgroundService;

    private final NetworkConfigListener networkConfigListener = new InternalNetworkConfigListener();

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected DeviceStore store;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected ClusterService clusterService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected MastershipService mastershipService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected MastershipTermService termService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected UpgradeService upgradeService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected NetworkConfigService networkConfigService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected ClusterCommunicationService communicationService;

    private ExecutorService portReqeustExecutor;
    /**
     * List of all registered PortConfigOperator.
     */
    private final List<PortConfigOperator> portOps = new CopyOnWriteArrayList<>();

    /**
     * Index to look up PortConfigOperator from Config each PortConfigOperator uses.
     */
    private final Multimap<Class<? extends Config<ConnectPoint>>, PortConfigOperator> portOpsIndex
            = synchronizedListMultimap(
            newListMultimap(new ConcurrentHashMap<>(), CopyOnWriteArrayList::new));

    // not part of portOps. must be executed at the end
    private PortAnnotationOperator portAnnotationOp;
    private DeviceAnnotationOperator deviceAnnotationOp;

    private static final MessageSubject PORT_UPDOWN_SUBJECT =
            new MessageSubject("port-updown-req");

    private static final Serializer SERIALIZER = Serializer.using(
            KryoNamespace.newBuilder()
                    .register(KryoNamespaces.API)
                    .register(InternalPortUpDownEvent.class)
                    .nextId(KryoNamespaces.BEGIN_USER_CUSTOM_ID)
                    .build("DeviceManager"));

    /**
     * Local storage for connectivity status of devices.
     */
    private class LocalStatus {
        boolean connected;
        Instant dateTime;

        public LocalStatus(boolean b, Instant now) {
            connected = b;
            dateTime = now;
        }
    }

    private final Map<DeviceId, LocalStatus> deviceLocalStatus =
            Maps.newConcurrentMap();

    @Activate
    public void activate() {
        portAnnotationOp = new PortAnnotationOperator(networkConfigService);
        deviceAnnotationOp = new DeviceAnnotationOperator(networkConfigService);
        portOpsIndex.put(PortAnnotationConfig.class, portAnnotationOp);

        backgroundService = newSingleThreadScheduledExecutor(
                groupedThreads("onos/device", "manager-background", log));
        localNodeId = clusterService.getLocalNode().id();

        store.setDelegate(delegate);
        eventDispatcher.addSink(DeviceEvent.class, listenerRegistry);
        mastershipService.addListener(mastershipListener);
        networkConfigService.addListener(networkConfigListener);

        backgroundService.scheduleWithFixedDelay(() -> {
            try {
                mastershipCheck();
            } catch (Exception e) {
                log.error("Exception thrown during integrity check", e);
            }
        }, 1, 1, TimeUnit.MINUTES);

        portReqeustExecutor = newSingleThreadExecutor();

        communicationService.<InternalPortUpDownEvent>addSubscriber(
                PORT_UPDOWN_SUBJECT,
                SERIALIZER::decode,
                this::handlePortRequest,
                portReqeustExecutor);

        log.info("Started");
    }

    @Deactivate
    public void deactivate() {
        backgroundService.shutdown();
        networkConfigService.removeListener(networkConfigListener);
        store.unsetDelegate(delegate);
        mastershipService.removeListener(mastershipListener);
        eventDispatcher.removeSink(DeviceEvent.class);
        communicationService.removeSubscriber(PORT_UPDOWN_SUBJECT);
        portReqeustExecutor.shutdown();
        log.info("Stopped");
    }

    @Override
    public int getDeviceCount() {
        checkPermission(DEVICE_READ);
        return store.getDeviceCount();
    }

    @Override
    public int getAvailableDeviceCount() {
        checkPermission(DEVICE_READ);
        return store.getAvailableDeviceCount();
    }

    @Override
    public Iterable<Device> getDevices() {
        checkPermission(DEVICE_READ);
        return store.getDevices();
    }

    @Override
    public Iterable<Device> getAvailableDevices() {
        checkPermission(DEVICE_READ);
        return store.getAvailableDevices();
    }

    @Override
    public Device getDevice(DeviceId deviceId) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        return store.getDevice(deviceId);
    }

    @Override
    public MastershipRole getRole(DeviceId deviceId) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        return mastershipService.getLocalRole(deviceId);
    }

    @Override
    public List<Port> getPorts(DeviceId deviceId) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        return store.getPorts(deviceId);
    }

    @Override
    public List<PortStatistics> getPortStatistics(DeviceId deviceId) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        return store.getPortStatistics(deviceId);
    }

    @Override
    public List<PortStatistics> getPortDeltaStatistics(DeviceId deviceId) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        return store.getPortDeltaStatistics(deviceId);
    }

    @Override
    public PortStatistics getStatisticsForPort(DeviceId deviceId, PortNumber portNumber) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        checkNotNull(portNumber, PORT_NUMBER_NULL);
        return store.getStatisticsForPort(deviceId, portNumber);
    }

    @Override
    public PortStatistics getDeltaStatisticsForPort(DeviceId deviceId, PortNumber portNumber) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        checkNotNull(portNumber, PORT_NUMBER_NULL);
        return store.getDeltaStatisticsForPort(deviceId, portNumber);
    }

    @Override
    public Port getPort(DeviceId deviceId, PortNumber portNumber) {
        checkPermission(DEVICE_READ);
        checkNotNull(deviceId, DEVICE_ID_NULL);
        checkNotNull(portNumber, PORT_NUMBER_NULL);
        return store.getPort(deviceId, portNumber);
    }

    @Override
    public boolean isAvailable(DeviceId deviceId) {
        checkPermission(DEVICE_READ);

        checkNotNull(deviceId, DEVICE_ID_NULL);
        return store.isAvailable(deviceId);
    }

    @Override
    public String localStatus(DeviceId deviceId) {
        LocalStatus ls = deviceLocalStatus.get(deviceId);
        if (ls == null) {
            return "No Record";
        }
        String timeAgo = Tools.timeAgo(ls.dateTime.toEpochMilli());
        return (ls.connected) ? "connected " + timeAgo : "disconnected " + timeAgo;
    }

    private boolean isLocallyConnected(DeviceId deviceId) {
        LocalStatus ls = deviceLocalStatus.get(deviceId);
        if (ls == null) {
            return false;
        }
        return ls.connected;
    }

    @Override
    public long getLastUpdatedInstant(DeviceId deviceId) {
        LocalStatus ls = deviceLocalStatus.get(deviceId);
        if (ls == null) {
            return 0;
        }
        return ls.dateTime.toEpochMilli();
    }

    // Check a device for control channel connectivity
    // and changes local-status appropriately.
    private boolean isReachable(DeviceId deviceId) {
        if (deviceId == null) {
            return false;
        }
        DeviceProvider provider = getProvider(deviceId);
        if (provider != null) {
            boolean reachable = provider.isReachable(deviceId);
            if (reachable && !isLocallyConnected(deviceId)) {
                deviceLocalStatus.put(deviceId, new LocalStatus(true, Instant.now()));
            } else if (!reachable && isLocallyConnected(deviceId)) {
                deviceLocalStatus.put(deviceId, new LocalStatus(false, Instant.now()));
            }
            return reachable;
        } else {
            log.debug("Provider not found for {}", deviceId);
            return false;
        }
    }

    @Override
    public void removeDevice(DeviceId deviceId) {
        checkNotNull(deviceId, DEVICE_ID_NULL);
        DeviceEvent event = store.removeDevice(deviceId);
        if (event != null) {
            log.info("Device {} administratively removed", deviceId);
            post(event);
        }
    }

    @Override
    public void removeDevicePorts(DeviceId deviceId) {
        checkNotNull(deviceId, DEVICE_ID_NULL);
        if (isAvailable(deviceId)) {
            log.debug("Cannot remove ports of device {} while it is available.", deviceId);
            return;
        }

        List<PortDescription> portDescriptions = ImmutableList.of();
        List<DeviceEvent> events = store.updatePorts(getProvider(deviceId).id(),
                                                     deviceId, portDescriptions);
        if (events != null) {
            for (DeviceEvent event : events) {
                post(event);
            }
        }
    }

    private void handlePortRequest(InternalPortUpDownEvent event) {
        DeviceId deviceId = event.deviceId();
        checkNotNull(deviceId, DEVICE_ID_NULL);
        checkNotNull(deviceId, PORT_NUMBER_NULL);
        checkState(mastershipService.isLocalMaster(deviceId), EVENT_NON_MASTER);
        changePortStateAtMaster(event.deviceId(), event.portNumber(), event.isEnable());
    }

    private void changePortStateAtMaster(DeviceId deviceId, PortNumber portNumber,
                                       boolean enable) {
        DeviceProvider provider = getProvider(deviceId);
        if (provider != null) {
            log.info("Port {} on device {} being administratively brought {}",
                     portNumber, deviceId,
                     (enable) ? "UP" : "DOWN");
            provider.changePortState(deviceId, portNumber, enable);
        } else {
            log.warn("Provider not found for {}", deviceId);
        }
    }

    @Override
    public void changePortState(DeviceId deviceId, PortNumber portNumber,
                                boolean enable) {
        checkNotNull(deviceId, DEVICE_ID_NULL);
        checkNotNull(deviceId, PORT_NUMBER_NULL);
        NodeId masterId = mastershipService.getMasterFor(deviceId);

        if (masterId == null) {
            // No master found; device is offline
            log.info("No master found for port state change for {}", deviceId);
            return;
        }

        if (!masterId.equals(localNodeId)) {
            //Send the request to the master node for the device
            log.info("Device {} is managed by {}, forwarding the request to the MASTER",
                     deviceId, masterId);
            communicationService.unicast(
                    new InternalPortUpDownEvent(deviceId, portNumber, enable),
                    PORT_UPDOWN_SUBJECT,
                    SERIALIZER::encode,
                    masterId).whenComplete((r, error) -> {
                if (error != null) {
                    log.warn("Failed to send packet-updown-req to {}", masterId, error);
                }
            });
        } else {
            changePortStateAtMaster(deviceId, portNumber, enable);
        }
    }

    @Override
    protected DeviceProviderService createProviderService(
            DeviceProvider provider) {
        return new InternalDeviceProviderService(provider);
    }

    /**
     * Checks if all the reachable devices have a valid mastership role.
     */
    private void mastershipCheck() {
        log.debug("Checking mastership");
        for (Device device : getDevices()) {
            final DeviceId deviceId = device.id();
            MastershipRole myRole = mastershipService.getLocalRole(deviceId);
            log.trace("Checking device {}. Current role is {}", deviceId, myRole);
            if (!isReachable(deviceId)) {
                if (myRole != NONE) {
                    // can't be master if device is not reachable
                    try {
                        if (myRole == MASTER) {
                            log.info("Local Role {}, Marking unreachable device {} offline", MASTER, deviceId);
                            post(store.markOffline(deviceId));
                        }
                        //relinquish master role and ability to be backup.
                        mastershipService.relinquishMastership(deviceId).get();
                    } catch (InterruptedException e) {
                        log.warn("Interrupted while relinquishing role for {}", deviceId);
                        Thread.currentThread().interrupt();
                    } catch (ExecutionException e) {
                        log.error("Exception thrown while relinquishing role for {}", deviceId, e);
                    }
                } else {
                    // check if the device has master and is available to the store, if not, mark it offline
                    // only the nodes which has mastership role can mark any device offline.
                    // This condition should never be hit unless in a device removed phase for NONE mastership roles.
                    NodeId master = mastershipService.getMasterFor(deviceId);
                    if (master == null && isAvailable(deviceId)) {
                        CompletableFuture<MastershipRole> roleFuture = mastershipService.requestRoleFor(deviceId);
                        roleFuture.thenAccept(role -> {
                            MastershipTerm term = termService.getMastershipTerm(deviceId);
                            if (term != null && localNodeId.equals(term.master())) {
                                log.info("Marking unreachable device {} offline", deviceId);
                                post(store.markOffline(deviceId));
                            } else {
                                log.info("Failed marking {} offline. {}", deviceId, role);
                            }
                            mastershipService.relinquishMastership(deviceId);
                        });
                    }
                }
                continue;
            }

            // If this node is the master, ensure the device is marked online.
            if (myRole == MASTER && canMarkOnline(device)) {
                post(store.markOnline(deviceId));
            }

            if (myRole != NONE) {
                continue;
            }

            log.info("{} is reachable but did not have a valid role, reasserting", deviceId);

            // isReachable but was not MASTER or STANDBY, get a role and apply
            // Note: NONE triggers request to MastershipService
            reassertRole(deviceId, NONE);
        }
    }

    // Personalized device provider service issued to the supplied provider.
    private class InternalDeviceProviderService
            extends AbstractProviderService<DeviceProvider>
            implements DeviceProviderService {

        InternalDeviceProviderService(DeviceProvider provider) {
            super(provider);
        }

        /**
         * Apply role in reaction to provider event.
         *
         * @param deviceId device identifier
         * @param newRole  new role to apply to the device
         * @return true if the request was sent to provider
         */
        private boolean applyRole(DeviceId deviceId, MastershipRole newRole) {

            if (newRole.equals(MastershipRole.NONE)) {
                //no-op
                return true;
            }

            DeviceProvider provider = provider();
            if (provider == null) {
                log.warn("Provider for {} was not found. Cannot apply role {}",
                         deviceId, newRole);
                return false;
            }
            provider.roleChanged(deviceId, newRole);
            // not triggering probe when triggered by provider service event
            return true;
        }

        @Override
        public void deviceConnected(DeviceId deviceId,
                                    DeviceDescription deviceDescription) {
            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkNotNull(deviceDescription, DEVICE_DESCRIPTION_NULL);
            checkValidity();

            deviceLocalStatus.put(deviceId, new LocalStatus(true, Instant.now()));

            BasicDeviceConfig cfg = networkConfigService.getConfig(deviceId, BasicDeviceConfig.class);
            if (!isAllowed(cfg)) {
                log.warn("Device {} is not allowed", deviceId);
                return;
            }
            PortDescriptionsConfig portConfig = networkConfigService.getConfig(deviceId, PortDescriptionsConfig.class);
            // Generate updated description and establish my Role
            deviceDescription = BasicDeviceOperator.combine(cfg, deviceDescription);
            DeviceAnnotationConfig annoConfig = networkConfigService.getConfig(deviceId, DeviceAnnotationConfig.class);
            if (annoConfig != null) {
                deviceDescription = deviceAnnotationOp.combine(deviceId, deviceDescription, Optional.of(annoConfig));
            }

            MastershipRole role = mastershipService.requestRoleForSync(deviceId);
            log.info("Local role is {} for {}", role, deviceId);
            DeviceEvent event = store.createOrUpdateDevice(provider().id(), deviceId,
                    deviceDescription);
            applyRole(deviceId, role);

            if (portConfig != null) {
                //updating the ports if configration exists
                List<PortDescription> complete = store.getPortDescriptions(provider().id(), deviceId)
                        .collect(Collectors.toList());
                complete.addAll(portConfig.portDescriptions());
                List<PortDescription> portDescriptions = complete.stream()
                        .map(e -> applyAllPortOps(deviceId, e))
                        .collect(Collectors.toList());
                store.updatePorts(provider().id(), deviceId, portDescriptions);
            }

            if (deviceDescription.isDefaultAvailable()) {
                log.info("Device {} connected", deviceId);
            } else {
                log.info("Device {} registered", deviceId);
            }

            if (event != null) {
                log.trace("event: {} {}", event.type(), event);
                post(event);
            }
        }

        private PortDescription ensurePortEnabledState(PortDescription desc, boolean enabled) {
            if (desc.isEnabled() != enabled) {
                return DefaultPortDescription.builder(desc)
                        .isEnabled(enabled)
                        .build();
            }
            return desc;
        }

        @Override
        public void deviceDisconnected(DeviceId deviceId) {
            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkValidity();
            deviceLocalStatus.put(deviceId, new LocalStatus(false, Instant.now()));
            log.info("Device {} disconnected from this node", deviceId);

            List<PortDescription> descs = store.getPortDescriptions(provider().id(), deviceId)
                    .map(desc -> ensurePortEnabledState(desc, false))
                    .collect(Collectors.toList());

            store.updatePorts(this.provider().id(), deviceId, descs);
            try {
                if (mastershipService.isLocalMaster(deviceId)) {
                    post(store.markOffline(deviceId));
                }
            } catch (IllegalStateException e) {
                log.warn("Failed to mark {} offline", deviceId);
                // only the MASTER should be marking off-line in normal cases,
                // but if I was the last STANDBY connection, etc. and no one else
                // was there to mark the device offline, this instance may need to
                // temporarily request for Master Role and mark offline.

                //there are times when this node will correctly have mastership, BUT
                //that isn't reflected in the ClockManager before the device disconnects.
                //we want to let go of the device anyways, so make sure this happens.

                // FIXME: Store semantics leaking out as IllegalStateException.
                //  Consider revising store API to handle this scenario.
                CompletableFuture<MastershipRole> roleFuture = mastershipService.requestRoleFor(deviceId);
                roleFuture.whenComplete((role, error) -> {
                    MastershipTerm term = termService.getMastershipTerm(deviceId);
                    // TODO: Move this type of check inside device clock manager, etc.
                    if (term != null && localNodeId.equals(term.master())) {
                        log.info("Retry marking {} offline", deviceId);
                        post(store.markOffline(deviceId));
                    } else {
                        log.info("Failed again marking {} offline. {}", deviceId, role);
                    }
                });
            } finally {
                try {
                    //relinquish master role and ability to be backup.
                    mastershipService.relinquishMastership(deviceId).get();
                } catch (InterruptedException e) {
                    log.warn("Interrupted while reliquishing role for {}", deviceId);
                    Thread.currentThread().interrupt();
                } catch (ExecutionException e) {
                    log.error("Exception thrown while relinquishing role for {}", deviceId, e);
                }
            }
        }

        @Override
        public void updatePorts(DeviceId deviceId,
                                List<PortDescription> portDescriptions) {
            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkNotNull(portDescriptions, PORT_DESC_LIST_NULL);
            checkValidity();
            if (!mastershipService.isLocalMaster(deviceId)) {
                // Never been a master for this device
                // any update will be ignored.
                log.trace("Ignoring {} port updates on standby node. {}", deviceId, portDescriptions);
                return;
            }
            PortDescriptionsConfig portConfig = networkConfigService.getConfig(deviceId, PortDescriptionsConfig.class);
            if (portConfig != null) {
                // Updating the ports if configuration exists (on new lists as
                // the passed one might be immutable)
                portDescriptions = Lists.newArrayList(portDescriptions);
                portDescriptions.addAll(portConfig.portDescriptions());
            }
            portDescriptions = portDescriptions.stream()
                    .map(e -> applyAllPortOps(deviceId, e))
                    .collect(Collectors.toList());
            List<DeviceEvent> events = store.updatePorts(this.provider().id(),
                                                         deviceId, portDescriptions);
            if (events != null) {
                for (DeviceEvent event : events) {
                    post(event);
                }
            }
        }

        @Override
        public void portStatusChanged(DeviceId deviceId,
                                      PortDescription portDescription) {
            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkNotNull(portDescription, PORT_DESCRIPTION_NULL);
            checkValidity();

            if (!mastershipService.isLocalMaster(deviceId)) {
                // Never been a master for this device
                // any update will be ignored.
                log.trace("Ignoring {} port update on standby node. {}", deviceId,
                          portDescription);
                return;
            }
            Device device = getDevice(deviceId);
            if (device == null) {
                log.trace("Device not found: {}", deviceId);
                return;
            }
            if ((Type.ROADM.equals(device.type())) || (Type.OTN.equals(device.type())) ||
                    (Type.OLS.equals(device.type())) || (Type.TERMINAL_DEVICE.equals(device.type()))) {
                // FIXME This is ignoring all other info in portDescription given as input??
                PortDescription storedPortDesc = store.getPortDescription(provider().id(),
                                                                          deviceId,
                                                                          portDescription.portNumber());
                portDescription = ensurePortEnabledState(storedPortDesc,
                                                         portDescription.isEnabled());
            }

            portDescription = applyAllPortOps(deviceId, portDescription);
            final DeviceEvent event = store.updatePortStatus(this.provider().id(),
                                                             deviceId,
                                                             portDescription);
            if (event != null) {
                log.info("Device {} port {} status changed (enabled={})",
                         deviceId, event.port().number(), portDescription.isEnabled());
                post(event);
            }
        }

        @Override
        public void deletePort(DeviceId deviceId, PortDescription basePortDescription) {

            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkNotNull(basePortDescription, PORT_DESCRIPTION_NULL);
            checkValidity();

            if (!mastershipService.isLocalMaster(deviceId)) {
                // Never been a master for this device
                // any update will be ignored.
                log.trace("Ignoring {} port update on standby node. {}", deviceId,
                          basePortDescription);
                return;
            }

            Device device = getDevice(deviceId);
            if (device == null) {
                log.trace("Device not found: {}", deviceId);
            }

            PortDescription newPortDescription = DefaultPortDescription.builder(basePortDescription)
                .isRemoved(true)
                .build();

            final DeviceEvent event = store.updatePortStatus(this.provider().id(),
                                                             deviceId,
                                                             newPortDescription);
            if (event != null) {
                log.info("Device {} port {} status changed", deviceId, event.port().number());
                post(event);
            }
        }

        @Override
        public void receivedRoleReply(DeviceId deviceId, MastershipRole requested,
                                      MastershipRole response) {
            // Several things can happen here:
            // 1. request and response match
            // 2. request and response don't match
            // 3. MastershipRole and requested match (and 1 or 2 are true)
            // 4. MastershipRole and requested don't match (and 1 or 2 are true)
            //
            // 2, 4, and 3 with case 2 are failure modes.

            // FIXME: implement response to this notification

            log.debug("got reply to a role request for {}: asked for {}, and got {}",
                      deviceId, requested, response);

            if (requested == null && response == null) {
                // something was off with DeviceProvider, maybe check channel too?
                log.warn("Failed to assert role onto Device {}", deviceId);
                mastershipService.relinquishMastership(deviceId);
                return;
            }

            final MastershipRole expected = mastershipService.getLocalRole(deviceId);

            if (requested == null) {
                // Provider is not able to reconcile role responses with
                // requests. We assume what was requested is what we expect.
                // This will work only if mastership doesn't change too often,
                // and devices are left enough time to provide responses before
                // a different role is requested.
                requested = expected;
            }

            if (Objects.equals(requested, response)) {
                if (Objects.equals(requested, expected)) {
                    return;
                } else {
                    log.warn("Role mismatch on {}. Set to {}, but store demands {}",
                             deviceId, response, expected);
                    // roleManager got the device to comply, but doesn't agree with
                    // the store; use the store's view, then try to reassert.
                    backgroundService.execute(() -> reassertRole(deviceId, expected));
                    return;
                }
            } else {
                // we didn't get back what we asked for. Reelect someone else.
                log.warn("Failed to assert role onto device {}. requested={}, response={}",
                         deviceId, requested, response);
                if (requested == MastershipRole.MASTER) {
                    mastershipService.relinquishMastership(deviceId);
                    // TODO: Shouldn't we be triggering event?
                    //final Device device = getDevice(deviceId);
                    //post(new DeviceEvent(DEVICE_MASTERSHIP_CHANGED, device));
                }
            }
        }

        @Override
        public void updatePortStatistics(DeviceId deviceId, Collection<PortStatistics> portStatistics) {
            checkNotNull(deviceId, DEVICE_ID_NULL);
            checkNotNull(portStatistics, "Port statistics list cannot be null");
            checkValidity();

            DeviceEvent event = store.updatePortStatistics(this.provider().id(),
                                                           deviceId, portStatistics);
            post(event);
        }
    }

    // by default allowed, otherwise check flag
    private boolean isAllowed(BasicDeviceConfig cfg) {
        return (cfg == null || cfg.isAllowed());
    }

    private boolean canMarkOnline(Device device) {
        DeviceProvider provider = getProvider(device.id());
        if (provider == null) {
            log.warn("Provider for {} was not found. Cannot evaluate availability", device.id());
            return false;
        }
        return provider.isAvailable(device.id());
    }

    // Applies the specified role to the device; ignores NONE

    /**
     * Apply role to device and send probe if MASTER.
     *
     * @param deviceId device identifier
     * @param newRole  new role to apply to the device
     * @return true if the request was sent to provider
     */
    private boolean applyRoleAndProbe(DeviceId deviceId, MastershipRole newRole) {
        if (newRole.equals(MastershipRole.NONE)) {
            //no-op
            return true;
        }

        DeviceProvider provider = getProvider(deviceId);
        if (provider == null) {
            log.warn("Provider for {} was not found. Cannot apply role {}", deviceId, newRole);
            return false;
        }
        provider.roleChanged(deviceId, newRole);

        if (newRole.equals(MastershipRole.MASTER)) {
            log.debug("sent TriggerProbe({})", deviceId);
            // only trigger event when request was sent to provider
            provider.triggerProbe(deviceId);
        }
        return true;
    }

    /**
     * Reassert role for specified device connected to this node.
     *
     * @param did      device identifier
     * @param nextRole role to apply. If NONE is specified,
     *                 it will ask mastership service for a role and apply it.
     */
    private void reassertRole(final DeviceId did,
                              final MastershipRole nextRole) {

        MastershipRole myNextRole = nextRole;
        if (myNextRole == NONE && upgradeService.isLocalActive()) {
            try {
                mastershipService.requestRoleFor(did).get();
                MastershipTerm term = termService.getMastershipTerm(did);
                if (term != null && localNodeId.equals(term.master())) {
                    myNextRole = MASTER;
                } else {
                    myNextRole = STANDBY;
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                log.error("Interrupted waiting for Mastership", e);
            } catch (ExecutionException e) {
                log.error("Encountered an error waiting for Mastership", e);
            }
        }

        switch (myNextRole) {
            case MASTER:
                final Device device = getDevice(did);
                if (device != null && !isAvailable(did) && canMarkOnline(device)) {
                    post(store.markOnline(did));
                }
                // TODO: should apply role only if there is mismatch
                log.debug("Applying role {} to {}", myNextRole, did);
                if (!applyRoleAndProbe(did, MASTER)) {
                    log.warn("Unsuccessful applying role {} to {}", myNextRole, did);
                    // immediately failed to apply role
                    mastershipService.relinquishMastership(did);
                    // FIXME disconnect?
                }
                break;
            case STANDBY:
                log.debug("Applying role {} to {}", myNextRole, did);
                if (!applyRoleAndProbe(did, STANDBY)) {
                    log.warn("Unsuccessful applying role {} to {}", myNextRole, did);
                    // immediately failed to apply role
                    mastershipService.relinquishMastership(did);
                    // FIXME disconnect?
                }
                break;
            case NONE:
                break;
            default:
                // should never reach here
                log.error("You didn't see anything. I did not exist.");
                break;
        }
    }

    private void handleMastershipEvent(MastershipEvent event) {
        if (event.type() == MastershipEvent.Type.BACKUPS_CHANGED) {
            // Don't care if backup list changed.
            return;
        }
        final DeviceId did = event.subject();

        // myRole suggested by MastershipService
        MastershipRole myNextRole;
        if (event.type() == MastershipEvent.Type.SUSPENDED) {
            myNextRole = NONE; // FIXME STANDBY OR NONE?
        } else if (localNodeId.equals(event.roleInfo().master())) {
            // confirm latest info
            MastershipTerm term = termService.getMastershipTerm(did);
            final boolean iHaveControl = term != null && localNodeId.equals(term.master());
            if (iHaveControl) {
                myNextRole = MASTER;
            } else {
                myNextRole = STANDBY;
            }
        } else if (event.roleInfo().backups().contains(localNodeId)) {
            myNextRole = STANDBY;
        } else {
            myNextRole = NONE;
        }

        final boolean isReachable = isReachable(did);
        if (!isReachable) {
            // device is not connected to this node
            if (mastershipService.getLocalRole(did) == NONE) {
                log.debug("Node was instructed to be {} role for {}, "
                                  + "but this node cannot reach the device "
                                  + "and role is already None. Ignoring request.",
                          myNextRole, did);
            } else if (myNextRole != NONE) {
                log.warn("Node was instructed to be {} role for {}, "
                                 + "but this node cannot reach the device.  "
                                 + "Relinquishing role.  ",
                         myNextRole, did);
                mastershipService.relinquishMastership(did);
            }
            return;
        }

        // device is connected to this node:
        if (store.getDevice(did) != null) {
            reassertRole(did, myNextRole);
        } else {
            log.debug("Device is not yet/no longer in the store: {}", did);
        }
    }

    // Intercepts mastership events
    private class InternalMastershipListener implements MastershipListener {

        @Override
        public void event(MastershipEvent event) {
            backgroundService.execute(() -> {
                try {
                    handleMastershipEvent(event);
                } catch (Exception e) {
                    log.warn("Failed to handle {}", event, e);
                }
            });
        }
    }

    // Store delegate to re-post events emitted from the store.
    private class InternalStoreDelegate implements DeviceStoreDelegate {
        @Override
        public void notify(DeviceEvent event) {
            post(event);
            if (event.type().equals(DeviceEvent.Type.DEVICE_REMOVED)) {
                // When device is administratively removed, force disconnect.
                DeviceId deviceId = event.subject().id();
                deviceLocalStatus.remove(deviceId);

                DeviceProvider provider = getProvider(deviceId);
                if (provider != null) {
                    log.info("Triggering disconnect for device {}", deviceId);
                    try {
                        provider.triggerDisconnect(deviceId);
                    } catch (UnsupportedOperationException e) {
                        log.warn("Unable to trigger disconnect due to {}", e.getMessage());
                    }
                }
            }
        }
    }

    @Override
    public Iterable<Device> getDevices(Type type) {
        checkPermission(DEVICE_READ);
        Set<Device> results = new HashSet<>();
        Iterable<Device> devices = store.getDevices();
        if (devices != null) {
            devices.forEach(d -> {
                if (type.equals(d.type())) {
                    results.add(d);
                }
            });
        }
        return results;
    }

    @Override
    public Iterable<Device> getAvailableDevices(Type type) {
        checkPermission(DEVICE_READ);
        Set<Device> results = new HashSet<>();
        Iterable<Device> availableDevices = store.getAvailableDevices();
        if (availableDevices != null) {
            availableDevices.forEach(d -> {
                if (type.equals(d.type())) {
                    results.add(d);
                }
            });
        }
        return results;
    }

    private class InternalNetworkConfigListener implements NetworkConfigListener {
        private DeviceId extractDeviceId(NetworkConfigEvent event) {
            DeviceId deviceId = null;
            if (event.configClass().equals(PortAnnotationConfig.class)) {
                if (event.subject().getClass() == ConnectPoint.class) {
                    deviceId = ((ConnectPoint) event.subject()).deviceId();
                }
            } else if (event.subject().getClass() == DeviceId.class) {
                deviceId = (DeviceId) event.subject();
            }
            return deviceId;
        }

        @Override
        public boolean isRelevant(NetworkConfigEvent event) {
            DeviceId deviceId = extractDeviceId(event);

            return (event.type() == NetworkConfigEvent.Type.CONFIG_ADDED
                    || event.type() == NetworkConfigEvent.Type.CONFIG_UPDATED
                    || event.type() == NetworkConfigEvent.Type.CONFIG_REMOVED)
                    && (event.configClass().equals(BasicDeviceConfig.class)
                    || portOpsIndex.containsKey(event.configClass())
                    || event.configClass().equals(PortDescriptionsConfig.class)
                    || event.configClass().equals(DeviceAnnotationConfig.class))
                    && deviceId != null && mastershipService.isLocalMaster(deviceId);
        }

        @Override
        public void event(NetworkConfigEvent event) {
            DeviceEvent de = null;
            if (event.configClass().equals(BasicDeviceConfig.class)) {
                log.debug("Detected device network config event {}", event.type());
                DeviceId did = (DeviceId) event.subject();
                DeviceProvider dp = getProvider(did);
                BasicDeviceConfig cfg =
                        networkConfigService.getConfig(did, BasicDeviceConfig.class);

                if (!isAllowed(cfg)) {
                    kickOutBadDevice(did);
                } else {
                    Device dev = getDevice(did);
                    DeviceDescription desc =
                            (dev == null) ? null : BasicDeviceOperator.descriptionOf(dev);
                    desc = BasicDeviceOperator.combine(cfg, desc);
                    if (desc != null && dp != null) {
                        de = store.createOrUpdateDevice(dp.id(), did, desc);
                    }
                }
            } else if (event.configClass().equals(PortDescriptionsConfig.class)) {
                DeviceId did = (DeviceId) event.subject();
                DeviceProvider dp = getProvider(did);
                if (!event.config().isPresent() ||
                    getDevice(did) == null || dp == null) {
                    // sanity check failed, ignore
                    return;
                }
                PortDescriptionsConfig portConfig = (PortDescriptionsConfig) event.config().get();
                    //updating the ports if configuration exists
                    List<PortDescription> complete = store.getPortDescriptions(dp.id(), did)
                            .collect(Collectors.toList());
                    complete.addAll(portConfig.portDescriptions());
                    store.updatePorts(dp.id(), did, complete);
            } else if (event.configClass().equals(DeviceAnnotationConfig.class)) {
                DeviceId did = (DeviceId) event.subject();
                DeviceProvider dp = getProvider(did);
                Device dev = getDevice(did);
                DeviceDescription desc =
                        (dev == null) ? null : BasicDeviceOperator.descriptionOf(dev);
                Optional<Config> prevConfig = event.prevConfig();
                desc = deviceAnnotationOp.combine(did, desc, prevConfig);
                if (desc != null && dp != null) {
                    de = store.createOrUpdateDevice(dp.id(), did, desc);
                }
            } else if (portOpsIndex.containsKey(event.configClass())) {
                ConnectPoint cpt = (ConnectPoint) event.subject();
                DeviceId did = cpt.deviceId();
                DeviceProvider dp = getProvider(did);

                // Note: assuming PortOperator can modify existing port,
                //       but cannot add new port purely from Config.
                de = Optional.ofNullable(dp)
                        .map(provider -> store.getPortDescription(provider.id(), did, cpt.port()))
                        .map(desc -> applyAllPortOps(cpt, desc, event.prevConfig()))
                        .map(desc -> store.updatePortStatus(dp.id(), did, desc))
                        .orElse(null);
            }

            if (de != null) {
                post(de);
            }
        }

        // removes the specified device if it exists
        private void kickOutBadDevice(DeviceId deviceId) {
            Device badDevice = getDevice(deviceId);
            if (badDevice != null) {
                removeDevice(deviceId);
            }
        }
    }

    @Override
    @SafeVarargs
    public final void registerPortConfigOperator(PortConfigOperator portOp,
                                                 Class<? extends Config<ConnectPoint>>... configs) {
        checkNotNull(portOp);

        portOp.bindService(networkConfigService);

        // update both portOpsIndex and portOps
        synchronized (portOpsIndex) {
            for (Class<? extends Config<ConnectPoint>> config : configs) {
                portOpsIndex.put(config, portOp);
            }

            portOps.add(portOp);
        }

        // TODO: Should we be applying to all existing Ports?
        Tools.stream(store.getAvailableDevices())
                .map(Device::id)
                .filter(mastershipService::isLocalMaster)
                // for each locally managed Device, update all port descriptions
                .map(did -> {
                    ProviderId pid = Optional.ofNullable(getProvider(did))
                            .map(Provider::id)
                            .orElse(null);
                    if (pid == null) {
                        log.warn("Provider not found for {}", did);
                        return ImmutableList.<DeviceEvent>of();
                    }
                    List<PortDescription> pds
                            = store.getPortDescriptions(pid, did)
                            .map(pdesc -> applyAllPortOps(did, pdesc))
                            .collect(Collectors.toList());
                    return store.updatePorts(pid, did, pds);
                })
                // ..and port port update event if necessary
                .forEach(evts -> evts.forEach(this::post));
    }

    @Override
    public void unregisterPortConfigOperator(PortConfigOperator portOp) {
        checkNotNull(portOp);


        // remove all portOp
        synchronized (portOpsIndex) {
            portOps.remove(portOp);

            // had to do this since COWArrayList iterator doesn't support remove
            portOpsIndex.keySet().forEach(key -> portOpsIndex.remove(key, portOp));
        }

    }

    /**
     * Merges the appropriate PortConfig with the description.
     *
     * @param did  ID of the Device where the port is attached
     * @param desc {@link PortDescription}
     * @return merged {@link PortDescription}
     */
    private PortDescription applyAllPortOps(DeviceId did, PortDescription desc) {
        return applyAllPortOps(new ConnectPoint(did, desc.portNumber()), desc);
    }

    /**
     * Merges the appropriate PortConfig with the description.
     *
     * @param cpt  ConnectPoint where the port is attached
     * @param desc {@link PortDescription}
     * @return merged {@link PortDescription}
     */
    private PortDescription applyAllPortOps(ConnectPoint cpt, PortDescription desc) {
        PortDescription work = desc;
        for (PortConfigOperator portOp : portOps) {
            work = portOp.combine(cpt, work);
        }
        return portAnnotationOp.combine(cpt, work);
    }

    /**
     * Merges the appropriate PortConfig with the description.
     *
     * @param cpt  ConnectPoint where the port is attached
     * @param desc {@link PortDescription}
     * @param prevConfig previous configuration
     * @return merged {@link PortDescription}
     */
    private PortDescription applyAllPortOps(ConnectPoint cpt, PortDescription desc,
                                            Optional<Config> prevConfig) {
        PortDescription work = desc;
        for (PortConfigOperator portOp : portOps) {
            work = portOp.combine(cpt, work, prevConfig);
        }
        return portAnnotationOp.combine(cpt, work, prevConfig);
    }

    /**
     * Port Enable/Disable message sent to the device's MASTER node.
     */
    private class InternalPortUpDownEvent {
        private final DeviceId deviceId;
        private final PortNumber portNumber;
        private final boolean enable;

        protected InternalPortUpDownEvent(
                DeviceId deviceId, PortNumber portNumber, boolean enable) {
            this.deviceId = deviceId;
            this.portNumber = portNumber;
            this.enable = enable;
        }

        public DeviceId deviceId() {
            return deviceId;
        }
        public PortNumber portNumber() {
            return portNumber;
        }
        public boolean isEnable() {
            return enable;
        }

        protected InternalPortUpDownEvent() {
            this.deviceId = null;
            this.portNumber = null;
            this.enable = false;
        }
    }
}
