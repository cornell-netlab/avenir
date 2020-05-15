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
package org.onosproject.inbandtelemetry.impl;

import com.google.common.collect.Maps;
import com.google.common.util.concurrent.Striped;
import org.onlab.util.KryoNamespace;
import org.onlab.util.SharedScheduledExecutors;
import org.onosproject.core.ApplicationId;
import org.onosproject.core.CoreService;
import org.onosproject.inbandtelemetry.api.IntConfig;
import org.onosproject.inbandtelemetry.api.IntIntent;
import org.onosproject.inbandtelemetry.api.IntIntentId;
import org.onosproject.inbandtelemetry.api.IntObjective;
import org.onosproject.inbandtelemetry.api.IntProgrammable;
import org.onosproject.inbandtelemetry.api.IntService;
import org.onosproject.mastership.MastershipService;
import org.onosproject.net.ConnectPoint;
import org.onosproject.net.Device;
import org.onosproject.net.DeviceId;
import org.onosproject.net.MastershipRole;
import org.onosproject.net.PortNumber;
import org.onosproject.net.device.DeviceEvent;
import org.onosproject.net.device.DeviceListener;
import org.onosproject.net.device.DeviceService;
import org.onosproject.net.host.HostEvent;
import org.onosproject.net.host.HostListener;
import org.onosproject.net.host.HostService;
import org.onosproject.store.serializers.KryoNamespaces;
import org.onosproject.store.service.AtomicIdGenerator;
import org.onosproject.store.service.AtomicValue;
import org.onosproject.store.service.AtomicValueEvent;
import org.onosproject.store.service.AtomicValueEventListener;
import org.onosproject.store.service.ConsistentMap;
import org.onosproject.store.service.MapEvent;
import org.onosproject.store.service.MapEventListener;
import org.onosproject.store.service.Serializer;
import org.onosproject.store.service.StorageService;
import org.onosproject.store.service.Versioned;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.slf4j.Logger;

import java.util.Collection;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.locks.Lock;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;
import static org.slf4j.LoggerFactory.getLogger;

/**
 * Simple implementation of IntService, for controlling INT-capable pipelines.
 * <p>
 * All INT intents are converted to an equivalent INT objective and applied to
 * all SOURCE_SINK devices. A device is deemed SOURCE_SINK if it has at least
 * one host attached.
 * <p>
 * The implementation listens for different types of events and when required it
 * configures a device by cleaning-up any previous state and applying the new
 * one.
 */
@Component(immediate = true, service = IntService.class)
public class SimpleIntManager implements IntService {

    private final Logger log = getLogger(getClass());

    private static final int CONFIG_EVENT_DELAY = 5; // Seconds.

    private static final String APP_NAME = "org.onosproject.inbandtelemetry";

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private CoreService coreService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private DeviceService deviceService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private StorageService storageService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private MastershipService mastershipService;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    private HostService hostService;

    private final Striped<Lock> deviceLocks = Striped.lock(10);

    private final ConcurrentMap<DeviceId, ScheduledFuture<?>> scheduledDeviceTasks = Maps.newConcurrentMap();

    // Distributed state.
    private ConsistentMap<IntIntentId, IntIntent> intentMap;
    private ConsistentMap<DeviceId, Long> devicesToConfigure;
    private AtomicValue<IntConfig> intConfig;
    private AtomicValue<Boolean> intStarted;
    private AtomicIdGenerator intentIds;

    // Event listeners.
    private final InternalHostListener hostListener = new InternalHostListener();
    private final InternalDeviceListener deviceListener = new InternalDeviceListener();
    private final InternalIntentMapListener intentMapListener = new InternalIntentMapListener();
    private final InternalIntConfigListener intConfigListener = new InternalIntConfigListener();
    private final InternalIntStartedListener intStartedListener = new InternalIntStartedListener();
    private final InternalDeviceToConfigureListener devicesToConfigureListener =
            new InternalDeviceToConfigureListener();

    @Activate
    public void activate() {

        final ApplicationId appId = coreService.registerApplication(APP_NAME);

        KryoNamespace.Builder serializer = KryoNamespace.newBuilder()
                .register(KryoNamespaces.API)
                .register(IntIntent.class)
                .register(IntIntentId.class)
                .register(IntDeviceRole.class)
                .register(IntIntent.IntHeaderType.class)
                .register(IntIntent.IntMetadataType.class)
                .register(IntIntent.IntReportType.class)
                .register(IntIntent.TelemetryMode.class)
                .register(IntConfig.class)
                .register(IntConfig.TelemetrySpec.class);

        devicesToConfigure = storageService.<DeviceId, Long>consistentMapBuilder()
                .withSerializer(Serializer.using(serializer.build()))
                .withName("onos-int-devices-to-configure")
                .withApplicationId(appId)
                .withPurgeOnUninstall()
                .build();
        devicesToConfigure.addListener(devicesToConfigureListener);

        intentMap = storageService.<IntIntentId, IntIntent>consistentMapBuilder()
                .withSerializer(Serializer.using(serializer.build()))
                .withName("onos-int-intents")
                .withApplicationId(appId)
                .withPurgeOnUninstall()
                .build();
        intentMap.addListener(intentMapListener);

        intStarted = storageService.<Boolean>atomicValueBuilder()
                .withSerializer(Serializer.using(serializer.build()))
                .withName("onos-int-started")
                .withApplicationId(appId)
                .build()
                .asAtomicValue();
        intStarted.addListener(intStartedListener);

        intConfig = storageService.<IntConfig>atomicValueBuilder()
                .withSerializer(Serializer.using(serializer.build()))
                .withName("onos-int-config")
                .withApplicationId(appId)
                .build()
                .asAtomicValue();
        intConfig.addListener(intConfigListener);

        intentIds = storageService.getAtomicIdGenerator("int-intent-id-generator");

        // Bootstrap config for already existing devices.
        triggerAllDeviceConfigure();

        hostService.addListener(hostListener);
        deviceService.addListener(deviceListener);

        startInt();
        log.info("Started", appId.id());
    }

    @Deactivate
    public void deactivate() {
        deviceService.removeListener(deviceListener);
        hostService.removeListener(hostListener);
        intentIds = null;
        intConfig.removeListener(intConfigListener);
        intConfig = null;
        intStarted.removeListener(intStartedListener);
        intStarted = null;
        intentMap.removeListener(intentMapListener);
        intentMap = null;
        devicesToConfigure.removeListener(devicesToConfigureListener);
        devicesToConfigure.destroy();
        devicesToConfigure = null;
        // Cancel tasks (if any).
        scheduledDeviceTasks.values().forEach(f -> {
            f.cancel(true);
            if (!f.isDone()) {
                try {
                    f.get(1, TimeUnit.SECONDS);
                } catch (InterruptedException | ExecutionException | TimeoutException e) {
                    // Don't care, we are terminating the service anyways.
                }
            }
        });
        // Clean up INT rules from existing devices.
        deviceService.getDevices().forEach(d -> cleanupDevice(d.id()));
        log.info("Deactivated");
    }

    @Override
    public void startInt() {
        // Atomic value event will trigger device configure.
        intStarted.set(true);
    }

    @Override
    public void startInt(Set<DeviceId> deviceIds) {
        log.warn("Starting INT for a subset of devices is not supported");
    }

    @Override
    public void stopInt() {
        // Atomic value event will trigger device configure.
        intStarted.set(false);
    }

    @Override
    public void stopInt(Set<DeviceId> deviceIds) {
        log.warn("Stopping INT for a subset of devices is not supported");
    }

    @Override
    public void setConfig(IntConfig cfg) {
        checkNotNull(cfg);
        // Atomic value event will trigger device configure.
        intConfig.set(cfg);
    }

    @Override
    public IntConfig getConfig() {
        return intConfig.get();
    }

    @Override
    public IntIntentId installIntIntent(IntIntent intent) {
        checkNotNull(intent);
        final Integer intentId = (int) intentIds.nextId();
        final IntIntentId intIntentId = IntIntentId.valueOf(intentId);
        // Intent map event will trigger device configure.
        intentMap.put(intIntentId, intent);
        return intIntentId;
    }

    @Override
    public void removeIntIntent(IntIntentId intentId) {
        checkNotNull(intentId);
        // Intent map event will trigger device configure.
        intentMap.remove(intentId).value();
    }

    @Override
    public IntIntent getIntIntent(IntIntentId intentId) {
        return Optional.ofNullable(intentMap.get(intentId).value()).orElse(null);
    }

    @Override
    public Map<IntIntentId, IntIntent> getIntIntents() {
        return intentMap.asJavaMap();
    }

    private boolean isConfigTaskValid(DeviceId deviceId, long creationTime) {
        Versioned<?> versioned = devicesToConfigure.get(deviceId);
        return versioned != null && versioned.creationTime() == creationTime;
    }

    private boolean isIntStarted() {
        return intStarted.get();
    }

    private boolean isNotIntConfigured() {
        return intConfig.get() == null;
    }

    private boolean isIntProgrammable(DeviceId deviceId) {
        final Device device = deviceService.getDevice(deviceId);
        return device != null && device.is(IntProgrammable.class);
    }

    private void triggerDeviceConfigure(DeviceId deviceId) {
        if (isIntProgrammable(deviceId)) {
            devicesToConfigure.put(deviceId, System.nanoTime());
        }
    }

    private void triggerAllDeviceConfigure() {
        deviceService.getDevices().forEach(d -> triggerDeviceConfigure(d.id()));
    }

    private void configDeviceTask(DeviceId deviceId, long creationTime) {
        if (isConfigTaskValid(deviceId, creationTime)) {
            // Task outdated.
            return;
        }
        if (!deviceService.isAvailable(deviceId)) {
            return;
        }
        final MastershipRole role = mastershipService.requestRoleForSync(deviceId);
        if (!role.equals(MastershipRole.MASTER)) {
            return;
        }
        deviceLocks.get(deviceId).lock();
        try {
            // Clean up first.
            cleanupDevice(deviceId);
            if (!configDevice(deviceId)) {
                // Clean up if fails.
                cleanupDevice(deviceId);
                return;
            }
            devicesToConfigure.remove(deviceId);
        } finally {
            deviceLocks.get(deviceId).unlock();
        }
    }

    private void cleanupDevice(DeviceId deviceId) {
        final Device device = deviceService.getDevice(deviceId);
        if (device == null || !device.is(IntProgrammable.class)) {
            return;
        }
        device.as(IntProgrammable.class).cleanup();
    }

    private boolean configDevice(DeviceId deviceId) {
        // Returns true if config was successful, false if not and a clean up is
        // needed.
        final Device device = deviceService.getDevice(deviceId);
        if (device == null || !device.is(IntProgrammable.class)) {
            return true;
        }

        if (isNotIntConfigured()) {
            log.warn("Missing INT config, aborting programming of INT device {}", deviceId);
            return true;
        }

        final boolean isEdge = !hostService.getConnectedHosts(deviceId).isEmpty();
        final IntDeviceRole intDeviceRole = isEdge
                ? IntDeviceRole.SOURCE_SINK
                : IntDeviceRole.TRANSIT;

        log.info("Started programming of INT device {} with role {}...",
                 deviceId, intDeviceRole);

        final IntProgrammable intProg = device.as(IntProgrammable.class);

        if (!isIntStarted()) {
            // Leave device with no INT configuration.
            return true;
        }

        if (!intProg.init()) {
            log.warn("Unable to init INT pipeline on {}", deviceId);
            return false;
        }

        if (intDeviceRole != IntDeviceRole.SOURCE_SINK) {
            // Stop here, no more configuration needed for transit devices.
            return true;
        }

        if (intProg.supportsFunctionality(IntProgrammable.IntFunctionality.SINK)) {
            if (!intProg.setupIntConfig(intConfig.get())) {
                log.warn("Unable to apply INT report config on {}", deviceId);
                return false;
            }
        }

        // Port configuration.
        final Set<PortNumber> hostPorts = deviceService.getPorts(deviceId)
                .stream()
                .map(port -> new ConnectPoint(deviceId, port.number()))
                .filter(cp -> !hostService.getConnectedHosts(cp).isEmpty())
                .map(ConnectPoint::port)
                .collect(Collectors.toSet());

        for (PortNumber port : hostPorts) {
            if (intProg.supportsFunctionality(IntProgrammable.IntFunctionality.SOURCE)) {
                log.info("Setting port {}/{} as INT source port...", deviceId, port);
                if (!intProg.setSourcePort(port)) {
                    log.warn("Unable to set INT source port {} on {}", port, deviceId);
                    return false;
                }
            }
            if (intProg.supportsFunctionality(IntProgrammable.IntFunctionality.SINK)) {
                log.info("Setting port {}/{} as INT sink port...", deviceId, port);
                if (!intProg.setSinkPort(port)) {
                    log.warn("Unable to set INT sink port {} on {}", port, deviceId);
                    return false;
                }
            }
        }

        if (!intProg.supportsFunctionality(IntProgrammable.IntFunctionality.SOURCE)) {
            // Stop here, no more configuration needed for sink devices.
            return true;
        }

        // Apply intents.
        // This is a trivial implementation where we simply get the
        // corresponding INT objective from an intent and we apply to all source
        // device.
        final Collection<IntObjective> objectives = intentMap.values().stream()
                .map(v -> getIntObjective(v.value()))
                .collect(Collectors.toList());
        int appliedCount = 0;
        for (IntObjective objective : objectives) {
            if (intProg.addIntObjective(objective)) {
                appliedCount = appliedCount + 1;
            }
        }

        log.info("Completed programming of {}, applied {} INT objectives of {} total",
                 deviceId, appliedCount, objectives.size());

        return true;
    }

    private IntObjective getIntObjective(IntIntent intent) {
        return new IntObjective.Builder()
                .withSelector(intent.selector())
                .withMetadataTypes(intent.metadataTypes())
                .withHeaderType(intent.headerType())
                .build();
    }

    /* Event listeners which trigger device configuration. */

    private class InternalHostListener implements HostListener {
        @Override
        public void event(HostEvent event) {
            final DeviceId deviceId = event.subject().location().deviceId();
            triggerDeviceConfigure(deviceId);
        }
    }

    private class InternalDeviceListener implements DeviceListener {
        @Override
        public void event(DeviceEvent event) {
            switch (event.type()) {
                case DEVICE_ADDED:
                case DEVICE_UPDATED:
                case DEVICE_REMOVED:
                case DEVICE_SUSPENDED:
                case DEVICE_AVAILABILITY_CHANGED:
                case PORT_ADDED:
                case PORT_UPDATED:
                case PORT_REMOVED:
                    triggerDeviceConfigure(event.subject().id());
                    return;
                case PORT_STATS_UPDATED:
                    return;
                default:
                    log.warn("Unknown device event type {}", event.type());
            }
        }
    }

    private class InternalIntentMapListener
            implements MapEventListener<IntIntentId, IntIntent> {
        @Override
        public void event(MapEvent<IntIntentId, IntIntent> event) {
            triggerAllDeviceConfigure();
        }
    }

    private class InternalIntConfigListener
            implements AtomicValueEventListener<IntConfig> {
        @Override
        public void event(AtomicValueEvent<IntConfig> event) {
            triggerAllDeviceConfigure();
        }
    }

    private class InternalIntStartedListener
            implements AtomicValueEventListener<Boolean> {
        @Override
        public void event(AtomicValueEvent<Boolean> event) {
            triggerAllDeviceConfigure();
        }
    }

    private class InternalDeviceToConfigureListener
            implements MapEventListener<DeviceId, Long> {
        @Override
        public void event(MapEvent<DeviceId, Long> event) {
            if (event.type().equals(MapEvent.Type.REMOVE) ||
                    event.newValue() == null) {
                return;
            }
            // Schedule task in the future. Wait for events for this device to
            // stabilize.
            final DeviceId deviceId = event.key();
            final long creationTime = event.newValue().creationTime();
            ScheduledFuture<?> newTask = SharedScheduledExecutors.newTimeout(
                    () -> configDeviceTask(deviceId, creationTime),
                    CONFIG_EVENT_DELAY, TimeUnit.SECONDS);
            ScheduledFuture<?> oldTask = scheduledDeviceTasks.put(deviceId, newTask);
            if (oldTask != null) {
                oldTask.cancel(false);
            }
        }
    }
}
