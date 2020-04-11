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

package org.stratumproject.fabricdemo.common;

import org.onlab.packet.Ip6Address;
import org.onlab.packet.MacAddress;
import org.onosproject.net.DeviceId;
import org.onosproject.net.config.Config;

/**
 * Device configuration object.
 */
public class FabricDeviceConfig extends Config<DeviceId> {

    public static final String CONFIG_KEY = "fabricDeviceConfig";
    private static final String MY_STATION_MAC = "myStationMac";
    private static final String IS_SPINE = "isSpine";

    @Override
    public boolean isValid() {
        return hasOnlyFields(MY_STATION_MAC, IS_SPINE) &&
                myStationMac() != null;
    }

    /**
     * Gets the MAC address of the switch.
     *
     * @return MAC address of the switch. Or null if not configured.
     */
    public MacAddress myStationMac() {
        String mac = get(MY_STATION_MAC, null);
        return mac != null ? MacAddress.valueOf(mac) : null;
    }

    /**
     * Checks if the switch is a spine switch.
     *
     * @return true if the switch is a spine switch. false if the switch is not
     * a spine switch, or if the value is not configured.
     */
    public boolean isSpine() {
        String isSpine = get(IS_SPINE, null);
        return isSpine != null && Boolean.valueOf(isSpine);
    }
}
