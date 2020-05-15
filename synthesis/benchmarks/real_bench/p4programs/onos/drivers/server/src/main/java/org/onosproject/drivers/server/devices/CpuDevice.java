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

package org.onosproject.drivers.server.devices;

/**
 * Represents an abstraction of a CPU core in ONOS.
 */
public interface CpuDevice {

    /**
     * Returns the ID of this CPU.
     * Typically this is the order number (0...N-1)
     * of this CPU core in the socket.
     *
     * @return CPU core ID
     */
    int id();

    /**
     * Returns the vendor of this CPU core.
     *
     * @return CPU core vendor
     */
    CpuVendor vendor();

    /**
     * Returns the frequency of this CPU core in MHz.
     *
     * @return CPU core frequency in MHz
     */
    long frequency();

}
