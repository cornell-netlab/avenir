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

package org.onosproject.pcep.server;

/**
 * PCEP peer state information.
 */

public interface PcepCfg {

    State getState();

    void setState(State state);

    enum State {
        /**
         * Signifies that its just created.
         */
        INIT,

        /**
         * Signifies that only IP Address is configured.
         */
        OPENWAIT,

        /**
         * Signifies that only Autonomous System is configured.
         */
        KEEPWAIT,

        /**
         * Signifies that both IP and Autonomous System is configured.
         */
        ESTABLISHED,

        /**
         * Signifies that both IP and Autonomous System is down.
         */
        DOWN
    }

}
