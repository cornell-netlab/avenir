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
package org.onosproject.pcep.server.impl;

import org.onosproject.pcep.server.PccId;
import org.onosproject.pcep.server.PcepCfg;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.TreeMap;


public class PcepConfig implements PcepCfg {

    private static final Logger log = LoggerFactory.getLogger(PcepConfig.class);

    private State state = State.INIT;
    private PccId pccId;
    private TreeMap<String, PcepCfg> bgpPeerTree = new TreeMap<>();

    @Override
    public State getState() {
        return state;
    }

    @Override
    public void setState(State state) {
        this.state = state;
    }

}
