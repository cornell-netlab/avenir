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
package org.onosproject.net.optical.device.port;

import java.util.Optional;

import org.onosproject.net.Port;
import org.onosproject.net.optical.OmsPort;
import org.onosproject.net.optical.device.OmsPortHelper;

import com.google.common.annotations.Beta;

/**
 * {@link PortMapper} to handler {@link OmsPort} translation.
 */
@Beta
public class OmsPortMapper extends AbstractPortMapper<OmsPort> {

    @Override
    public boolean is(Port port) {
        return port != null &&
               port.type() == Port.Type.OMS &&
               super.is(port);
    }

    @Override
    public Optional<OmsPort> as(Port port) {
        if (port instanceof OmsPort) {
            return Optional.of((OmsPort) port);
        }
        return super.as(port);
    }

    @Override
    protected Optional<OmsPort> mapPort(Port port) {
        if (port instanceof OmsPort) {
            return Optional.of((OmsPort) port);
        }
        return OmsPortHelper.asOmsPort(port);
    }

}
