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

package org.onosproject.net.pi.model;

import com.google.common.annotations.Beta;
import org.onlab.util.Identifier;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Identifier of a packet metadata in a protocol-independent pipeline, unique within the scope of a pipeline model.
 */
@Beta
public final class PiPacketMetadataId extends Identifier<String> {

    private PiPacketMetadataId(String name) {
        super(name);
    }

    /**
     * Returns an identifier for the given packet metadata name.
     *
     * @param name packet metadata name
     * @return packet metadata ID
     */
    public static PiPacketMetadataId of(String name) {
        checkNotNull(name);
        checkArgument(!name.isEmpty(), "Name can't be empty");
        return new PiPacketMetadataId(name);
    }
}
