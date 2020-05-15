/*
 * Copyright 2018-present Open Networking Foundation
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
package org.onosproject.store.service;

import java.util.function.BiFunction;

import org.onosproject.store.primitives.DistributedPrimitiveOptions;

/**
 * Builder for constructing new AtomicValue instances.
 *
 * @param <V> atomic value type
 */
public abstract class AtomicValueOptions<O extends AtomicValueOptions<O, V>, V> extends DistributedPrimitiveOptions<O> {
    protected BiFunction<V, org.onosproject.core.Version, V> compatibilityFunction;

    public AtomicValueOptions() {
        super(DistributedPrimitive.Type.VALUE);
    }

    /**
     * Sets a compatibility function on the map.
     *
     * @param compatibilityFunction the compatibility function
     * @return the consistent map builder
     */
    @SuppressWarnings("unchecked")
    public O withCompatibilityFunction(
        BiFunction<V, org.onosproject.core.Version, V> compatibilityFunction) {
        this.compatibilityFunction = compatibilityFunction;
        return (O) this;
    }
}
