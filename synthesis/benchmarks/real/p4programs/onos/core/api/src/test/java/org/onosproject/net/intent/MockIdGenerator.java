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
package org.onosproject.net.intent;

import org.onosproject.core.IdGenerator;

import java.util.concurrent.atomic.AtomicLong;

/**
 * Mock id generator for testing.
 */
public final class MockIdGenerator implements IdGenerator {

    public static final MockIdGenerator INSTANCE = new MockIdGenerator();

    private static boolean generatorIsBound = false;

    // Ban public construction
    private MockIdGenerator() {
    }

    /**
     * Binds clean mock generator to the intent.
     */
    public static synchronized void cleanBind() {
        INSTANCE.nextId = new AtomicLong(0);
        if (!generatorIsBound) {
            generatorIsBound = true;
            Intent.unbindIdGenerator(INSTANCE);
            Intent.bindIdGenerator(INSTANCE);
        }
    }

    /**
     * Unbinds mock generator from the intent.
     */
    public static synchronized void unbind() {
        if (generatorIsBound) {
            generatorIsBound = false;
            Intent.unbindIdGenerator(INSTANCE);
        }
    }

    private AtomicLong nextId = new AtomicLong(0);

    @Override
    public long getNewId() {
        return nextId.getAndIncrement();
    }
}
