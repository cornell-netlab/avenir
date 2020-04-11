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
package org.onosproject.store.service;

import java.util.concurrent.CompletableFuture;

/**
 * Async atomic value adapter.
 */
public class AsyncAtomicValueAdapter<V> implements AsyncAtomicValue<V> {
    @Override
    public String name() {
        return null;
    }

    @Override
    public CompletableFuture<Boolean> compareAndSet(V expect, V update) {
        return null;
    }

    @Override
    public CompletableFuture<V> get() {
        return null;
    }

    @Override
    public CompletableFuture<V> getAndSet(V value) {
        return null;
    }

    @Override
    public CompletableFuture<Void> set(V value) {
        return null;
    }

    @Override
    public CompletableFuture<Void> addListener(AtomicValueEventListener<V> listener) {
        return null;
    }

    @Override
    public CompletableFuture<Void> removeListener(AtomicValueEventListener<V> listener) {
        return null;
    }
}
