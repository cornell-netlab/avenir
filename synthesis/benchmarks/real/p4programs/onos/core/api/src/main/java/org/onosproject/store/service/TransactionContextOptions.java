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

import org.onosproject.store.primitives.DistributedPrimitiveOptions;

/**
 * Abstract base class for a transaction context builder.
 */
public abstract class TransactionContextOptions<O extends TransactionContextOptions<O>>
    extends DistributedPrimitiveOptions<O> {
    public TransactionContextOptions() {
        super(DistributedPrimitive.Type.TRANSACTION_CONTEXT);
    }
}
