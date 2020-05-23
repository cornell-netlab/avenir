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

package org.onosproject.messagingperf;

/**
 * Name/Value constants for properties.
 */
public final class OsgiPropertyConstants {
    private OsgiPropertyConstants() {
    }

    public static final String SENDER_THREAD_POOL_SIZE = "totalSenderThreads";
    public static final int SENDER_THREAD_POOL_SIZE_DEFAULT = 2;

    public static final String RECEIVER_THREAD_POOL_SIZE = "totalReceiverThreads";
    public static final int RECEIVER_THREAD_POOL_SIZE_DEFAULT = 2;

    public static final String SERIALIZATION_ON = "serializationOn";
    public static final boolean SERIALIZATION_ON_DEFAULT = true;

    public static final String RECEIVE_ON_IO_LOOP_THREAD = "receiveOnIOLoopThread";
    public static final boolean RECEIVE_ON_IO_LOOP_THREAD_DEFAULT = false;
}
