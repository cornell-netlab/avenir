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

package org.onosproject.net.intent;

/**
 * Manage installation process for specific installable Intents.
 */
public interface IntentInstaller<T extends Intent> {

    /**
     * The installation direction.
     */
    enum Direction {
        /**
         * Direction for adding any installable objects(flows, configs...).
         */
        ADD,

        /**
         * Direction for removing any installable objects(flows, configs...).
         */
        REMOVE
    }

    /**
     * Applies an Intent operation context.
     *
     * @param context the Intent operation context
     */
    void apply(IntentOperationContext<T> context);
}
