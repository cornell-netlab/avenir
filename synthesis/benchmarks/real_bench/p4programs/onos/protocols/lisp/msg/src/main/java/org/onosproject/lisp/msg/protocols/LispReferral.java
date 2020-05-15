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
package org.onosproject.lisp.msg.protocols;

/**
 * LISP referral section which is part of LISP referral record.
 */
public interface LispReferral extends LispGenericLocator {

    /**
     * A builder of LISP referral.
     */
    interface ReferralBuilder extends GenericLocatorBuilder<ReferralBuilder> {

        /**
         * Builds referral.
         *
         * @return referral instance
         */
        LispReferral build();
    }
}
