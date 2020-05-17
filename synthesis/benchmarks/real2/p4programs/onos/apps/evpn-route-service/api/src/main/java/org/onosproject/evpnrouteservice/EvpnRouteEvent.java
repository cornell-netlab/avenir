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

package org.onosproject.evpnrouteservice;

import java.util.Collection;
import java.util.Collections;
import java.util.Objects;

import org.onlab.util.Tools;
import org.onosproject.event.AbstractEvent;

import static com.google.common.base.MoreObjects.toStringHelper;

/**
 * Describes an event about a route.
 */
public class EvpnRouteEvent extends AbstractEvent<EvpnRouteEvent.Type,
        EvpnRoute> {

    private final EvpnRoute prevSubject;
    private final Collection<EvpnRoute> alternativeRoutes;

    /**
     * Route event type.
     */
    public enum Type {

        /**
         * Route is new and the next hop is resolved.
         * <p>
         * The subject of this event should be the route being added.
         * The prevSubject of this event should be null.
         * </p>
         */
        ROUTE_ADDED,

        /**
         * Route has updated information.
         * <p>
         * The subject of this event should be the new route.
         * The prevSubject of this event should be the old route.
         * </p>
         */
        ROUTE_UPDATED,

        /**
         * Route was removed or the next hop becomes unresolved.
         * <p>
         * The subject of this event should be the route being removed.
         * The prevSubject of this event should be null.
         * </p>
         */
        ROUTE_REMOVED,

        /**
         * The set of alternative routes for the subject's prefix has changed,
         * but the best route is still the same.
         * <p>
         * The subject is the best route for the prefix (which has already been
         * notified in a previous event).
         * The prevSubject of this event is null.
         * The alternatives contains the new set of alternative routes.
         * </p>
         */
        ALTERNATIVE_ROUTES_CHANGED
    }

    /**
     * Creates a new route event without specifying previous subject.
     *
     * @param type    event type
     * @param subject event subject
     */
    public EvpnRouteEvent(Type type, EvpnRoute subject) {
        this(type, subject, null, Collections.emptySet());
    }

    /**
     * Creates a new route event without specifying previous subject.
     *
     * @param type         event type
     * @param subject      event subject
     * @param alternatives alternative routes for subject's prefix
     */
    public EvpnRouteEvent(Type type, EvpnRoute subject,
                          Collection<EvpnRoute> alternatives) {
        this(type, subject, null, alternatives);
    }

    /**
     * Creates a new route event.
     *
     * @param type    event type
     * @param subject event subject
     * @param time    event time
     */
    protected EvpnRouteEvent(Type type, EvpnRoute subject, long time) {
        super(type, subject, time);
        this.prevSubject = null;

        this.alternativeRoutes = Collections.emptySet();
    }

    /**
     * Creates a new route event with previous subject.
     *
     * @param type        event type
     * @param subject     event subject
     * @param prevSubject previous subject
     */
    public EvpnRouteEvent(Type type, EvpnRoute subject, EvpnRoute prevSubject) {
        this(type, subject, prevSubject, Collections.emptySet());
    }

    /**
     * Creates a new route event with a previous subject and alternative routes.
     *
     * @param type         event type
     * @param subject      event subject
     * @param prevSubject  previous subject
     * @param alternatives alternative routes for subject's prefix
     */
    public EvpnRouteEvent(Type type, EvpnRoute subject, EvpnRoute prevSubject,
                          Collection<EvpnRoute> alternatives) {
        super(type, subject);
        this.prevSubject = prevSubject;
        this.alternativeRoutes = alternatives;
    }

    /**
     * Returns the previous subject of the event.
     *
     * @return previous subject to which this event pertains
     */
    public EvpnRoute prevSubject() {
        return prevSubject;
    }

    /**
     * Returns the set of alternative routes for the subject's prefix.
     *
     * @return alternative routes
     */
    public Collection<EvpnRoute> alternatives() {
        return alternativeRoutes;
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject(), type(), prevSubject(), alternativeRoutes);
    }

    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }

        if (!(other instanceof EvpnRouteEvent)) {
            return false;
        }

        EvpnRouteEvent that = (EvpnRouteEvent) other;

        return Objects.equals(this.subject(), that.subject()) &&
                Objects.equals(this.type(), that.type()) &&
                Objects.equals(this.prevSubject, that.prevSubject) &&
                Objects.equals(this.alternativeRoutes, that.alternativeRoutes);
    }

    @Override
    public String toString() {
        return toStringHelper(this)
                .add("time", Tools.defaultOffsetDataTime(time()))
                .add("type", type())
                .add("subject", subject())
                .add("prevSubject", prevSubject)
                .add("alternatives", alternativeRoutes)
                .toString();
    }
}
