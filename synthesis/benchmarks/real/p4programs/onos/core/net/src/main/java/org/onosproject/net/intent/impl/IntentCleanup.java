/*
 * Copyright 2015-present Open Networking Foundation
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
package org.onosproject.net.intent.impl;

import org.onosproject.cfg.ComponentConfigService;
import org.onosproject.net.intent.IntentData;
import org.onosproject.net.intent.IntentEvent;
import org.onosproject.net.intent.IntentListener;
import org.onosproject.net.intent.IntentService;
import org.onosproject.net.intent.IntentStore;
import org.onosproject.net.intent.Key;
import org.onosproject.store.service.WallClockTimestamp;
import org.osgi.service.component.ComponentContext;
import org.osgi.service.component.annotations.Activate;
import org.osgi.service.component.annotations.Component;
import org.osgi.service.component.annotations.Deactivate;
import org.osgi.service.component.annotations.Modified;
import org.osgi.service.component.annotations.Reference;
import org.osgi.service.component.annotations.ReferenceCardinality;
import org.slf4j.Logger;

import java.util.Dictionary;
import java.util.Properties;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutorService;

import static com.google.common.base.Strings.isNullOrEmpty;
import static java.util.concurrent.Executors.newSingleThreadExecutor;
import static org.onlab.util.Tools.get;
import static org.onlab.util.Tools.groupedThreads;
import static org.onosproject.net.OsgiPropertyConstants.ICU_ENABLED;
import static org.onosproject.net.OsgiPropertyConstants.ICU_ENABLED_DEFAULT;
import static org.onosproject.net.OsgiPropertyConstants.ICU_PERIOD;
import static org.onosproject.net.OsgiPropertyConstants.ICU_PERIOD_DEFAULT;
import static org.onosproject.net.OsgiPropertyConstants.ICU_RETRY_THRESHOLD;
import static org.onosproject.net.OsgiPropertyConstants.ICU_RETRY_THRESHOLD_DEFAULT;
import static org.slf4j.LoggerFactory.getLogger;

/**
 * This component cleans up intents that have encountered errors or otherwise
 * stalled during installation or withdrawal.
 * <p>
 * It periodically polls (based on configured period) for pending and CORRUPT
 * intents from the store and retries. It also listens for CORRUPT event
 * notifications, which signify errors in processing, and retries.
 * </p>
 */
@Component(
    immediate = true,
    property = {
        ICU_ENABLED + ":Boolean=" + ICU_ENABLED_DEFAULT,
        ICU_PERIOD + ":Integer=" + ICU_PERIOD_DEFAULT,
        ICU_RETRY_THRESHOLD + ":Integer=" + ICU_RETRY_THRESHOLD_DEFAULT
    }
)
public class IntentCleanup implements Runnable, IntentListener {

    private static final Logger log = getLogger(IntentCleanup.class);

    // Logical timeout for stuck Intents in INSTALLING or WITHDRAWING. The unit is seconds
    private static final int INSTALLING_WITHDRAWING_PERIOD = 120;

    private long periodMs;
    private long periodMsForStuck;

    /** Enables/disables the intent cleanup component. */
    private boolean enabled = ICU_ENABLED_DEFAULT;

    /** Frequency in ms between cleanup runs. */
    protected int period = ICU_PERIOD_DEFAULT;

    /** Number of times to retry CORRUPT intent without delay. */
    protected int retryThreshold = ICU_RETRY_THRESHOLD_DEFAULT;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected IntentService service;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected IntentStore store;

    @Reference(cardinality = ReferenceCardinality.MANDATORY)
    protected ComponentConfigService cfgService;

    private ExecutorService executor;
    private Timer timer;
    private TimerTask timerTask;

    @Activate
    public void activate() {
        cfgService.registerProperties(getClass());
        executor = newSingleThreadExecutor(groupedThreads("onos/intent", "cleanup", log));
        timer = new Timer("onos-intent-cleanup-timer");
        service.addListener(this);
        adjustRate();
        log.info("Started");
    }

    @Deactivate
    public void deactivate() {
        cfgService.unregisterProperties(getClass(), false);
        service.removeListener(this);
        timer.cancel();
        timerTask = null;
        executor.shutdown();
        log.info("Stopped");
    }

    @Modified
    public void modified(ComponentContext context) {
        Dictionary<?, ?> properties = context != null ? context.getProperties() : new Properties();

        int newPeriod;
        boolean newEnabled;
        try {
            String s = get(properties, ICU_PERIOD);
            newPeriod = isNullOrEmpty(s) ? period : Integer.parseInt(s.trim());

            s = get(properties, ICU_RETRY_THRESHOLD);
            retryThreshold = isNullOrEmpty(s) ? retryThreshold : Integer.parseInt(s.trim());

            s = get(properties, ICU_ENABLED);
            newEnabled = isNullOrEmpty(s) ? enabled : Boolean.parseBoolean(s.trim());
        } catch (NumberFormatException e) {
            log.warn(e.getMessage());
            newPeriod = period;
            newEnabled = enabled;
        }

        // Any change in the following parameters implies hard restart
        // We could further restrict only for values multiple of the period
        // of the stuck intents
        if (newPeriod != period || enabled != newEnabled || newPeriod <= INSTALLING_WITHDRAWING_PERIOD) {
            period = newPeriod;
            enabled = newEnabled;
            adjustRate();
        }

        log.info("Settings: enabled={}, period={}, retryThreshold={}",
                 enabled, period, retryThreshold);
    }

    protected void adjustRate() {
        if (timerTask != null) {
            timerTask.cancel();
            timerTask = null;
        }

        if (enabled) {
            timerTask = new TimerTask() {
                @Override
                public void run() {
                    executor.execute(IntentCleanup.this);
                }
            };
            // Convert to ms
            periodMs = period * 1_000L;
            periodMsForStuck = INSTALLING_WITHDRAWING_PERIOD * 1000L;
            // Schedule the executions
            timer.scheduleAtFixedRate(timerTask, periodMs, periodMs);
        }
    }


    @Override
    public void run() {
        try {
            cleanup();
        } catch (Exception e) {
            log.warn("Caught exception during Intent cleanup", e);
        }
    }

    private void resubmitCorrupt(IntentData intentData, boolean checkThreshold) {
        if (checkThreshold && intentData.errorCount() >= retryThreshold) {
            //FIXME trace or debug statement?
            return; // threshold met or exceeded
        } // FIXME should we backoff here?

        switch (intentData.request()) {
            case INSTALL_REQ:
                service.submit(intentData.intent());
                break;
            case WITHDRAW_REQ:
                service.withdraw(intentData.intent());
                break;
            default:
                log.warn("Trying to resubmit corrupt/failed intent {} in state {} with request {}",
                         intentData.key(), intentData.state(), intentData.request());
                break;
        }
    }

    private void resubmitPendingRequest(IntentData intentData) {
        // FIXME should we back off here?
        switch (intentData.request()) {
            case INSTALL_REQ:
            case WITHDRAW_REQ:
            case PURGE_REQ:
                service.addPending(IntentData.copy(intentData, new WallClockTimestamp()));
                break;
            default:
                log.warn("Failed to resubmit pending intent {} in state {} with request {}",
                         intentData.key(), intentData.state(), intentData.request());
                break;
        }
    }

    /**
     * Iterates through corrupt, failed and pending intents and
     * re-submit/withdraw appropriately.
     */
    private void cleanup() {
        int corruptCount = 0, failedCount = 0, stuckCount = 0, pendingCount = 0, skipped = 0;

        // Check the pending map first, because the check of the current map
        // will add items to the pending map.
        for (IntentData intentData : store.getPendingData(true, periodMs)) {
            log.debug("Resubmit Pending Intent: key {}, state {}, request {}",
                      intentData.key(), intentData.state(), intentData.request());
            resubmitPendingRequest(intentData);
            pendingCount++;
        }

        for (IntentData intentData : store.getIntentData(true, periodMs)) {
            IntentData pendingIntentData = store.getPendingData(intentData.key());
            if (pendingIntentData != null) {
                continue;
            }

            switch (intentData.state()) {
                case FAILED:
                    log.debug("Resubmit Failed Intent: key {}, state {}, request {}",
                            intentData.key(), intentData.state(), intentData.request());
                    resubmitCorrupt(intentData, false);
                    failedCount++;
                    break;
                case CORRUPT:
                    log.debug("Resubmit Corrupt Intent: key {}, state {}, request {}",
                            intentData.key(), intentData.state(), intentData.request());
                    resubmitCorrupt(intentData, false);
                    corruptCount++;
                    break;
                case INSTALLING: //FALLTHROUGH
                case WITHDRAWING:
                    // Instances can have different clocks and potentially we can have problems
                    // An Intent can be submitted again before the real period of the stuck intents
                    final WallClockTimestamp time = new WallClockTimestamp(
                            System.currentTimeMillis() - periodMsForStuck
                    );
                    if (intentData.version().isOlderThan(time)) {
                        resubmitPendingRequest(intentData);
                        stuckCount++;
                    } else {
                        skipped++;
                    }
                    break;
                default:
                    //NOOP
                    break;
            }
        }

        if (corruptCount + failedCount + stuckCount + pendingCount > 0) {
            log.debug("Intent cleanup ran and resubmitted {} corrupt, {} failed, {} stuck, and {} pending intents",
                    corruptCount, failedCount, stuckCount, pendingCount);
        }
        if (skipped > 0) {
            log.debug("Intent cleanup skipped {} intents", skipped);
        }
    }

    @Override
    public void event(IntentEvent event) {
        // this is the fast path for CORRUPT intents, retry on event notification.
        //TODO we might consider using the timer to back off for subsequent retries
        if (enabled && event.type() == IntentEvent.Type.CORRUPT) {
            Key key = event.subject().key();
            if (store.isMaster(key)) {
                IntentData data = store.getIntentData(event.subject().key());
                resubmitCorrupt(data, true);
            }
        }
    }
}
