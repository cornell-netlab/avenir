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
package org.onosproject.faultmanagement.alarms.gui;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.google.common.base.Strings;
import com.google.common.collect.ImmutableSet;
import org.onosproject.alarm.Alarm;
import org.onosproject.net.DeviceId;
import org.onosproject.ui.RequestHandler;
import org.onosproject.ui.UiMessageHandler;
import org.onosproject.ui.table.TableModel;
import org.onosproject.ui.table.TableRequestHandler;
import org.onosproject.ui.table.cell.TimeFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Instant;
import java.util.Collection;
import java.util.Set;

import static org.onosproject.alarm.AlarmId.alarmId;

/**
 * Skeletal ONOS UI Table-View message handler.
 */
public class AlarmTableMessageHandler extends UiMessageHandler {

    private static final String ALARM_TABLE_DATA_REQ = "alarmTableDataRequest";
    private static final String ALARM_TABLE_DATA_RESP = "alarmTableDataResponse";
    private static final String ALARM_TABLES = "alarmTables";

    private static final String ALARM_TABLE_DETAIL_REQ = "alarmTableDetailsRequest";
    private static final String ALARM_TABLE_DETAIL_RESP = "alarmTableDetailsResponse";
    private static final String DETAILS = "details";

    private static final String ID = "id";
    private static final String DEVICE_ID_STR = "alarmDeviceId";
    private static final String DESCRIPTION = "alarmDesc";
    private static final String SOURCE = "alarmSource";
    private static final String TIME_RAISED = "alarmTimeRaised";
    private static final String TIME_UPDATED = "alarmTimeUpdated";
    private static final String TIME_CLEARED = "alarmTimeCleared";
    private static final String SEVERITY = "alarmSeverity";
    private static final String SERVICE_AFFECTING = "alarmServiceAffecting";
    private static final String ACKNOWLEDGED = "alarmAcknowledged";
    private static final String CLEARED = "alarmCleared";
    private static final String MANUALLY_CLEARABLE = "alarmManuallyClearable";
    private static final String ASSIGNED_USER = "alarmAssignedUser";
    private static final String RESULT = "result";

    // TODO No need to show id column in ONOS-GUI

    // TODO Replace SEVERITY column by color-coding of row depending on severity
    // e.g. red=critical, green=cleared etc

    private static final String[] COLUMN_IDS = {
            ID, DEVICE_ID_STR, DESCRIPTION, SOURCE, TIME_RAISED, SEVERITY,
            MANUALLY_CLEARABLE, CLEARED, ACKNOWLEDGED
    };

    private final Logger log = LoggerFactory.getLogger(getClass());

    @Override
    protected Collection<RequestHandler> createRequestHandlers() {
        return ImmutableSet.of(
                new AlarmTableDataRequestHandler(),
                new AlarmTableDetailRequestHandler()
        );
    }

    // handler for alarm table requests
    private final class AlarmTableDataRequestHandler extends TableRequestHandler {

        private static final String NO_ROWS_MESSAGE = "No alarms found";

        private AlarmTableDataRequestHandler() {
            super(ALARM_TABLE_DATA_REQ, ALARM_TABLE_DATA_RESP, ALARM_TABLES);
        }

        @Override
        protected String defaultColumnId() {
            return ID;
        }

        @Override
        protected String[] getColumnIds() {
            return COLUMN_IDS;
        }

        @Override
        protected String noRowsMessage(ObjectNode payload) {
            return NO_ROWS_MESSAGE;
        }

        @Override
        protected TableModel createTableModel() {
            TableModel tm = super.createTableModel();
            tm.setFormatter(TIME_RAISED, new TimeFormatter());
            return tm;
        }

        @Override
        protected void populateTable(TableModel tm, ObjectNode payload) {
            log.debug(" populateTable: tm = {}; payload = {}", tm, payload);
            String devId = string(payload, "devId");

            Set<Alarm> alarms = Strings.isNullOrEmpty(devId) ?
                    AlarmServiceUtil.lookUpAlarms() :
                    AlarmServiceUtil.lookUpAlarms(DeviceId.deviceId(devId));

            alarms.forEach((alarm) -> populateRow(tm.addRow(), alarm));
        }

        private void populateRow(TableModel.Row row, Alarm alarm) {
            log.debug("populateRow: row = {} alarm = {}", row, alarm);

            row.cell(ID, alarm.id())
                    .cell(DEVICE_ID_STR, alarm.deviceId())
                    .cell(DESCRIPTION, alarm.description())
                    .cell(SOURCE, alarm.source())
                    .cell(TIME_RAISED, Instant.ofEpochMilli(alarm.timeRaised()))
                    .cell(SEVERITY, alarm.severity())
                    .cell(MANUALLY_CLEARABLE, alarm.manuallyClearable())
                    .cell(CLEARED, alarm.cleared())
                    .cell(ACKNOWLEDGED, alarm.acknowledged());
        }
    }

    // handler for alarm details requests
    private final class AlarmTableDetailRequestHandler extends RequestHandler {

        private AlarmTableDetailRequestHandler() {
            super(ALARM_TABLE_DETAIL_REQ);
        }

        @Override
        public void process(ObjectNode payload) {
            log.debug("payload = {}", payload);

            String id = string(payload, ID, "(none)");
            Alarm alarm = AlarmServiceUtil.lookupAlarm(alarmId(id));
            ObjectNode rootNode = objectNode();
            ObjectNode data = objectNode();
            rootNode.set(DETAILS, data);

            if (alarm == null) {
                rootNode.put(RESULT, "Item with id '" + id + "' not found");
                log.warn("attempted to get item detail for id '{}'", id);

            } else {
                rootNode.put(RESULT, "Found item with id '" + id + "'");

                data.put(ID, alarm.id().toString());
                data.put(DESCRIPTION, alarm.description());
                data.put(DEVICE_ID_STR, alarm.deviceId().toString());
                data.put(SOURCE, alarm.source().toString());
                long timeRaised = alarm.timeRaised();
                data.put(TIME_RAISED,
                         formatTime(timeRaised)
                );
                data.put(TIME_UPDATED, formatTime(alarm.timeUpdated()));
                data.put(TIME_CLEARED, formatTime(alarm.timeCleared()));
                data.put(SEVERITY, alarm.severity().toString());
                // TODO: The following should be change to %yes% and %no% if LION is added to Alarm View
                data.put(SERVICE_AFFECTING,
                         alarm.serviceAffecting() ? "Yes" : "No");
                data.put(ACKNOWLEDGED,
                         alarm.acknowledged() ? "Yes" : "No");
                data.put(CLEARED,
                         alarm.cleared() ? "Yes" : "No");
                data.put(ASSIGNED_USER, alarm.assignedUser());
            }
            log.debug("send = {}", rootNode);

            sendMessage(ALARM_TABLE_DETAIL_RESP, rootNode);
        }
    }

    private static String formatTime(Long msSinceStartOfEpoch) {
        if (msSinceStartOfEpoch == null) {
            return "-";
        }
        return new TimeFormatter().format(Instant.ofEpochMilli(msSinceStartOfEpoch));
    }
}
