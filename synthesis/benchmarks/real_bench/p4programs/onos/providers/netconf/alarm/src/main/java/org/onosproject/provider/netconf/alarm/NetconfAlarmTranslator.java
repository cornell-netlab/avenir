/*
 * Copyright 2016-present Open Networking Foundation
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

package org.onosproject.provider.netconf.alarm;

import com.google.common.collect.ImmutableSet;
import org.onosproject.alarm.Alarm;
import org.onosproject.alarm.AlarmId;
import org.onosproject.alarm.AlarmTranslator;
import org.onosproject.alarm.DefaultAlarm;
import org.onosproject.alarm.XmlEventParser;
import org.onosproject.net.DeviceId;
import org.slf4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;

import org.xml.sax.SAXException;

import static org.slf4j.LoggerFactory.getLogger;

/**
 * Translates NETCONF notification messages to actions on alarms.
 */
public class NetconfAlarmTranslator implements AlarmTranslator {

    private final Logger log = getLogger(getClass());
    private static final String EVENTTIME_TAGNAME = "eventTime";

    @Override
    public Collection<Alarm> translateToAlarm(DeviceId deviceId, InputStream message) {
        try {
            Collection<Alarm> alarms = new ArrayList<>();
            Document doc = XmlEventParser.createDocFromMessage(message);
            long timeStamp = XmlEventParser.getEventTime(doc);
            Node descriptionNode = XmlEventParser.getDescriptionNode(doc);
            while (descriptionNode != null) {
                if (descriptionNode.getNodeType() == Node.ELEMENT_NODE) {
                    String description = nodeToString(descriptionNode);
                    alarms.add(new DefaultAlarm.Builder(AlarmId.alarmId(deviceId, Long.toString(timeStamp)),
                                                        deviceId, description,
                                                        Alarm.SeverityLevel.WARNING,
                                                        timeStamp).build());
                    descriptionNode = null;
                } else {
                    descriptionNode = descriptionNode.getNextSibling();
                }
            }
            return alarms;
        } catch (SAXException | IOException | ParserConfigurationException |
                UnsupportedOperationException | IllegalArgumentException |
                TransformerException e) {
            log.error("Exception thrown translating message from {}.", deviceId, e);
            return ImmutableSet.of();
        }
    }

    private static String nodeToString(Node rootNode) throws TransformerException {
        TransformerFactory tf = TransformerFactory.newInstance();
        Transformer transformer = tf.newTransformer();
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
        StringWriter writer = new StringWriter();
        DOMSource source = new DOMSource(rootNode);
        transformer.transform(source, new StreamResult(writer));
        return writer.getBuffer().toString();
    }
}
