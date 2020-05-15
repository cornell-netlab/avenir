/*
 * Copyright 2019-present Open Networking Foundation
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
package org.onosproject.k8snode.cli;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Lists;
import org.apache.commons.lang.StringUtils;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.k8snode.api.K8sNode;
import org.onosproject.k8snode.api.K8sNodeService;

import java.util.Comparator;
import java.util.List;

import static org.onosproject.k8snode.util.K8sNodeUtil.genFormatString;
import static org.onosproject.k8snode.util.K8sNodeUtil.prettyJson;

/**
 * Lists all nodes registered to the service.
 */
@Service
@Command(scope = "onos", name = "k8s-nodes",
        description = "Lists all nodes registered in kubernetes node service")
public class K8sNodeListCommand extends AbstractShellCommand {

    private static final int HOSTNAME_LENGTH = 35;
    private static final int TYPE_LENGTH = 15;
    private static final int MANAGEMENT_IP_LENGTH = 25;
    private static final int DATA_IP_LENGTH = 25;
    private static final int STATUS = 15;
    private static final int MARGIN_LENGTH = 2;

    @Override
    protected void doExecute() {
        K8sNodeService nodeService = get(K8sNodeService.class);
        List<K8sNode> nodes = Lists.newArrayList(nodeService.nodes());
        nodes.sort(Comparator.comparing(K8sNode::hostname));

        String format = genFormatString(ImmutableList.of(HOSTNAME_LENGTH,
                TYPE_LENGTH, MANAGEMENT_IP_LENGTH, DATA_IP_LENGTH, STATUS));

        if (outputJson()) {
            print("%s", json(nodes));
        } else {
            print(format, "Hostname", "Type", "Management IP", "Data IP", "State");
            for (K8sNode node : nodes) {
                print(format,
                        StringUtils.substring(node.hostname(), 0,
                                HOSTNAME_LENGTH - MARGIN_LENGTH),
                        node.type(),
                        StringUtils.substring(node.managementIp().toString(), 0,
                                MANAGEMENT_IP_LENGTH - MARGIN_LENGTH),
                        node.dataIp() != null ? StringUtils.substring(
                                node.dataIp().toString(), 0,
                                DATA_IP_LENGTH - MARGIN_LENGTH) : "",
                        node.state());
            }
            print("Total %s nodes", nodeService.nodes().size());
        }
    }

    private String json(List<K8sNode> nodes) {
        ObjectMapper mapper = new ObjectMapper();
        ArrayNode result = mapper.createArrayNode();
        for (K8sNode node : nodes) {
            result.add(jsonForEntity(node, K8sNode.class));
        }
        return prettyJson(mapper, result.toString());
    }
}
