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
package org.onosproject.workflow.cli;

import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractChoicesCompleter;
import org.onosproject.workflow.api.WorkflowStore;

import java.util.List;
import java.util.stream.Collectors;

import static org.onlab.osgi.DefaultServiceDirectory.getService;

/**
 * Workflow ID completer.
 */
@Service
public class WorkFlowIdCompleter extends AbstractChoicesCompleter {
    @Override
    protected List<String> choices() {
        WorkflowStore workflowStore = getService(WorkflowStore.class);
        return workflowStore.getAll().stream()
                .map(workflow -> workflow.id().toString())
                .collect(Collectors.toList());
    }
}
