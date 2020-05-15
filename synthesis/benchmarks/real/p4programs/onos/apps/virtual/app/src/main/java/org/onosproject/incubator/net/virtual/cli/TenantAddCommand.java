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

package org.onosproject.incubator.net.virtual.cli;

import org.apache.karaf.shell.api.action.Argument;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.net.TenantId;
import org.onosproject.incubator.net.virtual.VirtualNetworkAdminService;

/**
 * Creates a new virtual network tenant.
 */
@Service
@Command(scope = "onos", name = "vnet-add-tenant",
        description = "Creates a new virtual network tenant.")

public class TenantAddCommand extends AbstractShellCommand {

    @Argument(index = 0, name = "id", description = "Tenant ID",
            required = true, multiValued = false)
    String id = null;

    @Override
    protected void doExecute() {
        VirtualNetworkAdminService service = get(VirtualNetworkAdminService.class);
        service.registerTenantId(TenantId.tenantId(id));
        print("Tenant successfully added.");
    }
}
