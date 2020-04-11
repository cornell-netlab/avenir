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
package org.onosproject.netconf.cli.impl;

import org.apache.karaf.shell.api.action.Argument;
import org.apache.karaf.shell.api.action.Command;
import org.apache.karaf.shell.api.action.Completion;
import org.apache.karaf.shell.api.action.lifecycle.Service;
import org.onosproject.cli.AbstractShellCommand;
import org.onosproject.cli.net.DeviceIdCompleter;
import org.onosproject.net.DeviceId;
import org.onosproject.net.behaviour.ConfigSetter;
import org.onosproject.net.driver.DriverHandler;
import org.onosproject.net.driver.DriverService;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Command that sets the configuration included in the specified file to the
 * specified device. It prints the response of the device.
 *
 * This is a temporary development tool for use until yang integration is complete.
 * This uses a not properly specified behavior. DO NOT USE AS AN EXAMPLE.
 */
//Temporary Developer tool, NOT TO BE USED in production or as example for
// future commands.
//FIXME Remove dependency to ConfigSetter.
@Service
@Command(scope = "onos", name = "netconf-rpc-test",
        description = "Debug tool to send NETCONF RPC request")
public class NetconfRpcTestCommand extends AbstractShellCommand {

    @Argument(index = 0, name = "uri", description = "Device ID",
            required = true, multiValued = false)
    private String uri = null;

    @Argument(index = 1, name = "cfgFile", description = "File path to RPC XML",
            required = true, multiValued = false)
    @Completion(DeviceIdCompleter.class)
    private String cfgFile = null;

    private DeviceId deviceId;

    @Override
    protected void doExecute() {
        DriverService service = get(DriverService.class);
        deviceId = DeviceId.deviceId(uri);
        DriverHandler h = service.createHandler(deviceId);
        ConfigSetter config = h.behaviour(ConfigSetter.class);
        checkNotNull(cfgFile, "Configuration file cannot be null");
        print(config.setConfiguration(cfgFile));
    }

}
