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
package org.onosproject.workflow.model.accessinfo;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.NumericNode;
import com.fasterxml.jackson.databind.node.TextNode;
import com.google.common.base.MoreObjects;
import org.onlab.packet.IpAddress;
import org.onlab.packet.TpPort;
import org.onosproject.workflow.api.WorkflowException;

import java.util.Objects;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * Class for SSH access information.
 */
public final class SshAccessInfo {

    private static final String REMOTE_IP = "remoteIp";
    private static final String PORT = "port";
    private static final String USER = "user";
    private static final String PASSWORD = "password";
    private static final String KEYFILE = "keyfile";

    private final IpAddress remoteIp;
    private final TpPort port;
    private final String user;
    private String password;
    private final String privateKey;

    /**
     * Constructor for SSH access information.
     *
     * @param remoteIp ssh remote ip address
     * @param port ssh port number
     * @param user user name
     * @param password password
     * @param privateKey path of ssh private key
     */
    private SshAccessInfo(IpAddress remoteIp, TpPort port, String user, String password, String privateKey) {
        this.remoteIp = checkNotNull(remoteIp);
        this.port = checkNotNull(port);
        this.user = checkNotNull(user);
        this.password = password;
        this.privateKey = checkNotNull(privateKey);
    }

    /**
     * Builds SshAccessInfo from json.
     * @param root json root node for SshAccessinfo
     * @return SSH access information
     * @throws WorkflowException workflow exception
     */
    public static SshAccessInfo valueOf(JsonNode root) throws WorkflowException {

        JsonNode node = root.at(ptr(REMOTE_IP));
        if (node == null || !(node instanceof TextNode)) {
            throw new WorkflowException("invalid remoteIp for " + root);
        }
        IpAddress sshIp = IpAddress.valueOf(node.asText());

        node = root.at(ptr(PORT));
        if (node == null || !(node instanceof NumericNode)) {
            throw new WorkflowException("invalid port for " + root);
        }
        TpPort sshPort = TpPort.tpPort(node.asInt());

        node = root.at(ptr(USER));
        if (node == null || !(node instanceof TextNode)) {
            throw new WorkflowException("invalid user for " + root);
        }
        String sshUser = node.asText();

        node = root.at(ptr(PASSWORD));
        if (node == null || !(node instanceof TextNode)) {
            throw new WorkflowException("invalid password for " + root);
        }
        String sshPassword = node.asText();

        node = root.at(ptr(KEYFILE));
        if (node == null || !(node instanceof TextNode)) {
            throw new WorkflowException("invalid keyfile for " + root);
        }
        String sshKeyfile = node.asText();

        return new SshAccessInfo(sshIp, sshPort, sshUser, sshPassword, sshKeyfile);
    }

    private static String ptr(String field) {
        return "/" + field;
    }

    /**
     * Returns the remote IP address.
     *
     * @return ip address
     */
    public IpAddress remoteIp() {
        return this.remoteIp;
    }

    /**
     * Returns the port number.
     *
     * @return ssh port
     */
    public TpPort port() {
        return this.port;
    }

    /**
     * Returns the user name.
     *
     * @return user name
     */
    public String user() {
        return this.user;
    }

    /**
     * Returns the password.
     * @return password
     */
    public String password() {
        return this.password;
    }

    /**
     * Returns the private key path.
     *
     * @return privateKey
     */
    public String privateKey() {
        return privateKey;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof SshAccessInfo) {
            SshAccessInfo that = (SshAccessInfo) obj;
            return Objects.equals(remoteIp, that.remoteIp) &&
                    Objects.equals(port, that.port) &&
                    Objects.equals(user, that.user) &&
                    Objects.equals(privateKey, that.privateKey);
        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hash(remoteIp, port, user, privateKey);
    }

    @Override
    public String toString() {
        return MoreObjects.toStringHelper(getClass())
                .add("remoteIp", remoteIp)
                .add("port", port)
                .add("user", user)
                .add("privateKey", privateKey)
                .toString();
    }
}
