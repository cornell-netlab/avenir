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
package org.onosproject.lisp.ctl.impl;

import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelPipeline;
import io.netty.channel.socket.nio.NioDatagramChannel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Creates a ChannelInitializer for a server-side LISP channel.
 */
public final class LispChannelInitializer extends ChannelInitializer<NioDatagramChannel> {

    private final Logger log = LoggerFactory.getLogger(getClass());
    private static final String LISP_MESSAGE_DECODER = "lispmessagedecoder";
    private static final String LISP_MESSAGE_ENCODER = "lispmessageencoder";
    private static final String LISP_CHANNEL_HANDLER = "handler";

    @Override
    protected void initChannel(NioDatagramChannel channel) throws Exception {
        ChannelPipeline pipeline = channel.pipeline();

        LispChannelHandler handler = new LispChannelHandler();

        pipeline.addLast(LISP_MESSAGE_DECODER, new LispMessageDecoder());
        pipeline.addLast(LISP_MESSAGE_ENCODER, new LispMessageEncoder());
        pipeline.addLast(LISP_CHANNEL_HANDLER, handler);
    }
}
