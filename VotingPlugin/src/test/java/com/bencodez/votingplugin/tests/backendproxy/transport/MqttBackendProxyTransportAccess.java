package com.bencodez.votingplugin.tests.backendproxy.transport;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.transport.*;
import com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport.*;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Typed test-only access to package-private MqttBackendProxyTransport seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class MqttBackendProxyTransportAccess {
    private MqttBackendProxyTransportAccess() { }
    public static void updateSecurity(MqttBackendProxyTransport target, SharedTransportEnvelopeAuthenticator replacementAuthenticator, TransportEnvelopeEncryption replacementEncryption) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport", target, "updateSecurity", new String[]{"SharedTransportEnvelopeAuthenticator", "TransportEnvelopeEncryption"}, new Object[]{replacementAuthenticator, replacementEncryption}); }
    public static void acceptAuthenticatedEnvelope(MqttBackendProxyTransport target, JsonEnvelope envelope, String topic) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.MqttBackendProxyTransport", target, "acceptAuthenticatedEnvelope", new String[]{"JsonEnvelope", "String"}, new Object[]{envelope, topic}); }
}
