package com.bencodez.votingplugin.tests.backendproxy.transport;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.backendproxy.transport.*;
import com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport.*;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import redis.clients.jedis.DefaultJedisClientConfig;

/** Typed test-only access to package-private RedisBackendProxyTransport seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class RedisBackendProxyTransportAccess {
    public static final int MAX_LEGACY_HANDOFF_DELIVERIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", "MAX_LEGACY_HANDOFF_DELIVERIES");
    public static final int MAX_IDENTIFIED_HANDOFF_DELIVERIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", "MAX_IDENTIFIED_HANDOFF_DELIVERIES");
    public static final int MAX_REPLAY_HANDOFF_DELIVERIES = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", "MAX_REPLAY_HANDOFF_DELIVERIES");
    public static final int REPLAY_BATCH_SIZE = (Integer) ReflectiveAccess.field("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", "REPLAY_BATCH_SIZE");
    private RedisBackendProxyTransportAccess() { }
    public static void updateSecurity(RedisBackendProxyTransport target, SharedTransportEnvelopeAuthenticator replacementAuthenticator, TransportEnvelopeEncryption replacementEncryption) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "updateSecurity", new String[]{"SharedTransportEnvelopeAuthenticator", "TransportEnvelopeEncryption"}, new Object[]{replacementAuthenticator, replacementEncryption}); }
    public static void acceptAuthenticatedEnvelope(RedisBackendProxyTransport target, JsonEnvelope envelope, String channel) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "acceptAuthenticatedEnvelope", new String[]{"JsonEnvelope", "String"}, new Object[]{envelope, channel}); }
    public static void dispatchReceivedSubscriberEnvelope(RedisBackendProxyTransport target, JsonEnvelope envelope, String deliveryId) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "dispatchReceivedSubscriberEnvelope", new String[]{"JsonEnvelope", "String"}, new Object[]{envelope, deliveryId}); }
    public static void dispatchIdentified(RedisBackendProxyTransport target, JsonEnvelope envelope, String deliveryId) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "dispatchIdentified", new String[]{"JsonEnvelope", "String"}, new Object[]{envelope, deliveryId}); }
    public static void dispatchLegacy(RedisBackendProxyTransport target, JsonEnvelope envelope) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "dispatchLegacy", new String[]{"JsonEnvelope"}, new Object[]{envelope}); }
    public static void fenceAfterHandoff(RedisBackendProxyTransport target) { ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", target, "fenceAfterHandoff", new String[]{}, new Object[]{}); }
    public static DefaultJedisClientConfig buildValidationClientConfig(int database, String username, String password, boolean ssl) { return (DefaultJedisClientConfig) ReflectiveAccess.call("com.bencodez.votingplugin.backendproxy.transport.RedisBackendProxyTransport", null, "buildValidationClientConfig", new String[]{"int", "String", "String", "boolean"}, new Object[]{database, username, password, ssl}); }
}
