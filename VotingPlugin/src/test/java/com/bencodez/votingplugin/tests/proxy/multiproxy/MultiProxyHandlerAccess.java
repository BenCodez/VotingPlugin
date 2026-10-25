package com.bencodez.votingplugin.tests.proxy.multiproxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.multiproxy.*;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler.*;
import java.util.Map;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;

/** Typed test-only access to package-private MultiProxyHandler seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class MultiProxyHandlerAccess {
    public static long unsignedBridgeNowNanos(MultiProxyHandler target) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", target, "unsignedBridgeNowNanos", new String[]{}, new Object[]{}); }
    public static final long VOTE_CAPABILITY_LEASE_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", "VOTE_CAPABILITY_LEASE_MILLIS");
    public static final long VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", "VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS");
    public static final long VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS = (Long) ReflectiveAccess.field("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", "VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS");
    private MultiProxyHandlerAccess() { }
    public static long capabilityNowMillis(MultiProxyHandler target) { return (Long) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", target, "capabilityNowMillis", new String[]{}, new Object[]{}); }
    public static void restoreVoteCapabilityPeers(MultiProxyHandler target) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", target, "restoreVoteCapabilityPeers", new String[]{}, new Object[]{}); }
    public static void acceptRedisEnvelope(MultiProxyHandler target, JsonEnvelope envelope) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", target, "acceptRedisEnvelope", new String[]{"JsonEnvelope"}, new Object[]{envelope}); }
    public static void acceptRedisEnvelope(MultiProxyHandler target, JsonEnvelope envelope, String channel) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", target, "acceptRedisEnvelope", new String[]{"JsonEnvelope", "String"}, new Object[]{envelope, channel}); }

    public static void stopSocketClients(Map<String, ClientHandler> clients) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler", null, "stopSocketClients", new String[]{"Map"}, new Object[]{clients}); }
}
