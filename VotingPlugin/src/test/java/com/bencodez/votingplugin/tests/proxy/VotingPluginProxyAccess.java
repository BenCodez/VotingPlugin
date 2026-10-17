package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.proxy.*;
import com.bencodez.votingplugin.proxy.VotingPluginProxy.*;
import java.util.Map;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import redis.clients.jedis.DefaultJedisClientConfig;

/** Typed test-only access to package-private VotingPluginProxy seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class VotingPluginProxyAccess {
    public static boolean sendProxyBroadcastEnvelopeNow(VotingPluginProxy target, String server, com.bencodez.simpleapi.servercomm.codec.JsonEnvelope envelope) { return (Boolean) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.VotingPluginProxy", target, "sendProxyBroadcastEnvelopeNow", new String[]{"String", "JsonEnvelope"}, new Object[]{server, envelope}); }
    public static VotingPluginProxyAccess.QueuedVoteResult replayQueuedVote(VotingPluginProxy target, VoteTimeQueue vote, VoteTotalsSnapshot totals, boolean realVote) { return ReflectiveAccess.enumView(VotingPluginProxyAccess.QueuedVoteResult.class, ReflectiveAccess.call("com.bencodez.votingplugin.proxy.VotingPluginProxy", target, "replayQueuedVote", new String[]{"VoteTimeQueue", "VoteTotalsSnapshot", "boolean"}, new Object[]{vote, totals, realVote})); }
    private VotingPluginProxyAccess() { }
    public static void stopSocketClients(Map<String, ClientHandler> clients) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.VotingPluginProxy", null, "stopSocketClients", new String[]{"Map"}, new Object[]{clients}); }
    public static DefaultJedisClientConfig buildRedisClientConfig(VotingPluginProxyConfig configSource) { return (DefaultJedisClientConfig) ReflectiveAccess.call("com.bencodez.votingplugin.proxy.VotingPluginProxy", null, "buildRedisClientConfig", new String[]{"VotingPluginProxyConfig"}, new Object[]{configSource}); }
    public static void acceptSharedTransportEnvelope(VotingPluginProxy target, JsonEnvelope envelope, Domain domain, String destination, java.util.function.Consumer<JsonEnvelope> accepted) { ReflectiveAccess.call("com.bencodez.votingplugin.proxy.VotingPluginProxy", target, "acceptSharedTransportEnvelope", new String[]{"JsonEnvelope", "Domain", "String", "Consumer"}, new Object[]{envelope, domain, destination, accepted}); }

    public enum QueuedVoteResult { SUCCESS, RETRY, RETRY_NONBLOCKING, TERMINAL }

}
