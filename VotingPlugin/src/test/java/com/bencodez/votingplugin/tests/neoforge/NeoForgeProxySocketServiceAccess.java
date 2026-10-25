package com.bencodez.votingplugin.tests.neoforge;

import com.bencodez.votingplugin.tests.support.ReflectiveAccess;
import com.bencodez.votingplugin.neoforge.*;
import com.bencodez.votingplugin.neoforge.NeoForgeProxySocketService.*;
import java.nio.file.Path;
import java.util.function.Consumer;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Typed test-only access to package-private NeoForgeProxySocketService seams. */
@SuppressWarnings({"unchecked", "rawtypes"})
public final class NeoForgeProxySocketServiceAccess {
    private NeoForgeProxySocketServiceAccess() { }

    public static NeoForgeProxySocketService create(NeoForgeProxySocketConfigurationAccess configuration, NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players, Consumer<JsonEnvelope> outbound, SharedTransportEnvelopeAuthenticator authenticator) { return (NeoForgeProxySocketService) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketService", new String[]{"NeoForgeProxySocketConfiguration", "NeoForgeVoteProcessor", "NeoForgePlayerDirectory", "Consumer", "SharedTransportEnvelopeAuthenticator"}, new Object[]{configuration, processor, players, outbound, authenticator}); }
    public static NeoForgeProxySocketService create(NeoForgeProxySocketConfigurationAccess configuration, NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players, Consumer<JsonEnvelope> outbound, SharedTransportEnvelopeAuthenticator authenticator, TransportEnvelopeEncryption encryption, Runnable transportClose, boolean scheduleHeartbeat) { return (NeoForgeProxySocketService) ReflectiveAccess.construct("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketService", new String[]{"NeoForgeProxySocketConfiguration", "NeoForgeVoteProcessor", "NeoForgePlayerDirectory", "Consumer", "SharedTransportEnvelopeAuthenticator", "TransportEnvelopeEncryption", "Runnable", "boolean"}, new Object[]{configuration, processor, players, outbound, authenticator, encryption, transportClose, scheduleHeartbeat}); }
    public static void receive(NeoForgeProxySocketService target, JsonEnvelope envelope) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketService", target, "receive", new String[]{"JsonEnvelope"}, new Object[]{envelope}); }
    public static void closeTransport(SocketHandler listener, ClientHandler client) { ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxySocketService", null, "closeTransport", new String[]{"SocketHandler", "ClientHandler"}, new Object[]{listener, client}); }
}
