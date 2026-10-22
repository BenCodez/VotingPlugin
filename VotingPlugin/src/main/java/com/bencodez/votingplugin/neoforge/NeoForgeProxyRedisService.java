package com.bencodez.votingplugin.neoforge;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import com.bencodez.votingplugin.core.vote.SharedVoteIdentity;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

/** Redis adapter for the existing proxy/backend channel and signed envelope protocol. */
final class NeoForgeProxyRedisService implements NeoForgeProxyTransport {
    interface Connection extends NeoForgeProxyBrokerService.Connection { }
    interface ConnectionFactory extends NeoForgeProxyBrokerService.ConnectionFactory {
        @Override Connection open() throws Exception;
    }
    private final NeoForgeProxyBrokerService broker;

    static NeoForgeProxyRedisService start(Path directory, NeoForgeProxyTransportConfiguration configuration,
            NeoForgeVoteProcessor processor, NeoForgePlayerDirectory players) throws IOException {
        var authenticator = SharedTransportEnvelopeAuthenticator.load(directory.resolve("secretkey.key"),
                configuration.sharedAuthentication());
        var encryption = TransportEnvelopeEncryption.load(directory.resolve("secretkey.key"),
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, configuration.communicationEncryption());
        AtomicReference<Runnable> reconnected = new AtomicReference<>(() -> { });
        var service = new NeoForgeProxyRedisService(configuration, processor, players, authenticator, encryption,
                () -> new NeoForgeRedisConnection(configuration.redis(), () -> reconnected.get().run()), true);
        reconnected.set(service.broker::transportReady);
        return service;
    }

    NeoForgeProxyRedisService(NeoForgeProxyTransportConfiguration configuration, NeoForgeVoteProcessor processor,
            NeoForgePlayerDirectory players, SharedTransportEnvelopeAuthenticator authenticator,
            TransportEnvelopeEncryption encryption, ConnectionFactory factory, boolean scheduleHeartbeat) {
        Objects.requireNonNull(configuration, "configuration");
        if (configuration.method() != NeoForgeProxyTransportConfiguration.Method.REDIS) {
            throw new IllegalArgumentException("Redis settings required");
        }
        broker = new NeoForgeProxyBrokerService(configuration, processor, players,
                authenticator, encryption, factory, scheduleHeartbeat);
    }

    void receive(String channel, byte[] payload) { broker.receive(channel, payload); }
    @Override public void playerOnline(SharedVoteIdentity identity) { broker.playerOnline(identity); }
    @Override public void playerOffline(SharedVoteIdentity identity) { broker.playerOffline(identity); }
    @Override public void close() { broker.close(); }
}
