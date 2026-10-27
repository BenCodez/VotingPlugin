package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Base64;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.neoforge.NeoForgeProxyTransport;
import com.bencodez.votingplugin.neoforge.NeoForgeRuntime;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

class NeoForgeProxyRedisServiceTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeProxyRedisService";
    private static final String INBOUND = "fixture/VotingPlugin_backend-test";
    private static final String OUTBOUND = "fixture/VotingPlugin";
    @TempDir Path directory;
    private SharedTransportEnvelopeAuthenticator proxy;
    private com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature origin;

    @BeforeEach void configure() throws IOException {
        origin = NeoForgeProxyOriginFixture.create(directory);
        byte[] independentKey = new byte[32];
        new java.security.SecureRandom().nextBytes(independentKey);
        Files.writeString(directory.resolve("secretkey.key"), Base64.getEncoder().encodeToString(independentKey));
        proxy = SharedTransportEnvelopeAuthenticator.load(directory.resolve("secretkey.key"), Mode.REQUIRED);
        Files.writeString(directory.resolve("Config.yml"), "DataStorage: SQLITE\nAllowUnjoined: true\n");
        Files.writeString(directory.resolve("VoteSites.yml"), """
                VoteSites:
                  Site:
                    Enabled: true
                    ServiceSite: Service
                    VoteDelay: 24
                    WaitUntilVoteDelay: false
                    GiveOfflineRewards: true
                """);
    }

    @Test void authenticatedBrokerCallbackRetainsBeforeAckAndDuplicatesSurviveRestart() throws Exception {
        UUID player = UUID.randomUUID();
        UUID id = UUID.randomUUID();
        for (int generation = 0; generation < 2; generation++) {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
                Connection connection = new Connection();
                try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED)) {
                    assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                    connection.sent.clear();
                    for (int duplicate = 0; duplicate < 2; duplicate++) {
                        deliver(connection, signed(vote(id, player), Domain.REDIS_PROXY_BACKEND,
                                "proxy-test", INBOUND));
                    }
                    assertEquals(1, runtime.deferredVotes().pending(player).size());
                    assertEquals(id, runtime.deferredVotes().pending(player).getFirst().voteId());
                    assertEquals(2, connection.sent.size());
                    for (JsonEnvelope ack : connection.sent) {
                        var verified = proxy.verify(ack, Domain.REDIS_PROXY_BACKEND, OUTBOUND);
                        assertTrue(verified.accepted());
                        UUID.fromString(verified.envelope().getFields().get(VotingPluginWire.K_REDIS_DELIVERY_ID));
                        assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_ACK, verified.envelope().getSubChannel());
                    }
                }
                assertTrue(connection.closed.await(3, TimeUnit.SECONDS));
                assertEquals(1, connection.closes.get());
            }
        }
    }

    @Test void wrongTopicDomainSenderUnsignedAndReplayedWrappersCannotCreateAnotherOccurrence() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED)) {
                assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                connection.sent.clear();
                UUID player = UUID.randomUUID();
                JsonEnvelope raw = vote(UUID.randomUUID(), player);
                deliver(connection, raw);
                deliver(connection, signed(raw, Domain.MQTT_PROXY_BACKEND, "proxy-test", INBOUND));
                deliver(connection, signed(raw, Domain.REDIS_PROXY_BACKEND, "other-peer", INBOUND));
                deliver(connection, signed(raw, Domain.REDIS_PROXY_BACKEND, "proxy-test", "other/topic"));
                byte[] good = encode(signed(raw, Domain.REDIS_PROXY_BACKEND, "proxy-test", INBOUND));
                connection.receiver.accept("other/topic", good);
                connection.receiver.accept(INBOUND, new byte[] {(byte) 0xff});
                connection.receiver.accept(INBOUND, new byte[256 * 1024 + 1]);
                assertTrue(runtime.deferredVotes().pending(player).isEmpty());
                assertTrue(connection.sent.isEmpty());
                connection.receiver.accept(INBOUND, good);
                connection.receiver.accept(INBOUND, good);
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, connection.sent.size());
                service.close();
                connection.sent.clear();
                deliver(connection, signed(vote(UUID.randomUUID(), player), Domain.REDIS_PROXY_BACKEND,
                        "proxy-test", INBOUND));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertTrue(connection.sent.isEmpty());
            }
        }
    }

    @Test void sharedKeyHolderCannotMintProxyOriginOrRedirectItsSignedPayload() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED)) {
                assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                connection.sent.clear();
                UUID player = UUID.randomUUID();
                JsonEnvelope raw = vote(UUID.randomUUID(), player);
                var originDomain = com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.REDIS;
                for (JsonEnvelope forged : List.of(raw,
                        origin.sign(raw, originDomain, "proxy-test", "other-backend"),
                        origin.sign(raw, originDomain, "other-proxy", "backend-test"),
                        origin.sign(raw, com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.MYSQL,
                                "proxy-test", "backend-test"))) {
                    // Attacker possesses the real network membership key, but not proxy signing authority.
                    deliver(connection, proxy.sign(forged, Domain.REDIS_PROXY_BACKEND, "proxy-test", INBOUND));
                    assertTrue(runtime.deferredVotes().pending(player).isEmpty());
                    assertTrue(connection.sent.isEmpty());
                }
                deliver(connection, signed(raw, Domain.REDIS_PROXY_BACKEND, "proxy-test", INBOUND));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, connection.sent.size());
            }
        }
    }

    @Test void compatibilityIsExplicitAndDoesNotAdvertiseSocketOrDelayRejectionSupport() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.COMPATIBILITY)) {
                assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                for (JsonEnvelope announced : connection.sent) {
                    assertFalse(VotingPluginWire.advertisesAuthenticatedSocketVoteDelivery(announced));
                    assertFalse(VotingPluginWire.advertisesVoteDelayRejectionAcknowledgement(announced));
                    assertTrue(VotingPluginWire.advertisesVoteDeliveryAcknowledgement(announced));
                }
                connection.sent.clear();
                UUID player = UUID.randomUUID();
                deliver(connection, origin.sign(vote(UUID.randomUUID(), player),
                        com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.REDIS, "proxy-test", "backend-test"));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, connection.sent.size());
            }
        }
    }

    @Test void capacityRefusalNeverAcknowledgesAnUnretainedVote() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED)) {
                assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                UUID player = UUID.randomUUID();
                for (int index = 0; index < NeoForgeDeferredVoteStoreAccess.MAX_DEFERRED_PER_USER; index++) {
                    deliver(connection, signed(vote(UUID.randomUUID(), player), Domain.REDIS_PROXY_BACKEND,
                            "proxy-test", INBOUND));
                }
                connection.sent.clear();
                deliver(connection, signed(vote(UUID.randomUUID(), player), Domain.REDIS_PROXY_BACKEND,
                        "proxy-test", INBOUND));
                assertTrue(connection.sent.isEmpty());
                assertEquals(NeoForgeDeferredVoteStoreAccess.MAX_DEFERRED_PER_USER,
                        runtime.deferredVotes().pending(player).size());
            }
        }
    }

    @Test void startupConnectionFailureRetriesWithoutBlockingTheCaller() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            AtomicInteger attempts = new AtomicInteger();
            Object factory = new Object() {
                public Connection open() throws IOException {
                    if (attempts.incrementAndGet() == 1) throw new IOException("synthetic unavailable broker");
                    return connection;
                }
            };
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED, factory)) {
                assertTrue(connection.ready.await(4, TimeUnit.SECONDS));
                assertEquals(2, attempts.get());
            }
            assertTrue(connection.closed.await(3, TimeUnit.SECONDS));
            assertEquals(1, connection.closes.get());
        }
    }

    @Test void shutdownWhileConnectingClosesLateClientWithoutSubscribing() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            CountDownLatch entered = new CountDownLatch(1);
            CountDownLatch release = new CountDownLatch(1);
            Object factory = new Object() {
                public Connection open() throws InterruptedException {
                    entered.countDown();
                    assertTrue(release.await(4, TimeUnit.SECONDS));
                    return connection;
                }
            };
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED, factory)) {
                assertTrue(entered.await(3, TimeUnit.SECONDS));
                try { service.close(); }
                finally { release.countDown(); }
                assertTrue(connection.closed.await(3, TimeUnit.SECONDS));
                assertEquals(1, connection.closes.get());
                assertEquals(null, connection.receiver);
                assertTrue(connection.sent.isEmpty());
            }
        }
    }

    @Test void failedAckPublishKeepsDurableOccurrenceForSameIdRetry() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Connection connection = new Connection();
            try (NeoForgeProxyTransport service = create(runtime, connection, Mode.REQUIRED)) {
                assertTrue(connection.ready.await(3, TimeUnit.SECONDS));
                connection.sent.clear();
                connection.failPublishes = true;
                UUID player = UUID.randomUUID();
                JsonEnvelope raw = vote(UUID.randomUUID(), player);
                deliver(connection, signed(raw, Domain.REDIS_PROXY_BACKEND, "proxy-test", INBOUND));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertTrue(connection.sent.isEmpty());
                connection.failPublishes = false;
                deliver(connection, signed(raw, Domain.REDIS_PROXY_BACKEND, "proxy-test", INBOUND));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, connection.sent.size());
            }
        }
    }

    @Test void nativeRuntimeSelectsRedisAndClosesItsActualAdapterWithoutSocketStartup() throws Exception {
        Files.writeString(directory.resolve("BungeeSettings.yml"), "UseBungeecord: true\nBungeeMethod: REDIS\n"
                + "Server: backend-test\n" + NeoForgeProxyOriginFixture.settings() + "Redis: {Host: redis.example}\n");
        Class<Object> type = ReflectiveAccess.type(OWNER);
        NeoForgeProxyTransport adapter = (NeoForgeProxyTransport) org.mockito.Mockito.mock(type);
        try (var mockedFactory = org.mockito.Mockito.mockStatic(type, invocation -> {
            assertEquals("start", invocation.getMethod().getName());
            return adapter;
        })) {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory, new Object())) {
                assertEquals(adapter, runtime.proxyTransport().orElseThrow());
                assertTrue(runtime.proxySocket().isEmpty());
            }
            org.mockito.Mockito.verify(adapter).close();
        }
    }

    private NeoForgeProxyTransport create(NeoForgeRuntime runtime, Connection connection, Mode mode) throws IOException {
        return create(runtime, connection, mode, new Factory(connection));
    }

    private NeoForgeProxyTransport create(NeoForgeRuntime runtime, Connection connection, Mode mode,
            Object factory) throws IOException {
        Path file = directory.resolve("redis-test-settings.yml");
        Files.writeString(file, "UseBungeecord: true\nBungeeMethod: REDIS\nServer: backend-test\n"
                + NeoForgeProxyOriginFixture.settings() + "SharedTransportAuthentication: " + mode
                + "\nRedis: {Host: redis.example, Prefix: 'fixture/'}\n");
        Object config = ReflectiveAccess.call("com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration",
                null, "load", new String[] {"ConfigurationNode", "ConfigurationNode"},
                new Object[] {YamlConfigurationLoader.builder().path(file).build().load(), runtime.config()});
        return (NeoForgeProxyTransport) ReflectiveAccess.construct(OWNER,
                new String[] {"NeoForgeProxyTransportConfiguration", "NeoForgeVoteProcessor", "NeoForgePlayerDirectory",
                        "SharedTransportEnvelopeAuthenticator", "TransportEnvelopeEncryption", "ProxyOriginEnvelopeSignature", "ConnectionFactory", "boolean"},
                new Object[] {config, runtime.voteProcessor(), runtime.players(),
                        SharedTransportEnvelopeAuthenticator.load(directory.resolve("secretkey.key"), mode),
                        TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND),
                        NeoForgeProxyOriginFixture.verifier(directory), factory, false});
    }

    private JsonEnvelope signed(JsonEnvelope envelope, Domain domain, String sender, String recipient) {
        var proof = origin.sign(envelope, com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.REDIS,
                "proxy-test", "backend-test");
        return proxy.sign(proof, domain, sender, recipient);
    }

    private static void deliver(Connection connection, JsonEnvelope envelope) {
        connection.receiver.accept(INBOUND, encode(envelope));
    }
    private static byte[] encode(JsonEnvelope envelope) {
        return JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8);
    }
    private static JsonEnvelope vote(UUID id, UUID player) {
        return VotingPluginWire.requestVoteDeliveryAcknowledgement(VotingPluginWire.vote("Alex", player.toString(),
                "Service", 100L, true, true, "", id, false, false, 1, 1));
    }

    public static final class Factory {
        private final Connection connection;
        Factory(Connection connection) { this.connection = connection; }
        public Connection open() { return connection; }
    }
    public static final class Connection {
        final List<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
        final CountDownLatch ready = new CountDownLatch(1);
        final CountDownLatch closed = new CountDownLatch(1);
        final AtomicInteger closes = new AtomicInteger();
        volatile BiConsumer<String, byte[]> receiver;
        volatile boolean failPublishes;
        public void subscribe(String topic, BiConsumer<String, byte[]> callback) {
            assertEquals(INBOUND, topic);
            receiver = callback;
        }
        public void publish(String topic, JsonEnvelope envelope) {
            assertEquals(OUTBOUND, topic);
            if (failPublishes) throw new IllegalStateException("synthetic publication failure");
            sent.add(envelope);
            if (envelope.getSubChannel().equals(VotingPluginWire.SUB_BACKEND_HEARTBEAT)) ready.countDown();
        }
        public void close() { closes.incrementAndGet(); closed.countDown(); }
    }
}
