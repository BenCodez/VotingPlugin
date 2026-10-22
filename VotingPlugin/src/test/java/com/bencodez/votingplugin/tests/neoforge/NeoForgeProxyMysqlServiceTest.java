package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.SQLException;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelopeCodec;
import com.bencodez.votingplugin.neoforge.NeoForgeProxyTransport;
import com.bencodez.votingplugin.neoforge.NeoForgeRuntime;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;
import com.bencodez.votingplugin.tests.support.ReflectiveAccess;

class NeoForgeProxyMysqlServiceTest {
    private static final String OWNER = "com.bencodez.votingplugin.neoforge.NeoForgeProxyMysqlService";
    @TempDir Path directory;

    @BeforeEach void configure() throws IOException {
        Files.writeString(directory.resolve("Config.yml"), "DataStorage: SQLITE\nAllowUnjoined: true\n"
                + "Database: {Host: db.example, Database: fixture, Username: fixture-user}\n");
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

    @Test void completeQueueEnvelopeReturnsAcceptanceOnlyAfterDurableRetentionAndSurvivesRestart() throws Exception {
        UUID player = UUID.randomUUID();
        UUID id = UUID.randomUUID();
        for (int generation = 0; generation < 2; generation++) {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
                Queue queue = new Queue();
                try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue))) {
                    assertTrue(queue.ready.await(3, TimeUnit.SECONDS));
                    queue.sent.clear();
                    for (int duplicate = 0; duplicate < 2; duplicate++) {
                        assertTrue(receive(service, "proxy", "backend-test", encode(vote(id, player))));
                    }
                    assertEquals(1, runtime.deferredVotes().pending(player).size());
                    assertEquals(id, runtime.deferredVotes().pending(player).getFirst().voteId());
                    assertEquals(2, queue.sent.size());
                    assertTrue(queue.sent.stream().allMatch(ack ->
                            VotingPluginWire.SUB_VOTE_DELIVERY_ACK.equals(ack.getSubChannel())));
                }
                assertTrue(queue.closed.await(3, TimeUnit.SECONDS));
                assertEquals(1, queue.closes.get());
            }
        }
    }

    @Test void wrongSqlRoleDestinationSchemaMalformedAndAdjacentDataCannotAdmitVotes() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue))) {
                assertTrue(queue.ready.await(3, TimeUnit.SECONDS));
                queue.sent.clear();
                UUID player = UUID.randomUUID();
                byte[] raw = encode(vote(UUID.randomUUID(), player));
                assertFalse(receive(service, "backend-peer", "backend-test", raw));
                assertFalse(receive(service, "proxy", "other-backend", raw));
                assertFalse(receive(service, "proxy", "backend-test", new byte[] {(byte) 0xff}));
                assertFalse(receive(service, "proxy", "backend-test", new byte[262145]));
                assertFalse(receive(service, "proxy", "backend-test", (new String(raw, StandardCharsets.UTF_8)
                        + " {}").getBytes(StandardCharsets.UTF_8)));
                assertFalse(receive(service, "proxy", "backend-test", encode(
                        JsonEnvelope.builder("fixture-unsupported").put("name", "雪").build())));
                assertTrue(runtime.deferredVotes().pending(player).isEmpty());
                assertTrue(queue.sent.isEmpty());
                assertTrue(receive(service, "proxy", "backend-test", raw));
                service.close();
                assertFalse(receive(service, "proxy", "backend-test", encode(vote(UUID.randomUUID(), player))));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
            }
        }
    }

    @Test void capacityRefusalReturnsFalseAndDoesNotAcknowledgeOrDeleteTheSqlOccurrence() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue))) {
                assertTrue(queue.ready.await(3, TimeUnit.SECONDS));
                UUID player = UUID.randomUUID();
                for (int index = 0; index < NeoForgeDeferredVoteStoreAccess.MAX_DEFERRED_PER_USER; index++) {
                    assertTrue(receive(service, "proxy", "backend-test", encode(vote(UUID.randomUUID(), player))));
                }
                queue.sent.clear();
                assertFalse(receive(service, "proxy", "backend-test", encode(vote(UUID.randomUUID(), player))));
                assertTrue(queue.sent.isEmpty());
                assertEquals(NeoForgeDeferredVoteStoreAccess.MAX_DEFERRED_PER_USER,
                        runtime.deferredVotes().pending(player).size());
            }
        }
    }

    @Test void failedAckLeavesTheSameDurableIdRetryable() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue))) {
                assertTrue(queue.ready.await(3, TimeUnit.SECONDS));
                queue.sent.clear();
                queue.failPublishes = true;
                UUID player = UUID.randomUUID();
                UUID id = UUID.randomUUID();
                byte[] raw = encode(vote(id, player));
                assertFalse(receive(service, "proxy", "backend-test", raw));
                assertEquals(id, runtime.deferredVotes().pending(player).getFirst().voteId());
                assertTrue(queue.sent.isEmpty());
                queue.failPublishes = false;
                assertTrue(receive(service, "proxy", "backend-test", raw));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, queue.sent.size());
            }
        }
    }

    @Test void startupRetriesAndPublishesOnlySupportedDurabilityCapabilities() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            AtomicInteger attempts = new AtomicInteger();
            Object factory = new Object() {
                public Queue open() throws SQLException {
                    if (attempts.incrementAndGet() == 1) throw new SQLException("synthetic unavailable database");
                    return queue;
                }
            };
            try (NeoForgeProxyTransport service = create(runtime, queue, factory)) {
                assertTrue(queue.ready.await(4, TimeUnit.SECONDS));
                assertEquals(2, attempts.get());
                for (JsonEnvelope announced : queue.sent) {
                    assertTrue(VotingPluginWire.advertisesVoteDeliveryAcknowledgement(announced));
                    assertFalse(VotingPluginWire.advertisesAuthenticatedSocketVoteDelivery(announced));
                    assertFalse(VotingPluginWire.advertisesVoteDelayRejectionAcknowledgement(announced));
                }
            }
            assertTrue(queue.closed.await(3, TimeUnit.SECONDS));
            assertEquals(1, queue.closes.get());
        }
    }

    @Test void shutdownDuringFactoryAdmissionClosesLateResourcesWithoutPolling() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            CountDownLatch entered = new CountDownLatch(1);
            CountDownLatch release = new CountDownLatch(1);
            Object factory = new Object() {
                public Queue open() throws InterruptedException {
                    entered.countDown();
                    assertTrue(release.await(4, TimeUnit.SECONDS));
                    return queue;
                }
            };
            try (NeoForgeProxyTransport service = create(runtime, queue, factory)) {
                assertTrue(entered.await(3, TimeUnit.SECONDS));
                try { service.close(); }
                finally { release.countDown(); }
                assertTrue(queue.closed.await(3, TimeUnit.SECONDS));
                assertEquals(1, queue.closes.get());
                assertEquals(0, queue.polls.get());
                assertTrue(queue.sent.isEmpty());
            }
        }
    }

    @Test void nativeRuntimeSelectsMysqlWithoutChangingSqliteUserStorage() throws Exception {
        Files.writeString(directory.resolve("BungeeSettings.yml"),
                "UseBungeecord: true\nBungeeMethod: MYSQL\nServer: backend-test\n");
        Class<Object> type = ReflectiveAccess.type(OWNER);
        NeoForgeProxyTransport adapter = (NeoForgeProxyTransport) org.mockito.Mockito.mock(type);
        try (var mocked = org.mockito.Mockito.mockStatic(type, invocation -> {
            assertEquals("start", invocation.getMethod().getName());
            return adapter;
        })) {
            try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory, new Object())) {
                assertEquals(adapter, runtime.proxyTransport().orElseThrow());
                assertTrue(runtime.proxySocket().isEmpty());
                assertEquals("SQLITE", runtime.config().node("DataStorage").getString());
                assertTrue(Files.isRegularFile(directory.resolve("VotingPlugin.db")));
            }
            org.mockito.Mockito.verify(adapter).close();
        }
    }

    private NeoForgeProxyTransport create(NeoForgeRuntime runtime, Queue queue, Object factory) throws IOException {
        Path file = directory.resolve("mysql-test-settings.yml");
        Files.writeString(file, "UseBungeecord: true\nBungeeMethod: MYSQL\nServer: backend-test\n");
        Object configuration = ReflectiveAccess.call(
                "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration", null, "load",
                new String[] {"ConfigurationNode", "ConfigurationNode"},
                new Object[] {YamlConfigurationLoader.builder().path(file).build().load(), runtime.config()});
        return (NeoForgeProxyTransport) ReflectiveAccess.construct(OWNER,
                new String[] {"NeoForgeProxyTransportConfiguration", "NeoForgeVoteProcessor", "NeoForgePlayerDirectory",
                        "TransportEnvelopeEncryption", "QueueFactory", "boolean"},
                new Object[] {configuration, runtime.voteProcessor(), runtime.players(),
                        TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND), factory, false});
    }

    private static boolean receive(NeoForgeProxyTransport service, String source, String destination, byte[] payload) {
        return (boolean) ReflectiveAccess.call(OWNER, service, "receive",
                new String[] {"String", "String", "byte[]"}, new Object[] {source, destination, payload});
    }
    private static byte[] encode(JsonEnvelope envelope) { return JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8); }
    private static JsonEnvelope vote(UUID id, UUID player) {
        return VotingPluginWire.requestVoteDeliveryAcknowledgement(VotingPluginWire.vote("Alex", player.toString(),
                "Service", 100L, true, true, "", id, false, false, 1, 1));
    }
    public static final class Factory {
        private final Queue queue;
        Factory(Queue queue) { this.queue = queue; }
        public Queue open() { return queue; }
    }
    public static final class Queue {
        final List<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
        final CountDownLatch ready = new CountDownLatch(1);
        final CountDownLatch closed = new CountDownLatch(1);
        final AtomicInteger closes = new AtomicInteger();
        final AtomicInteger polls = new AtomicInteger();
        volatile boolean failPublishes;
        public void poll(Object receiver) { polls.incrementAndGet(); }
        public void publish(JsonEnvelope envelope) {
            if (failPublishes) throw new IllegalStateException("synthetic publication failure");
            sent.add(envelope);
            if (VotingPluginWire.SUB_BACKEND_HEARTBEAT.equals(envelope.getSubChannel())) ready.countDown();
        }
        public void close() { closes.incrementAndGet(); closed.countDown(); }
    }
}
