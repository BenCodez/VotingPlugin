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
    private com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature origin;

    @BeforeEach void configure() throws IOException {
        origin = NeoForgeProxyOriginFixture.create(directory);
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

    @Test void sqlWriterCannotForgeProxyRoleForAnotherBackend() throws Exception {
        assertProxyOnlyRoleRequired(false);
    }

    @Test void sharedNetworkEncryptionKeyDoesNotGrantProxyVoteOrigin() throws Exception {
        assertProxyOnlyRoleRequired(true);
    }

    @Test void actualProxyPublisherThroughCompleteQueuePayloadReachesDurableConsumerExactlyOnce() throws Exception {
        var producer = new com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl();
        producer.setDataFolder(directory.toFile());
        producer.setMethod(com.bencodez.votingplugin.proxy.BungeeMethod.MYSQL);
        org.mockito.Mockito.when(producer.getConfig().getSpigotServers()).thenReturn(List.of("backend-test"));
        org.mockito.Mockito.when(producer.getConfig().getSpigotServerConfiguration("backend-test"))
                .thenReturn(java.util.Map.of("ProxySigningKeyFile", "synthetic-proxy-private.key"));
        org.mockito.Mockito.when(producer.getConfig().getProxyServerName()).thenReturn("proxy-test");
        Class<?> producerType = com.bencodez.votingplugin.proxy.VotingPluginProxy.class;
        var signers = ReflectiveAccess.call(producerType.getName(), producer, "createProxyOriginSigners",
                new String[] {"BungeeMethod"}, new Object[] {com.bencodez.votingplugin.proxy.BungeeMethod.MYSQL});
        var signerField = producerType.getDeclaredField("proxyOriginSigners");
        signerField.setAccessible(true);
        signerField.set(producer, signers);
        var sql = org.mockito.Mockito.mock(com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger.class);
        var queueField = producerType.getDeclaredField("proxyMysqlMessenger");
        queueField.setAccessible(true);
        queueField.set(producer, sql);
        UUID player = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        JsonEnvelope unclassified = vote(voteId, player);
        assertFalse(com.bencodez.votingplugin.tests.proxy.VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(
                producer, "backend-test", unclassified));
        org.mockito.Mockito.verifyNoInteractions(sql);
        var parsed = VotingPluginWire.readVote(unclassified);
        var totals = com.bencodez.votingplugin.proxy.VoteTotalsSnapshot.parseStorage(parsed.totals);
        ReflectiveAccess.call(producerType.getName(), producer, "stampLocalOriginTotals",
                new String[] {"UUID", "String", "String", "String", "long", "boolean", "boolean", "boolean",
                        "boolean", "boolean", "VoteTotalsSnapshot", "boolean"},
                new Object[] {parsed.voteId, parsed.player, parsed.uuid, parsed.service, parsed.time, parsed.realVote,
                        parsed.wasOnlineKnown, parsed.wasOnline, parsed.delayValidationKnown, parsed.delayValidated,
                        totals, true});
        var admittedFields = new java.util.HashMap<>(unclassified.getFields());
        admittedFields.put(VotingPluginWire.K_TOTALS, totals.toStorageString());
        assertTrue(com.bencodez.votingplugin.tests.proxy.VotingPluginProxyAccess.sendProxyBroadcastEnvelopeNow(
                producer, "backend-test", new JsonEnvelope(unclassified.getSubChannel(), unclassified.getSchema(), admittedFields)));
        var capture = org.mockito.ArgumentCaptor.forClass(JsonEnvelope.class);
        org.mockito.Mockito.verify(sql).sendToBackend(org.mockito.ArgumentMatchers.eq("backend-test"), capture.capture());
        byte[] completePayload = encodeUnsigned(capture.getValue());
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue))) {
                assertTrue(queue.polled.await(3, TimeUnit.SECONDS));
                queue.sent.clear();
                for (int duplicate = 0; duplicate < 2; duplicate++) {
                    assertTrue(receive(service, "proxy", "backend-test", completePayload));
                }
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(voteId, runtime.deferredVotes().pending(player).getFirst().voteId());
                assertEquals(2, queue.sent.size(), "retries re-ACK durable ownership without creating another vote");
            }
        }
    }

    private void assertProxyOnlyRoleRequired(boolean encrypted) throws Exception {
        byte[] networkKey = new byte[32];
        new java.security.SecureRandom().nextBytes(networkKey);
        Path keyFile = directory.resolve("synthetic-network.key");
        Files.writeString(keyFile, java.util.Base64.getEncoder().encodeToString(networkKey));
        var receiverEncryption = TransportEnvelopeEncryption.load(keyFile,
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, encrypted);
        // A different backend can possess the shared network key and queue-write
        // access without being authorized to originate proxy votes for this node.
        var peerEncryption = TransportEnvelopeEncryption.load(keyFile,
                TransportEnvelopeEncryption.Domain.PROXY_BACKEND, encrypted);
        try (NeoForgeRuntime runtime = NeoForgeRuntimeAccess.start(directory)) {
            Queue queue = new Queue();
            try (NeoForgeProxyTransport service = create(runtime, queue, new Factory(queue),
                    receiverEncryption, encrypted)) {
                assertTrue(queue.polled.await(3, TimeUnit.SECONDS));
                queue.sent.clear();
                UUID player = UUID.randomUUID();
                var forged = peerEncryption.encrypt(vote(UUID.randomUUID(), player));
                boolean accepted = receive(service, "proxy", "backend-test", encodeUnsigned(forged));
                assertAll("Queue metadata and shared membership must not establish proxy-only authority",
                        () -> assertFalse(accepted),
                        () -> assertTrue(runtime.deferredVotes().pending(player).isEmpty()),
                        () -> assertTrue(queue.sent.isEmpty()));
                // Same complete encrypted wrapper is accepted only with an independently pinned proxy proof.
                var valid = origin.sign(forged,
                        com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.MYSQL,
                        "proxy-test", "backend-test");
                assertTrue(receive(service, "proxy", "backend-test", encodeUnsigned(valid)));
                assertEquals(1, runtime.deferredVotes().pending(player).size());
                assertEquals(1, queue.sent.size());
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
                "UseBungeecord: true\nBungeeMethod: MYSQL\nServer: backend-test\n" + NeoForgeProxyOriginFixture.settings());
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
        return create(runtime, queue, factory,
                TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND), false);
    }

    private NeoForgeProxyTransport create(NeoForgeRuntime runtime, Queue queue, Object factory,
            TransportEnvelopeEncryption encryption, boolean encrypted) throws IOException {
        Path file = directory.resolve("mysql-test-settings.yml");
        Files.writeString(file, "UseBungeecord: true\nBungeeMethod: MYSQL\nServer: backend-test\n" + NeoForgeProxyOriginFixture.settings()
                + "CommunicationEncryption: " + encrypted + "\n");
        Object configuration = ReflectiveAccess.call(
                "com.bencodez.votingplugin.neoforge.NeoForgeProxyTransportConfiguration", null, "load",
                new String[] {"ConfigurationNode", "ConfigurationNode"},
                new Object[] {YamlConfigurationLoader.builder().path(file).build().load(), runtime.config()});
        return (NeoForgeProxyTransport) ReflectiveAccess.construct(OWNER,
                new String[] {"NeoForgeProxyTransportConfiguration", "NeoForgeVoteProcessor", "NeoForgePlayerDirectory",
                        "ProxyOriginEnvelopeSignature", "TransportEnvelopeEncryption", "QueueFactory", "boolean"},
                new Object[] {configuration, runtime.voteProcessor(), runtime.players(),
                        NeoForgeProxyOriginFixture.verifier(directory), encryption, factory, false});
    }

    private static boolean receive(NeoForgeProxyTransport service, String source, String destination, byte[] payload) {
        return (boolean) ReflectiveAccess.call(OWNER, service, "receive",
                new String[] {"String", "String", "byte[]"}, new Object[] {source, destination, payload});
    }
    private byte[] encode(JsonEnvelope envelope) { return encodeUnsigned(origin.sign(envelope,
            com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature.Domain.MYSQL, "proxy-test", "backend-test")); }
    private static byte[] encodeUnsigned(JsonEnvelope envelope) { return JsonEnvelopeCodec.encode(envelope).getBytes(StandardCharsets.UTF_8); }
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
        final CountDownLatch polled = new CountDownLatch(1);
        final CountDownLatch closed = new CountDownLatch(1);
        final AtomicInteger closes = new AtomicInteger();
        final AtomicInteger polls = new AtomicInteger();
        volatile boolean failPublishes;
        public void poll(Object receiver) { polls.incrementAndGet(); polled.countDown(); }
        public void publish(JsonEnvelope envelope) {
            if (failPublishes) throw new IllegalStateException("synthetic publication failure");
            sent.add(envelope);
            if (VotingPluginWire.SUB_BACKEND_HEARTBEAT.equals(envelope.getSubChannel())) ready.countDown();
        }
        public void close() { closes.incrementAndGet(); closed.countDown(); }
    }
}
