package com.bencodez.votingplugin.neoforge;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertTimeout;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Base64;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketHandler;
import com.bencodez.simpleapi.servercomm.sockets.SocketServer;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Domain;
import com.bencodez.votingplugin.proxy.security.SharedTransportEnvelopeAuthenticator.Mode;
import com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption;

class NeoForgeProxySocketServiceTest {
    @TempDir Path directory;
    private SharedTransportEnvelopeAuthenticator proxyAuthenticator;

    @BeforeEach
    void configuration() throws IOException {
        Files.writeString(directory.resolve("socket-auth.key"),
                Base64.getEncoder().encodeToString("0123456789abcdef0123456789abcdef".getBytes()));
        Files.writeString(directory.resolve("secretkey.key"),
                Base64.getEncoder().encodeToString("abcdef0123456789abcdef0123456789".getBytes()));
        proxyAuthenticator = SharedTransportEnvelopeAuthenticator.load(
                directory.resolve("socket-auth.key"), Mode.REQUIRED);
        Files.writeString(directory.resolve("Config.yml"), """
                DataStorage: SQLITE
                AllowUnjoined: true
                AddTotals: true
                AddTotalsOffline: true
                CountFakeVotes: true
                PointsOnVote: 1
                LimitVotePoints: -1
                """);
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

    @Test
    void commonBungeeAndVelocityEnvelopeIsRetainedBeforeAcknowledgement() throws IOException {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                for (String platform : List.of("BungeeCord", "Velocity")) {
                    UUID playerId = UUID.randomUUID();
                    UUID voteId = UUID.randomUUID();
                    sent.clear();
                    service.receive(fromProxy(vote(voteId, playerId, false)));
                    assertEquals(1, runtime.deferredVotes().pending(playerId).size(), platform);
                    assertEquals(voteId, runtime.deferredVotes().pending(playerId).get(0).voteId());
                    assertTrue(runtime.deferredVotes().pending(playerId).get(0).wasOnline());
                    assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_ACK, only(sent).getSubChannel());
                    assertEquals(voteId.toString(), only(sent).getFields().get(VotingPluginWire.K_VOTE_ID));
                }
            }
        }
    }

    @Test
    void duplicateAndRestartRemainOneDurableOccurrence() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        JsonEnvelope vote = vote(voteId, playerId, false);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(vote));
                service.receive(fromProxy(vote));
                assertEquals(1, runtime.deferredVotes().pending(playerId).size());
                assertEquals(2, sent.stream().filter(message -> VotingPluginWire.SUB_VOTE_DELIVERY_ACK
                        .equals(message.getSubChannel())).count());
            }
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(vote));
                assertEquals(1, runtime.deferredVotes().pending(playerId).size());
                assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_ACK, only(sent).getSubChannel());
            }
        }
    }

    @Test
    void cachedVoteBatchRetainsEachStableOccurrence() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID firstVoteId = UUID.randomUUID();
        UUID secondVoteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                service.receive(fromProxy(vote(firstVoteId, playerId, false, 1, 2)));
                service.receive(fromProxy(vote(secondVoteId, playerId, false, 2, 2)));

                assertEquals(List.of(firstVoteId, secondVoteId), runtime.deferredVotes().pending(playerId)
                        .stream().map(NeoForgeDeferredVote::voteId).toList());
                assertEquals(2, sent.stream().filter(message -> VotingPluginWire.SUB_VOTE_DELIVERY_ACK
                        .equals(message.getSubChannel())).count());
            }
        }
    }

    @Test
    void invalidCachedVoteBatchMetadataIsNotAcknowledged() throws IOException {
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(vote(UUID.randomUUID(), playerId, false, 0, 2)));
                service.receive(fromProxy(vote(UUID.randomUUID(), playerId, false, 3, 2)));
                service.receive(fromProxy(vote(UUID.randomUUID(), playerId, false, 1, 0)));
                assertTrue(sent.isEmpty());
                assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            }
        }
    }

    @Test
    void proxyBroadcastVoteIsNeitherRetainedNorAcknowledged() throws IOException {
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                UUID voteId = UUID.randomUUID();
                service.receive(fromProxy(vote(voteId, playerId, false, true)));
                service.receive(fromProxy(VotingPluginWire.requestVoteDeliveryAcknowledgement(
                        VotingPluginWire.voteOnline("Alex", playerId.toString(), "Service", 100L,
                                true, true, "", UUID.randomUUID(), false, true, 1, 1))));
                assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
                assertTrue(runtime.accounting().load(playerId).isEmpty());
                assertTrue(sent.stream().noneMatch(message ->
                        VotingPluginWire.SUB_VOTE_DELIVERY_ACK.equals(message.getSubChannel())));
            }
        }
    }

    @Test
    void completedRetryAcknowledgesWithoutRepeatingAdmissionAndReleaseKeepsTombstone() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(vote(voteId, playerId, false)));
                try (NeoForgeDeferredVoteStore.Claim claim = runtime.deferredVotes()
                        .claim(playerId, voteId).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
                }
                sent.clear();
                service.receive(fromProxy(vote(voteId, playerId, false)));
                assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_ACK, only(sent).getSubChannel());
                sent.clear();
                service.receive(fromProxy(VotingPluginWire.voteDeliveryReceiptRelease("neoforge", voteId,
                        VotingPluginWire.SUB_VOTE, playerId.toString())));
                assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK,
                        only(sent).getSubChannel());
                assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                        runtime.deferredVotes().state(playerId, voteId));
            }
        }
    }

    @Test
    void receiptReleaseFreesCapacityAndRemainsIdempotentAcrossRestart() throws IOException {
        UUID firstPlayer = UUID.randomUUID();
        UUID secondPlayer = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.rewardReplay().ifPresent(NeoForgeRewardReplayService::stopAdmission);
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(runtime.storage(), 1, 2, 1, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), java.time.Clock.systemUTC());
            runtime.players().joined(new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                    firstPlayer, "First", true));
            runtime.players().joined(new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                    secondPlayer, "Second", true));
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", false, false),
                    processor, runtime.players(), sent::add, proxyAuthenticator)) {
                service.receive(fromProxy(vote(firstVote, firstPlayer, false)));
                try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(firstPlayer, firstVote).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
                }
                assertEquals(NeoForgeVoteResult.Status.DEFERRED,
                        processor.process(new NeoForgeVoteRequest(secondVote, secondPlayer, "Second", "Service",
                                200L, true, true, true, true, NeoForgeVoteRequest.Scope.COMPLETE)).status());
                assertTrue(bounded.claim(secondPlayer, secondVote).isEmpty());

                sent.clear();
                JsonEnvelope release = VotingPluginWire.voteDeliveryReceiptRelease(
                        "neoforge", firstVote, VotingPluginWire.SUB_VOTE, firstPlayer.toString());
                service.receive(fromProxy(release));
                service.receive(fromProxy(release));
                assertEquals(2, sent.stream().filter(message ->
                        VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK.equals(message.getSubChannel()))
                        .count());
                try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(secondPlayer, secondVote).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
                }
            }
        }
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                    runtime.deferredVotes().state(firstPlayer, firstVote));
            assertEquals(NeoForgeVoteResult.Status.ALREADY_COMPLETED,
                    runtime.voteProcessor().process(new NeoForgeVoteRequest(firstVote, firstPlayer, "First",
                            "Service", 100L, true, true, false, true,
                            NeoForgeVoteRequest.Scope.COMPLETE)).status());
        }
    }

    @Test
    void expiredReleaseRetryIsRenewedAndAcknowledged() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.storage().user(playerId).write(com.bencodez.advancedcore.api.user.UserStorage.SQLITE,
                    NeoForgeDeferredVoteStore.COMPLETED_DEFERRED_VOTES,
                    new com.bencodez.simpleapi.sql.data.DataValueString("v2|" + voteId + "|"
                            + (System.currentTimeMillis() - TimeUnit.DAYS.toMillis(8))));
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(VotingPluginWire.voteDeliveryReceiptRelease(
                        "neoforge", voteId, VotingPluginWire.SUB_VOTE, playerId.toString())));
                assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_RECEIPT_RELEASE_ACK,
                        only(sent).getSubChannel());
            }
        }
    }

    @Test
    void fullReleaseReceiptCapacityWithholdsAcknowledgement() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID firstVote = UUID.randomUUID();
        UUID secondVote = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            runtime.rewardReplay().ifPresent(NeoForgeRewardReplayService::stopAdmission);
            runtime.players().joined(new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                    playerId, "Alex", true));
            NeoForgeDeferredVoteStore bounded = new NeoForgeDeferredVoteStore(
                    runtime.storage(), 2, 2, 2, 2, 1);
            NeoForgeVoteProcessor processor = new NeoForgeVoteProcessor(runtime.voteConfiguration(),
                    runtime.accounting(), bounded, runtime.players(), java.time.Clock.systemUTC());
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", false, false),
                    processor, runtime.players(), sent::add, proxyAuthenticator)) {
                service.receive(fromProxy(vote(firstVote, playerId, false)));
                service.receive(fromProxy(vote(secondVote, playerId, false)));
                try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(playerId, firstVote).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
                }
                try (NeoForgeDeferredVoteStore.Claim claim = bounded.claim(playerId, secondVote).orElseThrow()) {
                    assertEquals(NeoForgeDeferredVoteStore.CompletionResult.COMPLETED, claim.complete());
                }
                service.receive(fromProxy(VotingPluginWire.voteDeliveryReceiptRelease(
                        "neoforge", firstVote, VotingPluginWire.SUB_VOTE, playerId.toString())));
                sent.clear();

                service.receive(fromProxy(VotingPluginWire.voteDeliveryReceiptRelease(
                        "neoforge", secondVote, VotingPluginWire.SUB_VOTE, playerId.toString())));

                assertTrue(sent.isEmpty());
                assertEquals(NeoForgeDeferredVoteStore.OccurrenceState.COMPLETED,
                        bounded.state(playerId, secondVote));
            }
        }
    }

    @Test
    void socketCleanupContinuesAfterListenerCloseFailure() {
        SocketHandler listener = mock(SocketHandler.class);
        ClientHandler client = mock(ClientHandler.class);
        SocketServer server = mock(SocketServer.class);
        org.mockito.Mockito.when(listener.getServer()).thenReturn(server);
        doThrow(new IllegalStateException("listener failed")).when(listener).closeConnection();

        assertThrows(IllegalStateException.class,
                () -> NeoForgeProxySocketService.closeTransport(listener, client));

        verify(server).close();
        verify(client).stopConnection();
    }

    @Test
    void malformedAndUnsupportedManagedTotalsVotesAreNotAcknowledgedOrStored() throws IOException {
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(vote(UUID.randomUUID(), playerId, true)));
                JsonEnvelope malformed = JsonEnvelope.builder(VotingPluginWire.SUB_VOTE)
                        .schema(VotingPluginWire.SCHEMA_VERSION)
                        .put(VotingPluginWire.K_VOTE_ID, UUID.randomUUID().toString())
                        .put(VotingPluginWire.K_UUID, "not-a-uuid")
                        .put(VotingPluginWire.K_PLAYER, "Alex")
                        .put(VotingPluginWire.K_SERVICE, "Service").build();
                service.receive(fromProxy(VotingPluginWire.requestVoteDeliveryAcknowledgement(malformed)));
                assertTrue(sent.isEmpty());
                assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
            }
        }
    }

    @Test
    void unsignedOrWrongProxyIdentityCannotAdmitAVote() throws IOException {
        UUID playerId = UUID.randomUUID();
        JsonEnvelope vote = vote(UUID.randomUUID(), playerId, false);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(vote);
                service.receive(proxyAuthenticator.sign(vote, Domain.SOCKET_PROXY_BACKEND,
                        "other-proxy", "neoforge"));
                assertTrue(runtime.deferredVotes().pending(playerId).isEmpty());
                assertTrue(sent.isEmpty());
            }
        }
    }

    @Test
    void authenticatedEncryptedVoteIsDecryptedAndAcknowledgedInTheSameOrder() throws IOException {
        UUID playerId = UUID.randomUUID();
        UUID voteId = UUID.randomUUID();
        TransportEnvelopeEncryption encryption = TransportEnvelopeEncryption.load(
                directory.resolve("secretkey.key"), TransportEnvelopeEncryption.Domain.PROXY_BACKEND, true);
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService active = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", true, false),
                    runtime.voteProcessor(), runtime.players(), sent::add, proxyAuthenticator,
                    encryption, () -> { }, false)) {
                sent.clear();
                JsonEnvelope incoming = proxyAuthenticator.sign(
                        encryption.encrypt(vote(voteId, playerId, false)),
                        Domain.SOCKET_PROXY_BACKEND, "proxy1", "neoforge");
                active.receive(incoming);
                assertEquals(1, runtime.deferredVotes().pending(playerId).size());
                SharedTransportEnvelopeAuthenticator.Verification verified = proxyAuthenticator.verify(
                        only(sent), Domain.SOCKET_PROXY_BACKEND, "proxy1");
                assertTrue(verified.accepted());
                TransportEnvelopeEncryption.Decryption decrypted = encryption.decrypt(verified.envelope());
                assertTrue(decrypted.accepted());
                assertEquals(VotingPluginWire.SUB_VOTE_DELIVERY_ACK, decrypted.envelope().getSubChannel());
            }
        }
    }

    @Test
    void capacityFailureIsNotAcknowledged() throws IOException {
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                for (int index = 0; index < NeoForgeDeferredVoteStore.MAX_DEFERRED_PER_USER; index++) {
                    service.receive(fromProxy(vote(UUID.randomUUID(), playerId, false)));
                }
                sent.clear();
                service.receive(fromProxy(vote(UUID.randomUUID(), playerId, false)));
                assertTrue(sent.isEmpty());
                assertEquals(NeoForgeDeferredVoteStore.MAX_DEFERRED_PER_USER,
                        runtime.deferredVotes().pending(playerId).size());
            }
        }
    }

    @Test
    void statusAndPresenceUseExistingCapabilityProtocol() throws IOException {
        UUID requestId = UUID.randomUUID();
        UUID playerId = UUID.randomUUID();
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = service(runtime, sent)) {
                sent.clear();
                service.receive(fromProxy(VotingPluginWire.status("neoforge", requestId)));
                assertEquals(VotingPluginWire.SUB_STATUS_OKAY, only(sent).getSubChannel());
                assertEquals(requestId.toString(), only(sent).getFields().get(VotingPluginWire.K_REQUEST_ID));
                assertFalse(VotingPluginWire.advertisesVoteDeliveryAcknowledgement(only(sent)));
                assertTrue(VotingPluginWire.advertisesAuthenticatedSocketVoteDelivery(only(sent)));
                assertFalse(VotingPluginWire.advertisesVoteDelayRejectionAcknowledgement(only(sent)));
                sent.clear();
                var identity = new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(playerId, "Alex", true);
                runtime.players().joined(identity);
                service.playerOnline(identity);
                assertTrue(sent.stream().anyMatch(message -> VotingPluginWire.SUB_BACKEND_STARTED
                        .equals(message.getSubChannel())));
                assertTrue(sent.stream().anyMatch(message -> VotingPluginWire.SUB_LOGIN
                        .equals(message.getSubChannel())));
                sent.clear();
                service.playerOffline(identity);
                assertEquals(VotingPluginWire.SUB_LOGOUT, only(sent).getSubChannel());
            }
        }
    }

    @Test
    void startupAndHeartbeatDoNotAdvertiseUnsupportedDelayRejectionAcknowledgements() throws IOException {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService ignored = service(runtime, sent)) {
                List<JsonEnvelope> capabilities = sent.stream().filter(message ->
                        VotingPluginWire.SUB_BACKEND_STARTED.equals(message.getSubChannel())
                                || VotingPluginWire.SUB_BACKEND_HEARTBEAT.equals(message.getSubChannel())).toList();
                assertFalse(capabilities.isEmpty());
                assertTrue(capabilities.stream().noneMatch(VotingPluginWire::advertisesVoteDeliveryAcknowledgement));
                assertTrue(capabilities.stream()
                        .allMatch(VotingPluginWire::advertisesAuthenticatedSocketVoteDelivery));
                assertTrue(capabilities.stream().noneMatch(
                        VotingPluginWire::advertisesVoteDelayRejectionAcknowledgement));
            }
        }
    }

    @Test
    void existingBungeeSettingsShapeIsValidated() throws IOException {
        ConfigurationNode disabled = load("UseBungeecord: false\n");
        assertFalse(NeoForgeProxySocketConfiguration.load(disabled).enabled());
        ConfigurationNode enabled = load("""
                UseBungeecord: true
                BungeeMethod: SOCKETS
                Server: neoforge
                BungeeServer:
                  Name: proxy1
                  Host: 127.0.0.1
                  Port: 1297
                SpigotServer:
                  Host: 0.0.0.0
                  Port: 1298
                SocketAuthenticationKeyFile: socket-auth.key
                """);
        NeoForgeProxySocketConfiguration parsed = NeoForgeProxySocketConfiguration.load(enabled);
        assertTrue(parsed.enabled());
        assertEquals("neoforge", parsed.server());
        assertEquals("proxy1", parsed.proxyName());
        assertThrows(IllegalArgumentException.class, () -> NeoForgeProxySocketConfiguration.load(load("""
                UseBungeecord: true
                BungeeMethod: SOCKETS
                Server: PleaseSet
                BungeeServer:
                  Host: 127.0.0.1
                """)));
    }

    @Test
    void stalledSocketSendDoesNotBlockLifecycleEventsAndShutdownIsBounded() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CountDownLatch entered = new CountDownLatch(1);
            CountDownLatch release = new CountDownLatch(1);
            NeoForgeProxySocketService service = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", false, false),
                    runtime.voteProcessor(), runtime.players(), ignored -> {
                        entered.countDown();
                        try {
                            release.await();
                        } catch (InterruptedException interrupted) {
                            Thread.currentThread().interrupt();
                        }
                    }, proxyAuthenticator,
                    com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.disabled(
                            com.bencodez.votingplugin.proxy.security.TransportEnvelopeEncryption.Domain.PROXY_BACKEND),
                    () -> { }, true);
            try {
                assertTrue(entered.await(1, TimeUnit.SECONDS));
                var identity = new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                        UUID.randomUUID(), "Alex", true);
                assertTimeout(Duration.ofMillis(200), () -> service.playerOnline(identity));
                assertTimeout(Duration.ofSeconds(2), service::close);
            } finally {
                release.countDown();
                service.close();
            }
        }
    }

    @Test
    void rejectedPresenceSendTriggersAuthoritativeRecoveryAfterQueueDrains() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CountDownLatch entered = new CountDownLatch(1);
            CountDownLatch release = new CountDownLatch(1);
            CountDownLatch recovered = new CountDownLatch(1);
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            NeoForgeProxySocketService service = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", false, false),
                    runtime.voteProcessor(), runtime.players(), envelope -> {
                        sent.add(envelope);
                        if (VotingPluginWire.SUB_BACKEND_STARTED.equals(envelope.getSubChannel())
                                && Long.parseLong(envelope.getFields().get(VotingPluginWire.K_PRESENCE_TIMESTAMP))
                                > Long.parseLong(envelope.getFields().get(VotingPluginWire.K_BACKEND_STARTED_AT))) {
                            recovered.countDown();
                        }
                        entered.countDown();
                        try {
                            release.await();
                        } catch (InterruptedException interrupted) {
                            Thread.currentThread().interrupt();
                        }
                    }, proxyAuthenticator,
                    TransportEnvelopeEncryption.disabled(TransportEnvelopeEncryption.Domain.PROXY_BACKEND),
                    () -> { }, true);
            try {
                assertTrue(entered.await(1, TimeUnit.SECONDS));
                for (int index = 0; index < 140; index++) {
                    service.playerOnline(new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                            UUID.randomUUID(), "Player" + index, true));
                }
                release.countDown();
                assertTrue(recovered.await(5, TimeUnit.SECONDS));
            } finally {
                release.countDown();
                service.close();
            }
        }
    }

    @Test
    void snapshotAndLogoutArePublishedInOnePresenceOrder() throws Exception {
        try (NeoForgeRuntime runtime = NeoForgeRuntime.start(directory)) {
            CountDownLatch snapshotEntered = new CountDownLatch(1);
            CountDownLatch releaseSnapshot = new CountDownLatch(1);
            CountDownLatch logoutFinished = new CountDownLatch(1);
            AtomicBoolean blockSnapshot = new AtomicBoolean();
            CopyOnWriteArrayList<JsonEnvelope> sent = new CopyOnWriteArrayList<>();
            try (NeoForgeProxySocketService service = new NeoForgeProxySocketService(
                    new NeoForgeProxySocketConfiguration(true, "neoforge", "proxy1", "127.0.0.1", 1297,
                            "127.0.0.1", 1298, "socket-auth.key", false, false),
                    runtime.voteProcessor(), runtime.players(), envelope -> {
                        sent.add(envelope);
                        if (blockSnapshot.get() && VotingPluginWire.SUB_PRESENCE_SNAPSHOT
                                .equals(envelope.getSubChannel())) {
                            snapshotEntered.countDown();
                            try {
                                releaseSnapshot.await();
                            } catch (InterruptedException interrupted) {
                                Thread.currentThread().interrupt();
                            }
                        }
                    }, proxyAuthenticator)) {
                JsonEnvelope started = sent.stream().filter(message -> VotingPluginWire.SUB_BACKEND_STARTED
                        .equals(message.getSubChannel())).findFirst().orElseThrow();
                UUID incarnation = VotingPluginWire.readBackendIncarnationId(started);
                long startedAt = VotingPluginWire.readBackendStartedAt(started);
                var identity = new com.bencodez.votingplugin.core.vote.SharedVoteIdentity(
                        UUID.randomUUID(), "Alex", true);
                service.playerOnline(identity);
                sent.clear();
                blockSnapshot.set(true);
                Thread snapshot = new Thread(() -> service.receive(fromProxy(
                        VotingPluginWire.presenceSnapshotRequest("neoforge", UUID.randomUUID(), incarnation,
                                startedAt, System.currentTimeMillis()))));
                snapshot.start();
                assertTrue(snapshotEntered.await(5, TimeUnit.SECONDS));
                Thread logout = new Thread(() -> {
                    service.playerOffline(identity);
                    logoutFinished.countDown();
                });
                logout.start();
                assertFalse(logoutFinished.await(100, TimeUnit.MILLISECONDS));
                releaseSnapshot.countDown();
                snapshot.join(5_000L);
                logout.join(5_000L);
                assertTrue(logoutFinished.await(1, TimeUnit.SECONDS));

                JsonEnvelope snapshotMessage = sent.stream().filter(message -> VotingPluginWire.SUB_PRESENCE_SNAPSHOT
                        .equals(message.getSubChannel())).findFirst().orElseThrow();
                JsonEnvelope logoutMessage = sent.stream().filter(message -> VotingPluginWire.SUB_LOGOUT
                        .equals(message.getSubChannel())).findFirst().orElseThrow();
                assertTrue(Long.parseLong(logoutMessage.getFields().get(VotingPluginWire.K_PRESENCE_TIMESTAMP))
                        > Long.parseLong(snapshotMessage.getFields().get(VotingPluginWire.K_PRESENCE_TIMESTAMP)));
                assertEquals(List.of(identity.uuid().toString()), VotingPluginWire.readPresenceSnapshot(snapshotMessage)
                        .players.stream().map(player -> player.uuid).toList());
            } finally {
                releaseSnapshot.countDown();
            }
        }
    }

    private NeoForgeProxySocketService service(NeoForgeRuntime runtime, List<JsonEnvelope> sent) {
        return new NeoForgeProxySocketService(new NeoForgeProxySocketConfiguration(true,
                "neoforge", "proxy1", "127.0.0.1", 1297, "127.0.0.1", 1298,
                "socket-auth.key", false, false),
                runtime.voteProcessor(), runtime.players(), sent::add, proxyAuthenticator);
    }

    private JsonEnvelope fromProxy(JsonEnvelope envelope) {
        return proxyAuthenticator.sign(envelope, Domain.SOCKET_PROXY_BACKEND, "proxy1", "neoforge");
    }

    private static JsonEnvelope vote(UUID voteId, UUID playerId, boolean manageTotals) {
        return vote(voteId, playerId, manageTotals, false, 1, 1);
    }

    private static JsonEnvelope vote(UUID voteId, UUID playerId, boolean manageTotals,
            int num, int numberOfVotes) {
        return vote(voteId, playerId, manageTotals, false, num, numberOfVotes);
    }

    private static JsonEnvelope vote(UUID voteId, UUID playerId, boolean manageTotals, boolean broadcast) {
        return vote(voteId, playerId, manageTotals, broadcast, 1, 1);
    }

    private static JsonEnvelope vote(UUID voteId, UUID playerId, boolean manageTotals, boolean broadcast,
            int num, int numberOfVotes) {
        return VotingPluginWire.requestVoteDeliveryAcknowledgement(VotingPluginWire.vote(
                "Alex", playerId.toString(), "Service", 100L, true, true, "", voteId,
                manageTotals, broadcast, num, numberOfVotes));
    }

    private static JsonEnvelope only(List<JsonEnvelope> sent) {
        assertEquals(1, sent.size());
        return sent.get(0);
    }

    private ConfigurationNode load(String yaml) throws IOException {
        Path file = directory.resolve("shape-" + UUID.randomUUID() + ".yml");
        Files.writeString(file, yaml);
        return YamlConfigurationLoader.builder().path(file).build().load();
    }
}
