package com.bencodez.votingplugin.proxy.multiproxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.simpleapi.servercomm.sockets.ClientHandler;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.proxy.VotingPluginWire;

class MultiProxyHandlerLifecycleTest {

	@Test
	void doesNotAcceptAnEnvelopeWhenNoSocketDestinationExists() {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);

		assertFalse(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
	}

	@Test
	void rejectsSocketSendWhenAnyConfiguredDestinationFails() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("backend"));
		ClientHandler failing = mock(ClientHandler.class);
		doThrow(new IllegalStateException("socket unavailable")).when(failing).sendEnvelope(org.mockito.ArgumentMatchers.any());
		java.lang.reflect.Field clients = MultiProxyHandler.class.getDeclaredField("multiproxyClientHandles");
		clients.setAccessible(true);
		Map<String, ClientHandler> configured = new LinkedHashMap<>();
		configured.put("backend", failing);
		clients.set(handler, configured);

		assertFalse(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
	}

	@Test
	void acceptsAQueuedSocketSendWhenTheConfiguredDestinationReturnsNormally() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("backend"));
		ClientHandler healthy = mock(ClientHandler.class);
		java.lang.reflect.Field clients = MultiProxyHandler.class.getDeclaredField("multiproxyClientHandles");
		clients.setAccessible(true);
		Map<String, ClientHandler> configured = new LinkedHashMap<>();
		configured.put("backend", healthy);
		clients.set(handler, configured);

		assertTrue(handler.sendMultiProxyEnvelopeAccepted(JsonEnvelope.builder("vote").build()));
		verify(healthy).sendEnvelope(org.mockito.ArgumentMatchers.any());
	}

	@Test
	void redisSubsetSendPreservesConfiguredChannelCasing() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Proxy2"));
		com.bencodez.simpleapi.servercomm.redis.RedisHandler redis =
				mock(com.bencodez.simpleapi.servercomm.redis.RedisHandler.class);
		java.lang.reflect.Field connection = MultiProxyHandler.class.getDeclaredField("multiProxyRedis");
		connection.setAccessible(true);
		connection.set(handler, redis);
		JsonEnvelope envelope = JsonEnvelope.builder("vote").build();

		assertEquals(java.util.Set.of("Proxy2"), handler.getConfiguredMultiProxyVoteRecipients());
		assertTrue(handler.sendMultiProxyEnvelopeAccepted(envelope, List.of("proxy2")));

		verify(redis).publishEnvelope("VotingPluginProxy_Proxy2", envelope);
	}

	@Test
	void stopsEveryReplacedSocketClientEvenWhenOneStopFails() {
		ClientHandler failing = mock(ClientHandler.class);
		ClientHandler healthy = mock(ClientHandler.class);
		doThrow(new IllegalStateException("already closed")).when(failing).stopConnection();
		Map<String, ClientHandler> clients = new LinkedHashMap<>();
		clients.put("failing", failing);
		clients.put("healthy", healthy);

		MultiProxyHandler.stopSocketClients(clients);

		verify(failing).stopConnection();
		verify(healthy).stopConnection();
	}

	@Test
	void routesStableWireVoteWithoutOriginThroughLegacyTrigger() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope",
				com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.vote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", voteId,
				false, false, 1, 1));

		verify(handler).triggerVote(org.mockito.ArgumentMatchers.eq("Player"),
				org.mockito.ArgumentMatchers.eq("Service"), org.mockito.ArgumentMatchers.eq(true),
				org.mockito.ArgumentMatchers.eq(true), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class),
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"));
		verify(handler, org.mockito.Mockito.never()).triggerVote(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean(),
				org.mockito.ArgumentMatchers.anyBoolean(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void legacyVoteEnvelopeHasNoReliableOriginMetadata() {
		UUID voteId = UUID.randomUUID();
		JsonEnvelope envelope = VotingPluginWire.vote("Player", "00000000-0000-0000-0000-000000000001",
				"Service", 100L, false, true, "totals", voteId, false, false, 1, 1);

		assertEquals(voteId.toString(), envelope.getFields().get(VotingPluginWire.K_VOTE_ID));
		assertFalse(envelope.getFields().containsKey(VotingPluginWire.K_MULTI_PROXY_ORIGIN));
	}

	@Test
	void forwardsOriginAndStableWireVoteIdToDurableTrigger() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", voteId,
				false, false, 1, 1, "Primary"));

		verify(handler).triggerVote(org.mockito.ArgumentMatchers.eq("Player"),
				org.mockito.ArgumentMatchers.eq("Service"), org.mockito.ArgumentMatchers.eq(true),
				org.mockito.ArgumentMatchers.eq(true), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class),
				org.mockito.ArgumentMatchers.eq("00000000-0000-0000-0000-000000000001"),
				org.mockito.ArgumentMatchers.eq(voteId), org.mockito.ArgumentMatchers.eq("Primary"));
	}

	@Test
	void rejectsReliableVoteEnvelopeWithoutStableId() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVote("Player",
				"00000000-0000-0000-0000-000000000001", "Service", 100L, false, true, "totals", null,
				false, false, 1, 1, "Primary"));

		verify(handler, org.mockito.Mockito.never()).triggerVote(org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.anyString(), org.mockito.ArgumentMatchers.anyBoolean(),
				org.mockito.ArgumentMatchers.anyBoolean(), org.mockito.ArgumentMatchers.anyLong(),
				org.mockito.ArgumentMatchers.any(VoteTotalsSnapshot.class), org.mockito.ArgumentMatchers.anyString(),
				org.mockito.ArgumentMatchers.any(), org.mockito.ArgumentMatchers.anyString());
	}

	@Test
	void routesOnlyTheOriginatingProxyAcknowledgement() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteAck(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteAck(voteId, "Other", "Replica"));

		verify(handler).onMultiProxyVoteAcknowledged(voteId, "Replica");
	}

	@Test
	void routesOnlyAuthenticatedTargetedRetirementMessages() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Replica");
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.REDIS);
		org.mockito.Mockito.when(handler.getProxyServers()).thenReturn(List.of("Primary"));
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Other", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetire(voteId, "Primary", "Other"));

		verify(handler).onMultiProxyVoteRetirementRequested(voteId, "Primary");
	}

	@Test
	void routesOnlyTheOriginatingProxyRetirementAcknowledgement() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class, org.mockito.Mockito.CALLS_REAL_METHODS);
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		UUID voteId = UUID.randomUUID();
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetireAck(voteId, "Primary", "Replica"));
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyVoteRetireAck(voteId, "Other", "Replica"));

		verify(handler).onMultiProxyVoteRetirementAcknowledged(voteId, "Replica");
	}

	@Test
	void fencesOnlyPeersThatAdvertiseDurableAcknowledgements() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Legacy", "Capable"));
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1));

		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void durableAcknowledgementCapabilityExpiresAndCanBeRenewed() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Capable"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS - 1,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS,
				2_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void expiredKnownCapabilityWaitsForRenewalInsteadOfBecomingLegacy() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Capable"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_LEASE_MILLIS);
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);

		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities("Capable", 1, true));
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
		assertEquals(java.util.Set.of("Capable"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal());
	}

	@Test
	void durableCapabilityIdentitySurvivesRestartAndFencesAnOfflinePeer(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler beforeRestart = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handleCapability(beforeRestart, "Capable");
		assertEquals(java.util.Set.of("Capable"), beforeRestart.getMultiProxyVoteRecipients());

		MultiProxyHandler afterRestart = capabilityHandler(dataDirectory, "Capable", "Legacy");
		afterRestart.restoreVoteCapabilityPeers();

		assertTrue(afterRestart.getMultiProxyVoteRecipients().isEmpty());
		assertEquals(java.util.Set.of("Capable"),
				afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal());
		assertFalse(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().contains("Legacy"));
	}

	@Test
	void malformedCapabilityStateBlocksForwardingUntilTheOperatorRepairsItAndRestarts(@TempDir Path dataDirectory)
			throws Exception {
		Files.writeString(dataDirectory.resolve(".multiproxy-capability-peers.json"), "not-json");
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handler.restoreVoteCapabilityPeers();

		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		handleCapability(handler, "Capable");
		assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());

		// Removing the invalid local state and restarting deterministically returns
		// to fresh-install semantics: Legacy remains legacy and a new handshake
		// classifies only Capable as an ACK peer.
		Files.delete(dataDirectory.resolve(".multiproxy-capability-peers.json"));
		MultiProxyHandler recovered = capabilityHandler(dataDirectory, "Capable", "Legacy");
		recovered.restoreVoteCapabilityPeers();
		assertFalse(recovered.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(recovered.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		handleCapability(recovered, "Capable");
		assertEquals(java.util.Set.of("Capable"), recovered.getMultiProxyVoteRecipients());
		assertFalse(recovered.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().contains("Legacy"));
	}

	@Test
	void nonCanonicalCapabilityPeerBlocksForwardingInsteadOfBecomingLegacy(@TempDir Path dataDirectory)
			throws Exception {
		Files.writeString(dataDirectory.resolve(".multiproxy-capability-peers.json"),
				"{\"version\":1,\"peers\":[\"Capable\"]}");

		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable", "Legacy");
		handler.restoreVoteCapabilityPeers();

		assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
	}

	@Test
	void freshInstallKeepsLegacyPeersOnTheHistoricalRoute(@TempDir Path dataDirectory) {
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Legacy");
		handler.restoreVoteCapabilityPeers();
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
	}

	@Test
	void newlyConfiguredPeerWaitsForCapabilityAnnouncementBeforeItIsClassified() throws Exception {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of("Candidate"));
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L);

		assertEquals(java.util.Set.of("Candidate"),
				handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		handleCapability(handler, "Candidate");

		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipients());
	}

	@Test
	void missedCapabilityAnnouncementFallsBackOnlyAfterItsBoundedPersistedWindow(@TempDir Path dataDirectory) {
		MultiProxyHandler beforeRestart = capabilityHandler(dataDirectory, "Legacy");
		org.mockito.Mockito.when(beforeRestart.capabilityNowMillis()).thenReturn(1_000L);
		assertEquals(java.util.Set.of("Legacy"),
				beforeRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());

		MultiProxyHandler afterRestart = capabilityHandler(dataDirectory, "Legacy");
		afterRestart.restoreVoteCapabilityPeers();
		org.mockito.Mockito.when(afterRestart.capabilityNowMillis()).thenReturn(
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_DISCOVERY_WINDOW_MILLIS);
		assertTrue(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
		// The expired deadline remains a legacy classification rather than beginning
		// another discovery window on every retry/restart.
		assertTrue(afterRestart.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());
	}

	@Test
	void clockRollbackExpiresDiscoveryInsteadOfExtendingItsPersistedWindow(@TempDir Path dataDirectory) {
		MultiProxyHandler handler = capabilityHandler(dataDirectory, "Candidate");
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L, 500L);

		assertEquals(java.util.Set.of("Candidate"), handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
		assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery().isEmpty());

		MultiProxyCapabilityStore.State persisted = assertDoesNotThrow(() -> MultiProxyCapabilityStore.load(dataDirectory));
		assertEquals(500L, persisted.discoveryDeadlines().get("candidate"));
		assertEquals(500L, persisted.lastObservedMillis());
	}

	@Test
	void removedCapabilityPeerIsPrunedDurablyBeforeSameNameIsReadded(@TempDir Path dataDirectory) throws Exception {
		MultiProxyHandler original = capabilityHandler(dataDirectory, "Capable");
		handleCapability(original, "Capable");

		MultiProxyHandler removed = capabilityHandler(dataDirectory, "Legacy");
		removed.restoreVoteCapabilityPeers();
		assertTrue(MultiProxyCapabilityStore.load(dataDirectory).peers().isEmpty());

		MultiProxyHandler readded = capabilityHandler(dataDirectory, "Capable");
		readded.restoreVoteCapabilityPeers();
		assertTrue(readded.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		assertEquals(java.util.Set.of("Capable"),
				readded.getMultiProxyVoteRecipientsAwaitingCapabilityDiscovery());
	}

	@Test
	void failedConfigurationPruningBlocksForwardingWithoutInstallingStalePeerState(@TempDir Path dataDirectory)
			throws Exception {
		MultiProxyCapabilityStore.save(dataDirectory, java.util.Set.of("capable"), Map.of(), 0L);
		MultiProxyHandler removed = capabilityHandler(dataDirectory, "Legacy");
		try (org.mockito.MockedStatic<MultiProxyCapabilityStore> store = org.mockito.Mockito.mockStatic(
				MultiProxyCapabilityStore.class, org.mockito.Mockito.CALLS_REAL_METHODS)) {
			store.when(() -> MultiProxyCapabilityStore.save(org.mockito.ArgumentMatchers.eq(dataDirectory),
					org.mockito.ArgumentMatchers.anyCollection(), org.mockito.ArgumentMatchers.anyMap(),
					org.mockito.ArgumentMatchers.anyLong()))
					.thenThrow(new java.io.IOException("read-only capability state"));
			removed.restoreVoteCapabilityPeers();
		}

		assertTrue(removed.isMultiProxyVoteCapabilityRecoveryBlocked());
		assertTrue(removed.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		assertEquals(java.util.Set.of("capable"), MultiProxyCapabilityStore.load(dataDirectory).peers());
	}

	@Test
	void capabilityStoreRejectsSymlinkedState(@TempDir Path dataDirectory) throws Exception {
		Path outside = Files.createTempFile("multiproxy-capability-outside", ".json");
		try {
			Files.createSymbolicLink(dataDirectory.resolve(".multiproxy-capability-peers.json"), outside);
			assertThrows(java.io.IOException.class, () -> MultiProxyCapabilityStore.load(dataDirectory));
			MultiProxyHandler handler = capabilityHandler(dataDirectory, "Capable");
			handler.restoreVoteCapabilityPeers();
			handleCapability(handler, "Capable");
			assertTrue(handler.getMultiProxyVoteRecipients().isEmpty());
			assertTrue(handler.isMultiProxyVoteCapabilityRecoveryBlocked());
			assertTrue(handler.getMultiProxyVoteRecipientsAwaitingCapabilityRenewal().isEmpty());
		} finally {
			Files.deleteIfExists(outside);
		}
	}

	private static MultiProxyHandler capabilityHandler(Path dataDirectory, String... peers) {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyMethod()).thenReturn(MultiProxyMethod.SOCKETS);
		org.mockito.Mockito.when(handler.getMultiProxyServers()).thenReturn(List.of(peers));
		org.mockito.Mockito.when(handler.getPluginDataFolder()).thenReturn(dataDirectory.toFile());
		return handler;
	}

	private static void handleCapability(MultiProxyHandler handler, String peer) throws Exception {
		Method handleEnvelope = MultiProxyHandler.class.getDeclaredMethod("handleEnvelope", JsonEnvelope.class);
		handleEnvelope.setAccessible(true);
		handleEnvelope.invoke(handler, VotingPluginWire.multiProxyCapabilities(peer, 1, true));
	}

	@Test
	void renewalAdvertisementIsRateLimitedAfterAnInitialHandshake() {
		MultiProxyHandler handler = mock(MultiProxyHandler.class,
				org.mockito.Mockito.withSettings().useConstructor().defaultAnswer(org.mockito.Mockito.CALLS_REAL_METHODS));
		org.mockito.Mockito.when(handler.getMultiProxyServerName()).thenReturn("Primary");
		org.mockito.Mockito.when(handler.capabilityNowMillis()).thenReturn(1_000L,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS - 1,
				1_000L + MultiProxyHandler.VOTE_CAPABILITY_RENEWAL_MIN_INTERVAL_MILLIS);
		org.mockito.Mockito.when(handler.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any())).thenReturn(true);
		org.mockito.Mockito.clearInvocations(handler);

		handler.announceMultiProxyVoteCapability();
		assertFalse(handler.renewMultiProxyVoteCapabilityIfDue());
		assertTrue(handler.renewMultiProxyVoteCapabilityIfDue());
		org.mockito.Mockito.verify(handler, org.mockito.Mockito.times(2))
				.sendMultiProxyEnvelopeAccepted(org.mockito.ArgumentMatchers.any());
	}
}
