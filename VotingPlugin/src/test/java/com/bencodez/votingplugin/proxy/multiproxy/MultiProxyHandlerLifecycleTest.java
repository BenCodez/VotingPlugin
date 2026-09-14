package com.bencodez.votingplugin.proxy.multiproxy;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.junit.jupiter.api.Test;

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
