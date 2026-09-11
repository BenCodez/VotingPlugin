
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.ProxyMysqlUserTable;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.broadcast.ProxyBroadcastDecider;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VotingPluginProxyTest {
	@TempDir
	java.nio.file.Path temporaryDirectory;

	@InjectMocks
	private VotingPluginProxyTestImpl votingPluginProxy;

	@Mock
	private ProxyMysqlUserTable proxyMySQL;

	@Mock
	private GlobalDataHandlerProxy globalDataHandler;

	@Mock
	private MultiProxyHandler multiProxyHandler;

	@BeforeEach
	void setUp() {
		MockitoAnnotations.openMocks(this);
		votingPluginProxy.setProxyMySQL(proxyMySQL);
		votingPluginProxy.setGlobalDataHandler(globalDataHandler);
		votingPluginProxy.setMultiProxyHandler(multiProxyHandler);
		votingPluginProxy.setDataFolder(temporaryDirectory.toFile());

	}

	@Test
	void httpMethodPreparationRejectsAnOccupiedPortBeforeConfigurationPublication() throws Exception {
		try (java.net.ServerSocket occupied = new java.net.ServerSocket(0, 1,
				java.net.InetAddress.getByName("127.0.0.1"))) {
			com.bencodez.votingplugin.proxy.VotingPluginProxyConfig candidate =
					Mockito.mock(com.bencodez.votingplugin.proxy.VotingPluginProxyConfig.class);
			Mockito.when(candidate.getHttpHost()).thenReturn("127.0.0.1");
			Mockito.when(candidate.getHttpPort()).thenReturn(occupied.getLocalPort());
			Mockito.when(candidate.getHttpPublicEndpoint())
					.thenReturn("https://proxy.example.test:" + occupied.getLocalPort());

			assertThrows(IllegalStateException.class,
					() -> votingPluginProxy.prepareHttpTransportChange(candidate));
		}
	}

	@Test
	void indeterminateHttpVoteRetriesWithThePublishedStableDeliveryId() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		String deliveryId = "00000000-0000-0000-0000-000000000159";
		votingPluginProxy.failNextGeneratedHttpSend(deliveryId);

		assertTrue(votingPluginProxy.sendHttpVoteEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("vote").build()));
		assertEquals(java.util.List.of(deliveryId), votingPluginProxy.getAttemptedVotePartyDeliveryIds());
	}

	@Test
	void indeterminateGenericHttpSendRecoversWithoutEscapingToRetryTheVote() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		String deliveryId = "00000000-0000-0000-0000-00000000016a";
		votingPluginProxy.failNextGeneratedHttpSend(deliveryId);

		assertTrue(votingPluginProxy.sendGenericHttpEnvelopeForTest("Server1",
				VotingPluginWire.voteUpdate("uuid", 1, 10, "Service", 100L, "totals")));
		assertEquals(java.util.List.of(deliveryId), votingPluginProxy.getAttemptedVotePartyDeliveryIds());
	}

	@Test
	void rejectedGenericHttpSendIsHandledWithoutEscapingToRetryTheVote() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		assertFalse(votingPluginProxy.sendGenericHttpEnvelopeForTest("Server1",
				VotingPluginWire.voteUpdate("uuid", 1, 10, "Service", 100L, "totals")));
	}

	@Test
	void rejectedRewardBearingAuxiliarySendRetainsItsStableDeliveryIdForRetry() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);
		String deliveryId = "00000000-0000-0000-0000-00000000016b";
		JsonEnvelope envelope = VotingPluginWire.voteDelayRejected("Player",
				"00000000-0000-0000-0000-000000000001", "Service", true);

		assertFalse(votingPluginProxy.sendStableHttpEnvelopeForTest("Server1", deliveryId, envelope));
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		assertTrue(votingPluginProxy.sendStableHttpEnvelopeForTest("Server1", deliveryId, envelope));
		assertEquals(java.util.List.of(deliveryId, deliveryId),
				votingPluginProxy.getAttemptedVotePartyDeliveryIds());
	}

	@Test
	void cachedHttpVoteReusesItsDeterministicIdAfterRestart() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		OfflineBungeeVote pending = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 100L, false, "totals");

		assertTrue(votingPluginProxy.sendHttpVoteEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("vote").build(), pending));

		OfflineBungeeVote restored = new OfflineBungeeVote(pending.getVoteId(), pending.getPlayerName(),
				pending.getUuid(), pending.getService(), pending.getTime(), pending.isRealVote(), pending.getText(),
				pending.isBroadcastForwarded(), pending.isProxyBroadcastHandled(), pending.getBroadcastTargets(),
				pending.getBroadcastForwardedServers(), pending.isRewardDelivered(),
				OfflineBungeeVote.decodeHttpDeliveryIds(pending.encodeHttpDeliveryIds()));
		assertTrue(votingPluginProxy.sendHttpVoteEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("vote").build(), restored));
		assertEquals(2, votingPluginProxy.getAttemptedVotePartyDeliveryIds().size());
		assertEquals(votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(0),
				votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(1));
	}

	@Test
	void standaloneHttpBroadcastUsesSeparateStableIdFromRewardDelivery() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		String rewardId = "00000000-0000-0000-0000-000000000162";
		OfflineBungeeVote pending = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 100L, true, "totals");
		pending.setHttpDeliveryId("Server1", rewardId);

		assertTrue(votingPluginProxy.sendHttpBroadcastEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("vote").build(), pending));
		assertEquals(rewardId, pending.getHttpDeliveryId("Server1"));
		assertEquals(1, votingPluginProxy.getAttemptedVotePartyDeliveryIds().size());
		assertFalse(rewardId.equals(votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(0)));
	}

	@Test
	void cachedStandaloneBroadcastReusesItsDeterministicIdAfterRestart() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		OfflineBungeeVote pending = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 100L, true, "totals");

		assertTrue(votingPluginProxy.sendHttpBroadcastEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("broadcast").build(), pending));
		OfflineBungeeVote restored = new OfflineBungeeVote(pending.getVoteId(), pending.getPlayerName(), pending.getUuid(),
				pending.getService(), pending.getTime(), pending.isRealVote(), pending.getText(), false, true,
				java.util.Set.of("Server1"), java.util.Set.of(), pending.isRewardDelivered(),
				OfflineBungeeVote.decodeHttpDeliveryIds(pending.encodeHttpDeliveryIds()),
				OfflineBungeeVote.decodeHttpBroadcastDeliveryIds(pending.encodeHttpBroadcastDeliveryIds()));
		assertTrue(votingPluginProxy.sendHttpBroadcastEnvelopeWithRecoveryForTest("Server1",
				JsonEnvelope.builder("broadcast").build(), restored));
		assertEquals(2, votingPluginProxy.getAttemptedVotePartyDeliveryIds().size());
		assertEquals(votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(0),
				votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(1));
	}

	@Test
	void acceptedRewardStillPersistsUnresolvedStandaloneBroadcast() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote state = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), true, java.util.Collections.emptyMap(),
				java.util.Map.of("Server1", "00000000-0000-0000-0000-000000000169"));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.when(voteCache.addOnlineVoteDurably("player-uuid", state)).thenReturn(true);

		assertTrue(spyProxy.persistUncachedStandaloneBroadcastForTest("player-uuid", state, false));

		assertTrue(state.isRewardDelivered());
		assertFalse(state.isBroadcastForwarded());
		verify(voteCache).addOnlineVoteDurably("player-uuid", state);
	}

	@Test
	void uncachedStandaloneBroadcastPersistsAsBroadcastOnlyState() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote state = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false, java.util.Collections.emptyMap(),
				java.util.Map.of("Server1", "00000000-0000-0000-0000-000000000169"));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.when(voteCache.addOnlineVoteDurably("player-uuid", state)).thenReturn(true);

		assertTrue(spyProxy.persistUncachedStandaloneBroadcastForTest("player-uuid", state, false));

		assertTrue(state.isRewardDelivered());
		assertFalse(state.isBroadcastForwarded());
		verify(voteCache).addOnlineVoteDurably("player-uuid", state);
	}

	@Test
	void uncachedStandaloneBroadcastRetainsFailedDurabilityForRetry() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote state = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false, java.util.Collections.emptyMap(),
				java.util.Map.of("Server1", "00000000-0000-0000-0000-000000000169"));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.when(voteCache.addOnlineVoteDurably("player-uuid", state)).thenReturn(false);
		Mockito.when(voteCache.retainOnlineVoteForPersistenceRetry("player-uuid", state)).thenReturn(true);

		assertFalse(spyProxy.persistUncachedStandaloneBroadcastForTest("player-uuid", state, false));
		verify(voteCache).addOnlineVoteDurably("player-uuid", state);
		verify(voteCache).retainOnlineVoteForPersistenceRetry("player-uuid", state);
	}

	@Test
	void standaloneBroadcastDoesNotSendBeforeItsCanonicalStateIsDurable() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote state = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), true);
		Mockito.when(voteCache.addOnlineVoteDurably("player-uuid", state)).thenReturn(false);
		Mockito.when(voteCache.retainOnlineVoteForPersistenceRetry("player-uuid", state)).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		assertFalse(spyProxy.persistAndSendStandaloneBroadcastForTest("player-uuid", state,
				java.util.Set.of("Server1"), new java.util.LinkedHashSet<>()));

		assertTrue(spyProxy.getAttemptedVotePartyDeliveryIds().isEmpty());
		verify(voteCache).retainOnlineVoteForPersistenceRetry("player-uuid", state);
		verify(voteCache, never()).updateOnlineVote("player-uuid", state);
	}

	@Test
	void standaloneBroadcastPersistsCanonicalStateBeforeSending() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote state = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), true);
		Mockito.when(voteCache.addOnlineVoteDurably("player-uuid", state)).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote("player-uuid", state)).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		assertTrue(spyProxy.persistAndSendStandaloneBroadcastForTest("player-uuid", state,
				java.util.Set.of("Server1"), new java.util.LinkedHashSet<>()));

		assertEquals(1, spyProxy.getAttemptedVotePartyDeliveryIds().size());
		org.mockito.InOrder order = Mockito.inOrder(voteCache);
		order.verify(voteCache).addOnlineVoteDurably("player-uuid", state);
		order.verify(voteCache).updateOnlineVote("player-uuid", state);
		verify(voteCache, never()).removeOnlineVote("player-uuid", state);
	}

	@Test
	void liveVotePropagatesRetryWhenProcessingCannotBeMadeDurable() {
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getPrimaryServer()).thenReturn(true);
		votingPluginProxy.setProxyMySQL(null);

		com.bencodez.votingplugin.proxy.VotingPluginProxy.VoteRetryException failure = assertThrows(
				com.bencodez.votingplugin.proxy.VotingPluginProxy.VoteRetryException.class,
				() -> votingPluginProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001"));

		assertTrue(failure.getMessage().contains("retry is required"));
	}

	@Test
	void liveVoteRetryDoesNotApplyVotePartyTwice() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any()))
				.thenReturn(false, true, true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		spyProxy.setAvailableServers("Server1", "Server2", "Server3");
		spyProxy.vote("Player", "Service", true, false, 100L, null,
				"00000000-0000-0000-0000-000000000001", voteId);

		verify(spyProxy).addVoteParty();
		verify(voteCache, Mockito.times(3)).addServerVoteDurably(Mockito.anyString(), Mockito.any());
	}

	@Test
	void runtimeReplacementWaitsForLiveVoteRetryToSettle() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any()))
				.thenReturn(false, true, true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		IllegalStateException blocked = assertThrows(IllegalStateException.class,
				spyProxy::prepareForRuntimeReplacement);
		assertTrue(blocked.getMessage().contains("Live vote retries"));

		spyProxy.setAvailableServers("Server1", "Server2", "Server3");
		spyProxy.vote("Player", "Service", true, false, 100L, null,
				"00000000-0000-0000-0000-000000000001", voteId);

		verify(spyProxy).addVoteParty();
	}

	@Test
	void finalShutdownMovesLiveVoteRetryIntoDurableOutbox() throws Exception {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any()))
				.thenReturn(false, true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.retainOnlineVoteForPersistenceRetry(Mockito.anyString(), Mockito.any()))
				.thenReturn(true);
		Mockito.when(voteCache.retainServerVoteForPersistenceRetry(Mockito.anyString(), Mockito.any()))
				.thenReturn(true);
		Mockito.when(voteCache.retryPendingVotePersistence()).thenReturn(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		spyProxy.onDisable(false);

		verify(voteCache).retryPendingVotePersistence();
		java.lang.reflect.Field retries = VotingPluginProxy.class.getDeclaredField("liveVoteRetries");
		retries.setAccessible(true);
		assertTrue(((java.util.Map<?, ?>) retries.get(spyProxy)).isEmpty());
	}

	@Test
	void finalShutdownFencesQueuedVoteBeforeClearingLiveRetry() throws Exception {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		VoteTimeQueue queued = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", 100L, false,
				java.util.Collections.emptySet(), java.util.Collections.emptySet(), "totals", false,
				"00000000-0000-0000-0000-000000000001");
		queue.add(queued);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(false, true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.retainOnlineVoteForPersistenceRetry(Mockito.anyString(), Mockito.any()))
				.thenReturn(true);
		Mockito.when(voteCache.retainServerVoteForPersistenceRetry(Mockito.anyString(), Mockito.any()))
				.thenReturn(true);
		Mockito.when(voteCache.retryPendingVotePersistence()).thenReturn(true);
		Mockito.when(voteCache.updateTimeVote(queued)).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getPrimaryServer()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.lang.reflect.Field decider = VotingPluginProxy.class.getDeclaredField("proxyBroadcastDecider");
		decider.setAccessible(true);
		decider.set(spyProxy, new ProxyBroadcastDecider(spyProxy::getConfig, spyProxy::getAllAvailableServers,
				server -> true, server -> false));

		spyProxy.processQueue();
		assertFalse(queued.isProcessed());
		spyProxy.onDisable(false);

		assertTrue(queued.isProcessed());
		verify(voteCache).updateTimeVote(queued);
		verify(spyProxy).addVoteParty();
	}

	@Test
	void preparedRuntimeDoesNotStartAReplacementWindowVote() throws Exception {
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doNothing().when(spyProxy).addVoteParty();

		spyProxy.prepareForRuntimeReplacement();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", java.util.UUID.randomUUID()));
		verify(spyProxy, never()).addVoteParty();
		java.lang.reflect.Field retries = VotingPluginProxy.class.getDeclaredField("liveVoteRetries");
		retries.setAccessible(true);
		assertTrue(((java.util.Map<?, ?>) retries.get(spyProxy)).isEmpty());
	}

	@Test
	void durableRewardJournalOwnerRecreatesMissingServerOutbox() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote persistedOwner = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals");
		persistedOwner.setHttpDeliveryId("__vp_reward_target__:Server1",
				"00000000-0000-0000-0000-000000000199");
		OfflineBungeeVote owner = new OfflineBungeeVote(persistedOwner.getVoteId(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, false, java.util.Collections.emptySet(),
				java.util.Collections.emptySet(), false,
				OfflineBungeeVote.decodeHttpDeliveryIds(persistedOwner.encodeHttpDeliveryIds()),
				java.util.Collections.emptyMap());
		owner.setDeliveryStateDirty(false);
		Mockito.when(voteCache.retryPendingVotePersistence()).thenReturn(true);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("player-uuid"));
		Mockito.when(voteCache.getOnlineVotes("player-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(owner)));
		Mockito.when(voteCache.addServerVoteDurably(Mockito.eq("Server1"), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote("player-uuid", owner)).thenReturn(true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		spyProxy.retryPendingOnlineBroadcasts();

		assertTrue(owner.isRewardDelivered());
		verify(voteCache).addServerVoteDurably(Mockito.eq("Server1"), Mockito.argThat(vote ->
				"00000000-0000-0000-0000-000000000199".equals(vote.getHttpDeliveryId("Server1"))));
		verify(voteCache).updateOnlineVote("player-uuid", owner);
		verify(voteCache).removeOnlineVote("player-uuid", owner);
	}

	@Test
	void liveVoteRetryKeepsTotalsInputAfterVotePartyFailure() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getPrimaryServer()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		Mockito.when(proxyMySQL.containsKeyQuery(Mockito.anyString())).thenReturn(true);
		Mockito.when(proxyMySQL.getExactQuery(Mockito.any())).thenReturn(new java.util.ArrayList<>());
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		votingPluginProxy.setStandaloneBroadcastForwarding(false);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doThrow(new IllegalStateException("party persistence failed")).when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		assertThrows(IllegalArgumentException.class,
				() -> spyProxy.vote("OtherPlayer", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000002", voteId));
		spyProxy.vote("Player", "Service", true, false, 100L, null,
				"00000000-0000-0000-0000-000000000001", voteId);

		verify(spyProxy).addVoteParty();
		verify(proxyMySQL).update(Mockito.anyString(), Mockito.any(java.util.ArrayList.class));
	}

	@Test
	void liveVoteRetryDoesNotResendCompletedServerReward() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true, false, true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setStableHttpDeliveryResults(true, false, true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		spyProxy.vote("Player", "Service", true, false, 100L, null,
				"00000000-0000-0000-0000-000000000001", voteId);

		assertEquals(3, spyProxy.getAttemptedVotePartyDeliveryIds().size());
		verify(spyProxy).addVoteParty();
	}

	@Test
	void liveVoteRetryRetainsRecoveredHttpDeliveryId() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		Mockito.when(votingPluginProxy.getConfig().getSendVotesToAllServers()).thenReturn(true);
		Mockito.when(voteCache.addServerVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateServerVote(Mockito.anyString(), Mockito.any())).thenReturn(true, false, true);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any())).thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any())).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setStableHttpDeliveryResults(true, true);
		String recoveredId = "00000000-0000-0000-0000-000000000198";
		votingPluginProxy.failNextStableHttpSend(recoveredId);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		java.util.UUID voteId = java.util.UUID.randomUUID();

		assertThrows(VotingPluginProxy.VoteRetryException.class,
				() -> spyProxy.vote("Player", "Service", true, false, 100L, null,
						"00000000-0000-0000-0000-000000000001", voteId));
		spyProxy.vote("Player", "Service", true, false, 100L, null,
				"00000000-0000-0000-0000-000000000001", voteId);

		assertEquals(recoveredId, spyProxy.getAttemptedVotePartyDeliveryIds().get(2));
	}

	@Test
	void standaloneBackendCacheRowDoesNotCopyCanonicalMultiTargetState() {
		OfflineBungeeVote row = votingPluginProxy.createCachedRewardVoteForTest(
				java.util.UUID.randomUUID(), "Player", "player-uuid", "Service", 100L, true, "totals", true);

		assertTrue(row.isBroadcastForwarded());
		assertFalse(row.isProxyBroadcastHandled());
		assertTrue(row.getBroadcastTargets().isEmpty());
		assertTrue(row.getBroadcastForwardedServers().isEmpty());
		assertTrue(row.getHttpBroadcastDeliveryIds().isEmpty());
	}

	@Test
	void queuedRolloverReplayReusesStableStandaloneBroadcastDeliveryId() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		String deliveryId = "00000000-0000-0000-0000-000000000172";
		VoteTimeQueue queued = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", 100L, true,
				java.util.Set.of("Server1", "Server2"), java.util.Set.of("Server1"), "totals", false,
				"player-uuid", java.util.Map.of("Server2", deliveryId));
		queue.add(queued);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.addOnlineVoteDurably(Mockito.anyString(), Mockito.any(OfflineBungeeVote.class)))
				.thenReturn(true);
		Mockito.when(voteCache.updateOnlineVote(Mockito.anyString(), Mockito.any(OfflineBungeeVote.class)))
				.thenReturn(true);
		Mockito.when(voteCache.updateTimeVote(queued)).thenReturn(false, true);
		java.util.concurrent.atomic.AtomicInteger removeAttempts = new java.util.concurrent.atomic.AtomicInteger();
		Mockito.when(voteCache.removeTimeVote(queued)).thenAnswer(invocation ->
				removeAttempts.getAndIncrement() == 0 ? false : queue.remove(queued));
		Mockito.when(votingPluginProxy.getConfig().getPrimaryServer()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(
				Mockito.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class));
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		Mockito.doNothing().when(spyProxy).addVoteParty();
		spyProxy.processQueue();

		assertFalse(queue.isEmpty());
		assertThrows(IllegalStateException.class, spyProxy::prepareForRuntimeReplacement);
		verify(voteCache).removeTimeVote(queued);

		spyProxy.processQueue();

		assertTrue(queue.isEmpty());
		assertEquals(deliveryId, spyProxy.getAttemptedVotePartyDeliveryIds().get(0));
		assertEquals(2, spyProxy.getAttemptedVotePartyDeliveryIds().size());
		assertFalse(deliveryId.equals(spyProxy.getAttemptedVotePartyDeliveryIds().get(1)));
		verify(voteCache, Mockito.times(2)).updateTimeVote(queued);
		verify(voteCache, Mockito.times(2)).removeTimeVote(queued);
		verify(spyProxy).addVoteParty();
		verify(voteCache, never()).addOnlineVote(Mockito.anyString(), Mockito.any(OfflineBungeeVote.class));
	}

	@Test
	void durableCompletionTombstoneSkipsTimedVoteSideEffectsAfterRestart() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		VoteTimeQueue queued = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", 100L, false,
				java.util.Set.of(), java.util.Set.of(), "totals", false, "player-uuid");
		queue.add(queued);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.hasTimeVoteCompletion(queued)).thenReturn(true);
		Mockito.when(voteCache.updateTimeVote(queued)).thenReturn(true);
		Mockito.when(voteCache.removeTimeVote(queued)).thenAnswer(invocation -> queue.remove(queued));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		spyProxy.processQueue();

		assertTrue(queue.isEmpty());
		verify(spyProxy, never()).addVoteParty();
		verify(voteCache).clearTimeVoteCompletion(queued);
	}

	@Test
	void testAddVoteParty() {
		// Initial votePartyVotes should be 0
		assertEquals(0, votingPluginProxy.getVotePartyVotes());

		// Spy on the votingPluginProxy object
		VotingPluginProxy spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).checkVoteParty();

		// Add one vote party
		spyProxy.addCurrentVotePartyVotes(1);

		// Verify that votePartyVotes increased by 1
		assertEquals(1, spyProxy.getVotePartyVotes());
	}

	@Test
	void testAddCurrentVotePartyVotes() {
		// Initial votePartyVotes should be 0
		assertEquals(0, votingPluginProxy.getVotePartyVotes());

		// Add 3 votes
		votingPluginProxy.addCurrentVotePartyVotes(3);
		assertEquals(3, votingPluginProxy.getVotePartyVotes());

		// Add 2 more votes
		votingPluginProxy.addCurrentVotePartyVotes(2);
		assertEquals(5, votingPluginProxy.getVotePartyVotes());
	}

	@Test
	void rejectedHttpVotePartyRewardRemainsPendingUntilAccepted() throws Exception {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		votingPluginProxy.sendVoteParty("Server1");
		assertEquals(1, votingPluginProxy.getVoteCachePendingVotePartyRewardIds("Server1").size());
		String deliveryId = votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(0);

		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		votingPluginProxy.retryPendingVotePartyRewardsForTest();
		assertEquals(deliveryId, votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(1));
		assertEquals(1, votingPluginProxy.getVoteCachePendingVotePartyRewardIds("Server1").size());
		votingPluginProxy.acknowledgeVotePartyDeliveryForTest("server1", deliveryId);
		assertEquals(0, votingPluginProxy.getVoteCachePendingVotePartyRewardIds("Server1").size());
	}

	@Test
	void failedVotePartyAcknowledgementSaveRestoresPendingMarker() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);
		votingPluginProxy.sendVoteParty("Server1");
		String deliveryId = votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(0);

		votingPluginProxy.failNextVoteCacheSave();
		assertThrows(IllegalStateException.class,
				() -> votingPluginProxy.acknowledgeVotePartyDeliveryForTest("server1", deliveryId));
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyRewardIds("Server1").contains(deliveryId));
	}

	@Test
	void httpVotePartyStagesEveryTargetBeforeDelivery() {
		Mockito.when(votingPluginProxy.getConfig().getVotePartyEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getVotePartySendToAllServers()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getVotePartyBroadcast()).thenReturn("");
		Mockito.when(votingPluginProxy.getConfig().getVotePartyBungeeCommands()).thenReturn(java.util.List.of());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);
		votingPluginProxy.setVotePartyVotes(1);
		votingPluginProxy.setCurrentVotePartyVotesRequired(1);

		votingPluginProxy.checkVoteParty();

		assertEquals(1, votingPluginProxy.getVoteCachePendingVotePartyRewardIds("server1").size());
		assertEquals(1, votingPluginProxy.getVoteCachePendingVotePartyRewardIds("SERVER2").size());
		assertEquals(0, votingPluginProxy.getVotePartyVotes());
	}

	@Test
	void httpVotePartyRetriesJournaledProxyEffectsAfterExecutionFailure() {
		configureHttpVotePartyEffects("Party time", java.util.List.of("first", "second"));
		votingPluginProxy.failNextBroadcast();

		votingPluginProxy.checkVoteParty();

		assertEquals(0, votingPluginProxy.getVotePartyVotes());
		assertEquals("Party time", votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().broadcast());
		assertEquals(java.util.List.of("first", "second"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.getBroadcasts().isEmpty());
		assertTrue(votingPluginProxy.getConsoleCommands().isEmpty());

		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertEquals(java.util.List.of("Party time"), votingPluginProxy.getBroadcasts());
		assertEquals(java.util.List.of("first", "second"), votingPluginProxy.getConsoleCommands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	@Test
	void scheduledVotePartyEffectRetryProcessesAnotherReachedThreshold() {
		configureHttpVotePartyEffects("Party time", java.util.List.of());
		java.util.concurrent.ScheduledExecutorService scheduler = Mockito
				.mock(java.util.concurrent.ScheduledExecutorService.class);
		votingPluginProxy.setSchedulerForTest(scheduler);
		votingPluginProxy.failNextBroadcast();

		votingPluginProxy.checkVoteParty();
		votingPluginProxy.setVotePartyVotes(1);
		org.mockito.ArgumentCaptor<Runnable> retry = org.mockito.ArgumentCaptor.forClass(Runnable.class);
		verify(scheduler).schedule(retry.capture(), Mockito.eq(5L),
				Mockito.eq(java.util.concurrent.TimeUnit.SECONDS));

		retry.getValue().run();

		assertEquals(java.util.List.of("Party time", "Party time"), votingPluginProxy.getBroadcasts());
		assertEquals(0, votingPluginProxy.getVotePartyVotes());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	@Test
	void httpVotePartyRestoresExecutedEffectWhenProgressIsNotDurable() {
		configureHttpVotePartyEffects("Party time", java.util.List.of("reward all"));
		votingPluginProxy.failSaveAfterNextBroadcast();

		votingPluginProxy.checkVoteParty();

		assertEquals(java.util.List.of("Party time"), votingPluginProxy.getBroadcasts());
		assertEquals("Party time", votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().broadcast());
		assertEquals(java.util.List.of("reward all"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());

		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertEquals(java.util.List.of("Party time", "Party time"), votingPluginProxy.getBroadcasts());
		assertEquals(java.util.List.of("reward all"), votingPluginProxy.getConsoleCommands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	@Test
	void httpVotePartyPersistsOrderedCommandProgress() {
		configureHttpVotePartyEffects("", java.util.List.of("first", "second", "third"));
		votingPluginProxy.failConsoleCommand("second");

		votingPluginProxy.checkVoteParty();

		assertEquals(java.util.List.of("first"), votingPluginProxy.getConsoleCommands());
		assertEquals(java.util.List.of("second", "third"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertEquals(java.util.List.of("first", "second", "third"), votingPluginProxy.getConsoleCommands());
	}

	@Test
	void declinedVotePartyCommandRemainsPendingForRetry() {
		configureHttpVotePartyEffects("", java.util.List.of("not-yet-registered"));
		votingPluginProxy.declineNextVotePartyCommand();

		votingPluginProxy.checkVoteParty();

		assertEquals(java.util.List.of("not-yet-registered"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
		assertEquals(java.util.List.of("not-yet-registered"), votingPluginProxy.getConsoleCommands());
	}

	@Test
	void httpVotePartyKeepsCommandPendingUntilAsyncExecutionCompletesAndResumesReachedThreshold() {
		configureHttpVotePartyEffects("", java.util.List.of("async command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();

		votingPluginProxy.checkVoteParty();

		assertEquals(java.util.List.of("async command"), votingPluginProxy.getConsoleCommands());
		assertEquals(java.util.List.of("async command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		assertFalse(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		votingPluginProxy.setVotePartyVotes(1);

		completion.complete(null);

		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
		assertEquals(java.util.List.of("async command", "async command"),
				votingPluginProxy.getConsoleCommands());
		assertEquals(0, votingPluginProxy.getVotePartyVotes());
	}

	@Test
	void completedCommandWithUnpersistedProgressIsQuarantinedInsteadOfReplayed() {
		configureHttpVotePartyEffects("", java.util.List.of("async command", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();

		votingPluginProxy.checkVoteParty();
		votingPluginProxy.failNextVoteCacheSave();
		completion.complete(null);

		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertEquals(java.util.List.of("async command", "next command"), votingPluginProxy.getConsoleCommands());
		assertEquals(java.util.List.of("async command"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	@Test
	void httpVotePartyQuarantinesHungCommandWithoutRetryingIt() {
		configureHttpVotePartyEffects("", java.util.List.of("hung command", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();

		votingPluginProxy.checkVoteParty();
		votingPluginProxy.runVotePartyProxyCommandTimeoutForTest();

		assertEquals(java.util.List.of("hung command"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
		assertEquals(java.util.List.of("hung command", "next command"), votingPluginProxy.getConsoleCommands());

		completion.complete(null);
		assertEquals(java.util.List.of("hung command", "next command"), votingPluginProxy.getConsoleCommands());
		assertEquals(java.util.List.of("hung command"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
	}

	@Test
	void runtimeReplacementQuarantinesAnInFlightVotePartyCommandBeforeHandoff() {
		configureHttpVotePartyEffects("", java.util.List.of("in flight", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();
		votingPluginProxy.checkVoteParty();

		assertTrue(votingPluginProxy.quarantineInFlightVotePartyProxyCommandForReplacementForTest());

		assertEquals(java.util.List.of("in flight"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
		assertEquals(java.util.List.of("next command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		completion.complete(null);
		assertEquals(java.util.List.of("in flight"), votingPluginProxy.getConsoleCommands());
	}

	@Test
	void finalShutdownQuarantinesAnInFlightVotePartyCommandBeforeTeardown() {
		configureHttpVotePartyEffects("", java.util.List.of("in flight", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();
		votingPluginProxy.checkVoteParty();

		votingPluginProxy.onDisable(false);

		assertEquals(java.util.List.of("in flight"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
		assertEquals(java.util.List.of("next command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		completion.complete(null);
		assertEquals(java.util.List.of("in flight"), votingPluginProxy.getConsoleCommands());
	}

	@Test
	void finalShutdownRetriesTheDurableFenceBeforeTeardown() {
		configureHttpVotePartyEffects("", java.util.List.of("in flight", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();
		votingPluginProxy.checkVoteParty();
		votingPluginProxy.failNextVoteCacheSave();

		votingPluginProxy.onDisable(false);

		assertEquals(java.util.List.of("in flight"),
				votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().commands());
		assertEquals(java.util.List.of("next command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		completion.complete(null);
		assertEquals(java.util.List.of("in flight"), votingPluginProxy.getConsoleCommands());
		assertEquals(java.util.List.of("next command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
	}

	@Test
	void transportReloadRetainsHttpUntilPendingVotePartyRewardsAreAcknowledged() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteCachePendingVotePartyReward("Server1", "delivery", true);
		Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");

		votingPluginProxy.reloadFromControl();

		assertEquals(BungeeMethod.HTTP, votingPluginProxy.getMethod());
		votingPluginProxy.setVoteCachePendingVotePartyReward("Server1", "delivery", false);
		votingPluginProxy.reloadFromControl();
		assertEquals(BungeeMethod.MYSQL, votingPluginProxy.getMethod());
	}

	@Test
	void transportReloadRetainsHttpForPendingServerAndOnlineDeliveries() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote serverVote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 100L, true, "totals");
		serverVote.setHttpDeliveryId("Server1", "00000000-0000-0000-0000-000000000180");
		OfflineBungeeVote onlineVote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 101L, true, "totals");
		onlineVote.setHttpBroadcastDeliveryId("Server2", "00000000-0000-0000-0000-000000000181");
		Mockito.when(voteCache.getCachedVotesServers()).thenReturn(new String[] { "Server1" });
		Mockito.when(voteCache.getVotes("Server1"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(serverVote)));
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("uuid"));
		Mockito.when(voteCache.getOnlineVotes("uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(onlineVote)));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.setMethod(BungeeMethod.HTTP);
		Mockito.when(spyProxy.getConfig().getBungeeMethod()).thenReturn("MYSQL");

		spyProxy.reloadFromControl();

		assertEquals(BungeeMethod.HTTP, spyProxy.getMethod());
	}

	@Test
	void transportReloadRetainsHttpForPendingTimedBroadcast() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue timed = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", 100L);
		timed.setHttpBroadcastDeliveryId("Server1", "00000000-0000-0000-0000-000000000182");
		Mockito.when(voteCache.getTimeChangeQueue())
				.thenReturn(new java.util.concurrent.ConcurrentLinkedQueue<>(java.util.List.of(timed)));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.setMethod(BungeeMethod.HTTP);
		Mockito.when(spyProxy.getConfig().getBungeeMethod()).thenReturn("REDIS");

		spyProxy.reloadFromControl();

		assertEquals(BungeeMethod.HTTP, spyProxy.getMethod());
	}

	@Test
	void transportReloadRetainsHttpForOrdinaryDurableQueueEntry() throws Exception {
		HttpProxyTransportServer transport = Mockito.mock(HttpProxyTransportServer.class);
		setProxyField(votingPluginProxy, "httpTransportServer", transport);
		votingPluginProxy.setPendingHttpTransportDeliveries(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("MQTT");

		votingPluginProxy.reloadFromControl();

		assertEquals(BungeeMethod.HTTP, votingPluginProxy.getMethod());
		assertTrue(votingPluginProxy.isRetainingHttpTransportForDeferredReconciliation(),
				"a full platform reload must keep this runtime's live HTTP listener until its queue drains");
		assertTrue(votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement());
	}

	@Test
	void ordinaryFullReloadsDoNotProbeHttpRetention() {
		votingPluginProxy.setMethod(BungeeMethod.REDIS);
		Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("HTTP");
		assertFalse(votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement());

		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("HTTP");
		assertFalse(votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement());
	}

	@Test
	void changedHttpEndpointProbesRetentionButAnUnchangedListenerDoesNot() throws Exception {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		setProxyField(votingPluginProxy, "httpTransportServer", Mockito.mock(HttpProxyTransportServer.class));
		setProxyField(votingPluginProxy, "liveHttpHost", "127.0.0.1");
		setProxyField(votingPluginProxy, "liveHttpPort", 8080);
		setProxyField(votingPluginProxy, "liveHttpPublicEndpoint", "https://old.example:8080");
		Mockito.when(votingPluginProxy.getConfig().getHttpHost()).thenReturn("127.0.0.1");
		Mockito.when(votingPluginProxy.getConfig().getHttpPort()).thenReturn(8080);
		Mockito.when(votingPluginProxy.getConfig().getHttpPublicEndpoint()).thenReturn("https://old.example:8080");
		Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("HTTP");
		assertFalse(votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement());

		Mockito.when(votingPluginProxy.getConfig().getHttpPublicEndpoint()).thenReturn("https://new.example:8443");
		assertTrue(votingPluginProxy.requiresHttpRetentionCheckBeforeRuntimeReplacement());
	}

	@Test
	void deferredHttpTransportChangeReconcilesAfterTheFinalAcknowledgement() throws Exception {
		HttpProxyTransportServer transport = Mockito.mock(HttpProxyTransportServer.class);
		setProxyField(votingPluginProxy, "httpTransportServer", transport);
		java.util.concurrent.ScheduledExecutorService scheduler = java.util.concurrent.Executors
				.newSingleThreadScheduledExecutor();
		try {
			votingPluginProxy.setSchedulerForTest(scheduler);
			votingPluginProxy.setPendingHttpTransportDeliveries(true);
			votingPluginProxy.setMethod(BungeeMethod.HTTP);
			Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("REDIS");

			votingPluginProxy.reloadFromControl();
			assertEquals(BungeeMethod.HTTP, votingPluginProxy.getMethod());

			votingPluginProxy.setPendingHttpTransportDeliveries(false);
			votingPluginProxy.acknowledgeHttpDeliveryForTest("Server1", "delivery");
			long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(2);
			while (votingPluginProxy.getReloadCoreCalls() == 0 && System.nanoTime() < deadline)
				Thread.sleep(10L);

			assertEquals(1, votingPluginProxy.getReloadCoreCalls(),
					"the retained HTTP runtime must be rebuilt after its durable queue drains");
		} finally {
			scheduler.shutdownNow();
		}
	}

	@Test
	void deferredHttpEndpointChangeReconcilesAfterTheFinalAcknowledgement() throws Exception {
		HttpProxyTransportServer transport = Mockito.mock(HttpProxyTransportServer.class);
		setProxyField(votingPluginProxy, "httpTransportServer", transport);
		setProxyField(votingPluginProxy, "liveHttpHost", "old.example");
		setProxyField(votingPluginProxy, "liveHttpPort", 8080);
		setProxyField(votingPluginProxy, "liveHttpPublicEndpoint", "https://old.example:8080");
		java.util.concurrent.ScheduledExecutorService scheduler = java.util.concurrent.Executors
				.newSingleThreadScheduledExecutor();
		try {
			votingPluginProxy.setSchedulerForTest(scheduler);
			votingPluginProxy.setPendingHttpTransportDeliveries(true);
			votingPluginProxy.setMethod(BungeeMethod.HTTP);
			Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("HTTP");
			Mockito.when(votingPluginProxy.getConfig().getHttpHost()).thenReturn("new.example");
			Mockito.when(votingPluginProxy.getConfig().getHttpPort()).thenReturn(8443);
			Mockito.when(votingPluginProxy.getConfig().getHttpPublicEndpoint())
					.thenReturn("https://new.example:8443");

			votingPluginProxy.reloadFromControl();
			assertEquals(BungeeMethod.HTTP, votingPluginProxy.getMethod());

			votingPluginProxy.setPendingHttpTransportDeliveries(false);
			votingPluginProxy.acknowledgeHttpDeliveryForTest("Server1", "delivery");
			long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(2);
			while (votingPluginProxy.getReloadCoreCalls() == 0 && System.nanoTime() < deadline)
				Thread.sleep(10L);

			assertEquals(1, votingPluginProxy.getReloadCoreCalls(),
					"a changed HTTP endpoint must replace the retained runtime after its queue drains");
		} finally {
			scheduler.shutdownNow();
		}
	}

	@Test
	void deferredHttpTransportChangePollsWhenAckPrecedesQueueRemoval() throws Exception {
		HttpProxyTransportServer transport = Mockito.mock(HttpProxyTransportServer.class);
		setProxyField(votingPluginProxy, "httpTransportServer", transport);
		java.util.concurrent.ScheduledExecutorService scheduler = java.util.concurrent.Executors
				.newSingleThreadScheduledExecutor();
		try {
			votingPluginProxy.setSchedulerForTest(scheduler);
			votingPluginProxy.setPendingHttpTransportDeliveries(true);
			votingPluginProxy.setMethod(BungeeMethod.HTTP);
			Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("REDIS");

			votingPluginProxy.reloadFromControl();
			// The acknowledgement callback is intentionally observed before SimpleAPI
			// removes its entry, so the first reconciliation still sees it as pending.
			votingPluginProxy.acknowledgeHttpDeliveryForTest("Server1", "delivery");
			Thread.sleep(200L);
			assertEquals(0, votingPluginProxy.getReloadCoreCalls());

			votingPluginProxy.setPendingHttpTransportDeliveries(false);
			long deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(3);
			while (votingPluginProxy.getReloadCoreCalls() == 0 && System.nanoTime() < deadline)
				Thread.sleep(10L);

			assertEquals(1, votingPluginProxy.getReloadCoreCalls(),
					"the deferred runtime must observe queue removal even without another acknowledgement");
		} finally {
			scheduler.shutdownNow();
		}
	}

	@Test
	void concurrentManualReloadInvalidatesQueuedDeferredHttpReload() throws Exception {
		HttpProxyTransportServer transport = Mockito.mock(HttpProxyTransportServer.class);
		setProxyField(votingPluginProxy, "httpTransportServer", transport);
		java.util.concurrent.ScheduledExecutorService scheduler = java.util.concurrent.Executors
				.newSingleThreadScheduledExecutor();
		try {
			votingPluginProxy.setSchedulerForTest(scheduler);
			votingPluginProxy.setPendingHttpTransportDeliveries(true);
			votingPluginProxy.setMethod(BungeeMethod.HTTP);
			Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("REDIS");
			votingPluginProxy.reloadFromControl();

			// A direct platform reload changes the configured transport while the old
			// acknowledgement task is queued. It must invalidate that task before it
			// can enter the platform's reload coordination path.
			Mockito.when(votingPluginProxy.getConfig().getBungeeMethod()).thenReturn("HTTP");
			Thread manualReload = new Thread(votingPluginProxy::reloadFromControl);
			manualReload.start();
			manualReload.join(java.util.concurrent.TimeUnit.SECONDS.toMillis(1));
			assertFalse(manualReload.isAlive());
			Thread.sleep(300L);

			assertEquals(0, votingPluginProxy.getReloadCoreCalls(),
					"an invalidated deferred generation must not perform a stale full replacement");
		} finally {
			scheduler.shutdownNow();
		}
	}

	@Test
	void startupRetainsHttpForPersistedQueueWithoutLiveServer() throws Exception {
		java.nio.file.Path backendQueue = temporaryDirectory.resolve("http/outgoing-v1/lobby-1");
		java.nio.file.Files.createDirectories(backendQueue);
		java.nio.file.Files.writeString(backendQueue.resolve(".pending-delivery.json"), "pending");
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(temporaryDirectory.toFile()).when(spyProxy).getDataFolderPlugin();
		spyProxy.setMethod(BungeeMethod.HTTP);
		Mockito.when(spyProxy.getConfig().getBungeeMethod()).thenReturn("REDIS");

		spyProxy.reloadFromControl();

		assertEquals(BungeeMethod.HTTP, spyProxy.getMethod());
	}

	@Test
	void httpVotePartyRetainsHungCommandFenceWhenQuarantineIsNotDurable() {
		configureHttpVotePartyEffects("", java.util.List.of("hung command", "next command"));
		java.util.concurrent.CompletableFuture<Void> completion =
				votingPluginProxy.delayNextVotePartyCommandCompletion();
		votingPluginProxy.checkVoteParty();
		votingPluginProxy.failNextVoteCacheSave();

		votingPluginProxy.runVotePartyProxyCommandTimeoutForTest();

		assertEquals(java.util.List.of("hung command", "next command"),
				votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().commands());
		assertTrue(votingPluginProxy.getVoteCacheQuarantinedVotePartyProxyEffects().isEmpty());
		assertFalse(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());
		assertEquals(java.util.List.of("hung command"), votingPluginProxy.getConsoleCommands());

		completion.complete(null);
		assertEquals(java.util.List.of("hung command", "next command"), votingPluginProxy.getConsoleCommands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	@Test
	void committedHttpProxyEffectsRecoverAfterTransportModeChanges() {
		votingPluginProxy.setMethod(BungeeMethod.SOCKETS);
		votingPluginProxy.setVoteCachePendingVotePartyProxyEffects(
				new com.bencodez.votingplugin.proxy.cache.PendingVotePartyProxyEffects(
						"Party time", java.util.List.of("reward all")));

		assertTrue(votingPluginProxy.retryPendingVotePartyProxyEffectsForTest());

		assertEquals(java.util.List.of("Party time"), votingPluginProxy.getBroadcasts());
		assertEquals(java.util.List.of("reward all"), votingPluginProxy.getConsoleCommands());
		assertTrue(votingPluginProxy.getVoteCachePendingVotePartyProxyEffects().isEmpty());
	}

	private void configureHttpVotePartyEffects(String broadcast, java.util.List<String> commands) {
		Mockito.when(votingPluginProxy.getConfig().getVotePartyEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getVotePartySendToAllServers()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getVotePartyBroadcast()).thenReturn(broadcast);
		Mockito.when(votingPluginProxy.getConfig().getVotePartyBungeeCommands()).thenReturn(commands);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVotePartyVotes(1);
		votingPluginProxy.setCurrentVotePartyVotesRequired(1);
	}

	@Test
	void rolloverProjectionIncludesQueuedVotesAndVotePartyThresholds() {
		Mockito.when(votingPluginProxy.getConfig().getVotePartyEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getVotePartyIncreaseVotesRequired()).thenReturn(5);
		votingPluginProxy.setVotePartyVotes(8);
		votingPluginProxy.setCurrentVotePartyVotesRequired(10);

		int[] projected = votingPluginProxy.getProjectedVotePartyStateForTest(3);

		assertEquals(1, projected[0]);
		assertEquals(15, projected[1]);
		assertEquals(8, votingPluginProxy.getVotePartyVotes());
		assertEquals(10, votingPluginProxy.getCurrentVotePartyVotesRequired());
	}

	@Test
	void invalidVoteStopsBeforeAnyPersistentRewardCacheOrForwardingState() {
		for (String username : new String[] { "MchtNameOver16xxx", "../MchtTraversal", "Mcht/Slash", "Mcht\\Slash",
				"Mcht Space", "Mcht\tTab", "Mcht\u00E9Unicode" }) {
			VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);

			spyProxy.vote(username, "MCHT", true, true, 0, null, null);

			verify(spyProxy, never()).getUUID(Mockito.anyString());
			verify(spyProxy, never()).addVoteParty();
			verify(spyProxy, never()).getVoteCacheHandler();
			verify(spyProxy, never()).getGlobalMessageProxyHandler();
			verify(proxyMySQL, never()).containsKeyQuery(Mockito.anyString());
			verify(multiProxyHandler, never()).sendMultiProxyEnvelope(Mockito.any());
			assertEquals(0, spyProxy.getVotePartyVotes(), username);
			assertTrue(spyProxy.getWarnings().stream().anyMatch(warning -> warning.contains("Rejected vote")), username);
		}
	}

	@Test
	void immediatePluginMessageReportsActualDeliveryResult() {
		votingPluginProxy.setPluginMessageDeliveryResult(false);
		assertFalse(votingPluginProxy.sendPluginMessageImmediately("Server1",
				VotingPluginWire.voteBroadcast("uuid", "Player", "Service", 100L, "", false)));

		votingPluginProxy.setPluginMessageDeliveryResult(true);
		assertTrue(votingPluginProxy.sendPluginMessageImmediately("Server1",
				VotingPluginWire.voteBroadcast("uuid", "Player", "Service", 100L, "", false)));
	}

	@Test
	void communicationTestCompletesOnlyForTheCorrelatedBackendReply() throws Exception {
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		java.util.concurrent.ScheduledExecutorService scheduler = Mockito
				.mock(java.util.concurrent.ScheduledExecutorService.class);
		com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler messageHandler = Mockito
				.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		votingPluginProxy.setSchedulerForTest(scheduler);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(messageHandler);

		java.util.concurrent.CompletableFuture<VotingPluginProxy.CommunicationTestResult> result = votingPluginProxy
				.testBackendCommunication("Server1", 5000L);
		String requestId = votingPluginProxy.getLastCommunicationTestEnvelope().getFields()
				.get(VotingPluginWire.K_REQUEST_ID);
		assertFalse(result.isDone());

		votingPluginProxy.handleStatusOkayForTest(VotingPluginWire.statusOkay("Server2",
				java.util.UUID.fromString(requestId)));
		assertFalse(result.isDone());
		votingPluginProxy.handleStatusOkayForTest(VotingPluginWire.statusOkay("Server1",
				java.util.UUID.fromString(requestId)));

		VotingPluginProxy.CommunicationTestResult completed = result.get();
		assertTrue(completed.success());
		assertEquals("Server1", completed.server());
		assertEquals("MQTT", completed.method());
		assertTrue(completed.roundTripMillis() >= 0L);
	}

	@Test
	void communicationTestReportsUnavailableWhenTheActiveTransportCannotSend() throws Exception {
		java.util.concurrent.ScheduledExecutorService scheduler = Mockito
				.mock(java.util.concurrent.ScheduledExecutorService.class);
		votingPluginProxy.setSchedulerForTest(scheduler);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(Mockito
				.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class));
		votingPluginProxy.setCommunicationTestDeliveryResult(false);

		for (BungeeMethod unavailable : new BungeeMethod[] { BungeeMethod.MQTT, BungeeMethod.SOCKETS }) {
			votingPluginProxy.setMethod(unavailable);
			VotingPluginProxy.CommunicationTestResult result = votingPluginProxy
					.testBackendCommunication("Server1", 5000L).get();

			assertFalse(result.success());
			assertEquals("TRANSPORT_UNAVAILABLE", result.code());
			assertEquals(unavailable.name(), result.method());
		}
		Mockito.verifyNoInteractions(scheduler);
	}

	@Test
	void pluginMessagingCommunicationTestExplainsOnlinePlayerRequirement() throws Exception {
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(Mockito
				.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(false).when(spyProxy).isSomeoneOnlineServer(Mockito.eq("Server1"));

		VotingPluginProxy.CommunicationTestResult result = spyProxy.testBackendCommunication("Server1", 5000L).get();

		assertFalse(result.success());
		assertEquals("PLAYER_REQUIRED", result.code());
	}

	@Test
	void pluginMessagingLegacyLoginUsesTheProxyCurrentServerAndUuid() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(uuid).when(spyProxy).getUUID("Player");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", uuid, "claimed-backend"));

		verify(spyProxy).login("Player", uuid, "Server1");
	}

	@Test
	void pluginMessagingLegacyLoginRejectsMismatchedUuid() {
		String authoritativeUuid = java.util.UUID.randomUUID().toString();
		String claimedUuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(authoritativeUuid).when(spyProxy).getUUID("Player");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", claimedUuid, "Server1"));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void standaloneTransportLegacyLoginUsesProxyAuthoritativeRoute() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(uuid).when(spyProxy).getUUID("Player");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", uuid, "Server1"));

		verify(spyProxy).login("Player", uuid, "Server1");
	}

	@Test
	void standaloneTransportLegacyLoginRejectsUnknownServer() {
		String uuid = java.util.UUID.randomUUID().toString();
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(false).when(spyProxy).isServerValid("unknown-server");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", uuid, "unknown-server"));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void standaloneTransportLegacyLoginAcceptsModernToLegacyHandoffUsingProxyRoute() {
		java.util.UUID playerUuid = java.util.UUID.randomUUID();
		java.util.UUID incarnation = java.util.UUID.randomUUID();
		long now = System.currentTimeMillis();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server1", incarnation,
				1000L, 1000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().playerOnline("Player",
				playerUuid.toString(), "Server1", java.util.UUID.randomUUID(), incarnation, 1000L, 1100L,
				now));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn("Server2").when(spyProxy).getCurrentPlayerServer("Player");
		Mockito.doReturn(playerUuid.toString()).when(spyProxy).getUUID("Player");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(
				VotingPluginWire.login("Player", playerUuid.toString(), "Server2"));

		verify(spyProxy).login("Player", playerUuid.toString(), "Server2");
	}

	@Test
	void standaloneTransportLegacyLoginRejectsClaimNotMatchingProxyRoute() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(uuid).when(spyProxy).getUUID("Player");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", uuid, "Server2"));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void standaloneTransportLegacyLoginRejectsNameUuidMismatch() {
		String aliceUuid = java.util.UUID.randomUUID().toString();
		String bobUuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(aliceUuid).when(spyProxy).getUUID("Alice");
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Alice", bobUuid, "Server1"));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void dedicatedProxyLegacyLoginRequiresConfirmedDestinationPresence() {
		java.util.UUID playerUuid = java.util.UUID.randomUUID();
		java.util.UUID incarnation = java.util.UUID.randomUUID();
		long now = System.currentTimeMillis();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getDedicatedVotingProxy()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server1", incarnation,
				1000L, 1000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().playerOnline("Player",
				playerUuid.toString(), "Server1", java.util.UUID.randomUUID(), incarnation, 1000L, 1100L,
				now));
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(
				VotingPluginWire.login("Player", playerUuid.toString(), "Server2"));
		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(
				VotingPluginWire.login("Player", playerUuid.toString(), "Server1"));
		verify(spyProxy).login("Player", playerUuid.toString(), "Server1");
	}

	@Test
	void dedicatedProxyLegacyLoginRejectsUnknownIdentity() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getDedicatedVotingProxy()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", uuid, "Server1"));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void pluginMessagingIgnoresExtendedPresenceLogin() {
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
		java.util.UUID connectionId = java.util.UUID.randomUUID();
		java.util.UUID incarnationId = java.util.UUID.randomUUID();

		spyProxy.handleLoginMessageForTest(VotingPluginWire.login("Player",
				java.util.UUID.randomUUID().toString(), "survival", connectionId, incarnationId, 1000L, 1100L));

		verify(spyProxy, never()).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
		assertEquals(0, spyProxy.getBackendPlayerPresenceTracker().getOnlinePlayerCount());
	}

	@Test
	void httpModernPresenceRejectsAuthenticatedBackendThatDoesNotMatchProxyRoute() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(uuid).when(spyProxy).getUUID("Player");
		var handler = Mockito.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		spyProxy.setGlobalMessageProxyHandlerForTest(handler);
		JsonEnvelope envelope = VotingPluginWire.login("Player", uuid, "Server2", java.util.UUID.randomUUID(),
				java.util.UUID.randomUUID(), 1000L, 1100L);

		spyProxy.handleHttpTransportEnvelopeForTest(new com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer.ReceivedEnvelope(
				"Server2", java.util.UUID.randomUUID().toString(), envelope));

		verify(handler, never()).onMessage(Mockito.any());
		assertEquals(0, spyProxy.getBackendPlayerPresenceTracker().getOnlinePlayerCount());
	}

	@Test
	void httpModernPresenceAcceptsAuthenticatedBackendMatchingProxyRouteAndUuid() {
		String uuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(uuid).when(spyProxy).getUUID("Player");
		var handler = Mockito.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		spyProxy.setGlobalMessageProxyHandlerForTest(handler);
		JsonEnvelope envelope = VotingPluginWire.login("Player", uuid, "Server1", java.util.UUID.randomUUID(),
				java.util.UUID.randomUUID(), 1000L, 1100L);

		spyProxy.handleHttpTransportEnvelopeForTest(new com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer.ReceivedEnvelope(
				"Server1", java.util.UUID.randomUUID().toString(), envelope));

		verify(handler).onMessage(envelope);
	}

	@Test
	void httpModernPresenceRejectsUuidThatDoesNotMatchProxyPlayer() {
		String authoritativeUuid = java.util.UUID.randomUUID().toString();
		String claimedUuid = java.util.UUID.randomUUID().toString();
		Mockito.when(votingPluginProxy.getConfig().getOnlineMode()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(authoritativeUuid).when(spyProxy).getUUID("Player");
		var handler = Mockito.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		spyProxy.setGlobalMessageProxyHandlerForTest(handler);
		JsonEnvelope envelope = VotingPluginWire.login("Player", claimedUuid, "Server1", java.util.UUID.randomUUID(),
				java.util.UUID.randomUUID(), 1000L, 1100L);

		spyProxy.handleHttpTransportEnvelopeForTest(new com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer.ReceivedEnvelope(
				"Server1", java.util.UUID.randomUUID().toString(), envelope));

		verify(handler, never()).onMessage(Mockito.any());
		assertEquals(0, spyProxy.getBackendPlayerPresenceTracker().getOnlinePlayerCount());
	}

	@Test
	void httpEnvelopeRejectsServerFieldThatDoesNotMatchAuthenticatedBackend() {
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		var handler = Mockito.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(handler);
		JsonEnvelope envelope = JsonEnvelope.builder("vote").put(VotingPluginWire.K_SERVER, "Server2").build();

		votingPluginProxy.handleHttpTransportEnvelopeForTest(new com.bencodez.simpleapi.servercomm.http.HttpProxyTransportServer.ReceivedEnvelope(
				"Server1", java.util.UUID.randomUUID().toString(), envelope));

		verify(handler, never()).onMessage(Mockito.any());
	}

	@Test
	void dedicatedVotingProxyRoutesUsingConfirmedBackendPresence() {
		Mockito.when(votingPluginProxy.getConfig().getDedicatedVotingProxy()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		long now = System.currentTimeMillis();
		java.util.UUID incarnation = java.util.UUID.randomUUID();
		java.util.UUID playerUuid = java.util.UUID.randomUUID();

		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server2", incarnation,
				1000L, 1000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().playerOnline("Player", playerUuid.toString(),
				"Server2", java.util.UUID.randomUUID(), incarnation, 1000L, 1100L, now));

		assertTrue(votingPluginProxy.isPlayerOnlineForVoteRoutingForTest("Player"));
		assertEquals("Server2", votingPluginProxy.getCurrentPlayerServerForVoteRoutingForTest("Player"));
		assertTrue(votingPluginProxy.isSomeoneOnlineServerForVoteRoutingForTest("Server2"));
		assertFalse(votingPluginProxy.isPlayerOnlineForVoteRoutingForTest("Unknown"));
	}

	@Test
	void dedicatedVotingProxyDoesNotUsePluginMessagingPresence() {
		Mockito.when(votingPluginProxy.getConfig().getDedicatedVotingProxy()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);

		assertTrue(votingPluginProxy.isPlayerOnlineForVoteRoutingForTest("Player"));
		assertEquals("Server1", votingPluginProxy.getCurrentPlayerServerForVoteRoutingForTest("Player"));
	}

	@Test
	void dedicatedSnapshotDrainsCachedVotesForConfirmedPlayers() {
		Mockito.when(votingPluginProxy.getConfig().getDedicatedVotingProxy()).thenReturn(true);
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		long now = System.currentTimeMillis();
		java.util.UUID incarnation = java.util.UUID.randomUUID();
		java.util.UUID playerUuid = java.util.UUID.randomUUID();
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server2", incarnation,
				1000L, 1000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().playerOnline("Player", playerUuid.toString(),
				"Server2", java.util.UUID.randomUUID(), incarnation, 1000L, 1100L, now));

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		doNothing().when(spyProxy).login(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
		spyProxy.processDedicatedSnapshotLoginsForTest("Server2", java.util.Collections.emptySet());

		verify(spyProxy).login("Player", playerUuid.toString(), "Server2");
	}

	@Test
	void handoffBlockedBySnapshotCooldownIsRetried() {
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler messageHandler = Mockito
				.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(messageHandler);
		long now = System.currentTimeMillis();
		java.util.UUID sourceIncarnation = java.util.UUID.randomUUID();
		java.util.UUID destinationIncarnation = java.util.UUID.randomUUID();
		java.util.UUID playerUuid = java.util.UUID.randomUUID();
		java.util.UUID sourceConnection = java.util.UUID.randomUUID();
		java.util.UUID destinationConnection = java.util.UUID.randomUUID();
		java.util.UUID cooldownRequest = java.util.UUID.randomUUID();

		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server1", sourceIncarnation,
				1000L, 1000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().backendStarted("Server2",
				destinationIncarnation, 2000L, 2000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().playerOnline("Player", playerUuid.toString(),
				"Server1", sourceConnection, sourceIncarnation, 1000L, 1100L, now));
		assertEquals(cooldownRequest, votingPluginProxy.getBackendPlayerPresenceTracker().beginSnapshot("Server2",
				cooldownRequest, destinationIncarnation, 2000L, now));
		assertTrue(votingPluginProxy.getBackendPlayerPresenceTracker().applySnapshotChunk("Server2", cooldownRequest,
				0, 1, java.util.List.of(), destinationIncarnation, 2000L, 2100L, now));

		votingPluginProxy.handleLoginMessageForTest(VotingPluginWire.login("Player", playerUuid.toString(), "Server2",
				destinationConnection, destinationIncarnation, 2000L, 2200L));

		assertEquals(1, votingPluginProxy.getPendingPresenceHandoffCountForTest());
		verify(messageHandler, never()).sendMessage(Mockito.anyString(), Mockito.anyInt(), Mockito.any());

		votingPluginProxy.retryPendingPresenceHandoffsForTest(now + 30001L);

		verify(messageHandler).sendMessage(Mockito.eq("Server2"), Mockito.eq(1), Mockito.any());
		assertEquals(1, votingPluginProxy.getPendingPresenceHandoffCountForTest());
	}

	@Test
	void presenceTransportRequestsBackendResyncFiveSecondsAfterProxyStart() {
		votingPluginProxy.setMethod(BungeeMethod.MQTT);
		java.util.concurrent.ScheduledExecutorService scheduler = Mockito
				.mock(java.util.concurrent.ScheduledExecutorService.class);
		com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler messageHandler = Mockito
				.mock(com.bencodez.simpleapi.servercomm.global.GlobalMessageProxyHandler.class);
		votingPluginProxy.setSchedulerForTest(scheduler);
		votingPluginProxy.setGlobalMessageProxyHandlerForTest(messageHandler);
		org.mockito.ArgumentCaptor<Runnable> task = org.mockito.ArgumentCaptor.forClass(Runnable.class);

		votingPluginProxy.scheduleBackendPresenceStartupResyncForTest();

		verify(scheduler).schedule(task.capture(), Mockito.eq(5L),
				Mockito.eq(java.util.concurrent.TimeUnit.SECONDS));
		task.getValue().run();

		org.mockito.ArgumentCaptor<String> targets = org.mockito.ArgumentCaptor.forClass(String.class);
		org.mockito.ArgumentCaptor<com.bencodez.simpleapi.servercomm.codec.JsonEnvelope> envelopes = org.mockito.ArgumentCaptor
				.forClass(com.bencodez.simpleapi.servercomm.codec.JsonEnvelope.class);
		verify(messageHandler, Mockito.times(2)).sendMessage(targets.capture(), Mockito.anyInt(), envelopes.capture());
		assertEquals(java.util.Set.of("Server1", "Server2"), new java.util.HashSet<>(targets.getAllValues()));
		for (com.bencodez.simpleapi.servercomm.codec.JsonEnvelope envelope : envelopes.getAllValues()) {
			VotingPluginWire.PresenceResyncRequest request = VotingPluginWire.readPresenceResyncRequest(envelope);
			assertEquals(VotingPluginWire.SUB_PRESENCE_RESYNC_REQUEST, envelope.getSubChannel());
			assertTrue(java.util.Set.of("Server1", "Server2").contains(request.server));
			assertTrue(request.requestId != null);
			assertTrue(request.requestedAt > 0L);
		}
	}

	@Test
	void pluginMessagingDoesNotSchedulePresenceStartupResync() {
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		java.util.concurrent.ScheduledExecutorService scheduler = Mockito
				.mock(java.util.concurrent.ScheduledExecutorService.class);
		votingPluginProxy.setSchedulerForTest(scheduler);

		votingPluginProxy.scheduleBackendPresenceStartupResyncForTest();

		Mockito.verifyNoInteractions(scheduler);
	}

	@Test
	void standaloneMysqlBroadcastReportsTransportFailure() throws Exception {
		MySqlMessenger messenger = Mockito.mock(MySqlMessenger.class);
		Mockito.doThrow(new java.sql.SQLException("send failed")).when(messenger)
				.sendToBackend(Mockito.eq("Server1"), Mockito.any());
		votingPluginProxy.setMethod(BungeeMethod.MYSQL);
		votingPluginProxy.setProxyMysqlMessenger(messenger);

		assertFalse(votingPluginProxy.sendProxyBroadcastImmediately("Server1",
				VotingPluginWire.voteBroadcast("uuid", "Player", "Service", 100L, "", false)));
	}

	@Test
	void rolloverCompletesBeforeVoteDataLoadsAndRejectedVoteDoesNotBroadcast() {
		votingPluginProxy.setPlayerOnline(false);
		Mockito.when(votingPluginProxy.getConfig().getBungeeManageTotals()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getGlobalDataEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastEnabled()).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getProxyBroadcastOfflineMode()).thenReturn("FORWARD");
		Mockito.when(proxyMySQL.containsKeyQuery(Mockito.anyString())).thenReturn(true);
		Mockito.when(proxyMySQL.getExactQuery(Mockito.any())).thenReturn(new java.util.ArrayList<>());
		Mockito.when(globalDataHandler.isTimeChangedHappened()).thenReturn(true, false);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(false).when(spyProxy).checkVoteDelay(Mockito.anyString(), Mockito.anyString(),
				Mockito.anyString(), Mockito.any(), Mockito.anyBoolean());

		spyProxy.vote("Player", "Service", true, true, 0, null, null);

		org.mockito.InOrder order = Mockito.inOrder(globalDataHandler, proxyMySQL, spyProxy);
		order.verify(globalDataHandler).isTimeChangedHappened();
		order.verify(globalDataHandler).checkForFinishedTimeChanges();
		order.verify(globalDataHandler).isTimeChangedHappened();
		order.verify(proxyMySQL).getExactQuery(Mockito.any());
		order.verify(spyProxy).checkVoteDelay(Mockito.anyString(), Mockito.eq("Player"), Mockito.eq("Service"),
				Mockito.any(), Mockito.eq(true));
		verify(spyProxy, never()).sendPluginMessageData(Mockito.anyString(), Mockito.anyString(), Mockito.any(),
				Mockito.anyBoolean());
	}

	@Test
	void acceptedTimeChangeVoteReservesItsDelaySlot() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", System.currentTimeMillis()));
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.getVotes(Mockito.anyString())).thenReturn(new java.util.ArrayList<>());

		Mockito.when(votingPluginProxy.getConfig().getWaitUntilVoteDelaySites()).thenReturn(java.util.List.of("Site"));
		Mockito.when(votingPluginProxy.getConfig().getWaitUntilVoteDelayService("Site")).thenReturn("Service");
		Mockito.when(votingPluginProxy.getConfig().getWaitUntilVoteDelayVoteDelay("Site")).thenReturn(1);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		assertFalse(spyProxy.checkVoteDelay("player-uuid", "Player", "Service", new java.util.ArrayList<>(), true));
		assertTrue(spyProxy.checkVoteDelay("player-uuid", "Player", "Service", new java.util.ArrayList<>(), false));
	}

	@Test
	void terminalRolloverReplayIsRemovedBeforeLaterEntries() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		VoteTimeQueue invalid = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Invalid\\Service", 100L);
		queue.add(invalid);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.removeTimeVote(invalid)).thenAnswer(invocation -> queue.remove(invalid));

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		spyProxy.processQueue();

		assertTrue(queue.isEmpty());
		verify(voteCache).removeTimeVote(invalid);
	}

	@Test
	void durablyProcessedRolloverVoteIsDeletedWithoutReplay() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		VoteTimeQueue processed = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Invalid\\Service", 100L,
				false, java.util.Collections.emptySet(), java.util.Collections.emptySet(), "totals", true);
		queue.add(processed);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.removeTimeVote(processed)).thenAnswer(invocation -> {
			queue.remove(processed);
			return true;
		});

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		spyProxy.processQueue();

		assertTrue(queue.isEmpty());
		verify(spyProxy, never()).getUUID(Mockito.anyString());
		verify(voteCache).removeTimeVote(processed);
	}

	@Test
	void processedRolloverVoteIsRetainedUntilStableHttpBroadcastDeliveryCompletes() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		String deliveryId = "00000000-0000-0000-0000-000000000167";
		VoteTimeQueue processed = new VoteTimeQueue(java.util.UUID.randomUUID(), "Player", "Service", 100L, true,
				java.util.Set.of("Server1"), java.util.Collections.emptySet(), "totals", true, "player-uuid",
				java.util.Map.of("Server1", deliveryId));
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(processed);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.updateTimeVote(processed)).thenReturn(true);
		Mockito.when(voteCache.removeTimeVote(processed)).thenAnswer(invocation -> queue.remove(processed));
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.processQueue();

		assertTrue(queue.isEmpty());
		assertFalse(processed.hasPendingHttpBroadcastDeliveryIds());
		verify(voteCache).updateTimeVote(processed);
		verify(voteCache).removeTimeVote(processed);
		verify(spyProxy, never()).getUUID(Mockito.anyString());
	}

	@Test
	void pendingServerBroadcastRetriesBeforeOfflineRewardDelivery() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false);
		Mockito.when(voteCache.hasVotes("Server1")).thenReturn(true);
		Mockito.when(voteCache.getVotes("Server1"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateServerVote("Server1", vote)).thenReturn(true);

		votingPluginProxy.setPlayerOnline(false);
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());
		Mockito.when(votingPluginProxy.getConfig().getWaitForUserOnline()).thenReturn(true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.checkCachedVotes("Server1");

		assertTrue(vote.isProxyBroadcastComplete());
		assertTrue(vote.isBroadcastForwarded());
		assertFalse(vote.isRewardDelivered());
		verify(voteCache).updateServerVote("Server1", vote);
	}

	@Test
	void acceptedCachedRewardRetainsAmbiguousStandaloneBroadcastWithoutRebroadcasting() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false, java.util.Collections.emptyMap(),
				java.util.Map.of("Server1", "00000000-0000-0000-0000-000000000170"));
		Mockito.when(voteCache.hasVotes("Server1")).thenReturn(true);
		Mockito.when(voteCache.getVotes("Server1"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateServerVote("Server1", vote)).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		votingPluginProxy.setStableHttpDeliveryResults(false, true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.checkCachedVotes("Server1");

		assertTrue(vote.isRewardDelivered());
		assertFalse(vote.isProxyBroadcastComplete());
		assertEquals("false", spyProxy.getLastVoteEnvelope().getFields()
				.get(VotingPluginWire.K_BUNGEE_BROADCAST));
		verify(voteCache).updateServerVote("Server1", vote);
		verify(voteCache).removeServerVotes(Mockito.eq("Server1"), Mockito.argThat(java.util.List::isEmpty));
	}

	@Test
	void rejectedHttpQueueDeliveryRetainsCachedServerVote() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals");
		Mockito.when(voteCache.hasVotes("Server1")).thenReturn(true);
		Mockito.when(voteCache.getVotes("Server1"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.checkCachedVotes("Server1");

		verify(voteCache).removeServerVotes(Mockito.eq("Server1"),
				Mockito.argThat(java.util.List::isEmpty));
	}

	@Test
	void rejectedHttpQueueDeliveryRetainsCachedOnlineVote() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals");
		Mockito.when(voteCache.hasOnlineVotes("player-uuid")).thenReturn(true);
		Mockito.when(voteCache.getOnlineVotes("player-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.checkOnlineVotes("Player", "player-uuid", "Server1");

		assertFalse(vote.isRewardDelivered());
		verify(voteCache, never()).removeOnlineVote("player-uuid", vote);
		verify(multiProxyHandler, never()).sendClearVote(Mockito.anyString(), Mockito.anyString());
	}

	@Test
	void acceptedOnlineRewardRetainsAmbiguousStandaloneBroadcastWithoutRebroadcasting() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "player-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false, java.util.Collections.emptyMap(),
				java.util.Map.of("Server1", "00000000-0000-0000-0000-000000000171"));
		Mockito.when(voteCache.hasOnlineVotes("player-uuid")).thenReturn(true);
		Mockito.when(voteCache.getOnlineVotes("player-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);
		votingPluginProxy.setStableHttpDeliveryResults(false, true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.checkOnlineVotes("Player", "player-uuid", "Server1");

		assertTrue(vote.isRewardDelivered());
		assertFalse(vote.isProxyBroadcastComplete());
		assertEquals("false", spyProxy.getLastVoteEnvelope().getFields()
				.get(VotingPluginWire.K_BUNGEE_BROADCAST));
		verify(voteCache, never()).removeOnlineVote("player-uuid", vote);
	}

	@Test
	void pendingOnlineBroadcastRetriesWhenTargetGainsAnyCarrier() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("voter-uuid"));
		Mockito.when(voteCache.getOnlineVotes("voter-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateOnlineVote("voter-uuid", vote)).thenReturn(true);

		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingOnlineBroadcastsForTest("Server1");

		assertTrue(vote.isProxyBroadcastComplete());
		assertTrue(vote.isBroadcastForwarded());
		assertFalse(vote.isRewardDelivered());
		verify(voteCache).updateOnlineVote("voter-uuid", vote);
	}

	@Test
	void periodicRetryDeliversBrokerBackedOnlineCacheWithoutVoterLogin() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1", "Server2"),
				java.util.Collections.emptySet(), false);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("voter-uuid"));
		Mockito.when(voteCache.getOnlineVotes("voter-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateOnlineVote("voter-uuid", vote)).thenReturn(true);

		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingOnlineBroadcasts();

		assertTrue(vote.isProxyBroadcastComplete());
		assertEquals(java.util.Set.of("Server1", "Server2"), vote.getBroadcastForwardedServers());
		verify(voteCache).updateOnlineVote("voter-uuid", vote);
	}

	@Test
	void rejectedPeriodicHttpBroadcastKeepsItsDeterministicIdentity() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("voter-uuid"));
		Mockito.when(voteCache.getOnlineVotes("voter-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateOnlineVote("voter-uuid", vote)).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingOnlineBroadcasts();

		assertEquals(null, vote.getHttpBroadcastDeliveryId("Server1"));
		verify(voteCache, never()).updateOnlineVote("voter-uuid", vote);
	}

	@Test
	void rejectedCarrierHttpBroadcastKeepsItsDeterministicIdentity() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("voter-uuid"));
		Mockito.when(voteCache.getOnlineVotes("voter-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));
		Mockito.when(voteCache.updateOnlineVote("voter-uuid", vote)).thenReturn(true);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(false);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingOnlineBroadcastsForTest("Server1");

		assertEquals(null, vote.getHttpBroadcastDeliveryId("Server1"));
		verify(voteCache, never()).updateOnlineVote("voter-uuid", vote);
	}

	@Test
	void timedBroadcastRetriesWhileRolloverIsStillActive() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L, true,
				java.util.Set.of("Server1"), java.util.Collections.emptySet(), "totals", false, "voter-uuid",
				java.util.Map.of("Server1", "stale-http-delivery-id"));
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(vote);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingTimeBroadcastsForTest("Server1");

		assertEquals(java.util.Set.of("Server1"), vote.getBroadcastForwardedServers());
		assertEquals(null, vote.getHttpBroadcastDeliveryId("Server1"));
		verify(voteCache).updateTimeVote(vote);
	}

	@Test
	void timedHttpBroadcastDoesNotSendUntilItsNewDeliveryIdIsPersisted() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L, true,
				java.util.Set.of("Server1"), java.util.Collections.emptySet(), "totals", false, "voter-uuid");
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(vote);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(voteCache.updateTimeVote(vote)).thenReturn(false);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingTimeBroadcastsForTest("Server1");

		assertTrue(vote.getHttpBroadcastDeliveryId("Server1") != null);
		assertTrue(spyProxy.getAttemptedVotePartyDeliveryIds().isEmpty());
	}

	@Test
	void timedHttpBroadcastPersistsIdBeforeSendAndReusesItAfterPostAcceptCrash() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L, true,
				java.util.Set.of("Server1"), java.util.Collections.emptySet(), "totals", false, "voter-uuid");
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(vote);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		java.util.concurrent.atomic.AtomicReference<String> persistedDeliveryId = new java.util.concurrent.atomic.AtomicReference<>();
		Mockito.when(voteCache.updateTimeVote(vote)).thenAnswer(invocation -> {
			if (persistedDeliveryId.get() == null) {
				assertTrue(spyProxy.getAttemptedVotePartyDeliveryIds().isEmpty());
				persistedDeliveryId.set(vote.getHttpBroadcastDeliveryId("Server1"));
				return true;
			}
			throw new IllegalStateException("simulated crash after HTTP acceptance");
		});

		assertThrows(IllegalStateException.class, () -> spyProxy.retryPendingTimeBroadcastsForTest("Server1"));
		String deliveryId = persistedDeliveryId.get();
		assertTrue(deliveryId != null && !deliveryId.isBlank());
		assertEquals(java.util.List.of(deliveryId), spyProxy.getAttemptedVotePartyDeliveryIds());

		VoteTimeQueue restored = new VoteTimeQueue(vote.getVoteId(), vote.getName(), vote.getService(), vote.getTime(),
				vote.isProxyBroadcastHandled(), vote.getBroadcastTargets(), java.util.Collections.emptySet(),
				vote.getTotals(), vote.isProcessed(), vote.getUuid(),
				java.util.Map.of("Server1", deliveryId));
		java.util.Queue<VoteTimeQueue> restoredQueue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		restoredQueue.add(restored);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(restoredQueue);
		Mockito.when(voteCache.updateTimeVote(restored)).thenReturn(true);
		spyProxy.setVoteEnvelopeDeliveryResult(true);
		spyProxy.retryPendingTimeBroadcastsForTest("Server1");

		assertTrue(restored.getBroadcastForwardedServers().contains("Server1"));
		assertEquals(null, restored.getHttpBroadcastDeliveryId("Server1"));
		assertEquals(java.util.List.of(deliveryId, deliveryId), votingPluginProxy.getAttemptedVotePartyDeliveryIds());
	}

	@Test
	void timedHttpBroadcastPersistsEachAcceptedTargetBeforePreparingTheNext() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L, true,
				new java.util.LinkedHashSet<>(java.util.List.of("Server1", "Server2")),
				java.util.Collections.emptySet(), "totals", false, "voter-uuid");
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(vote);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.HTTP);
		votingPluginProxy.setVoteEnvelopeDeliveryResult(true);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		java.util.concurrent.atomic.AtomicReference<VoteTimeQueue> persistedBeforeSecondSend =
				new java.util.concurrent.atomic.AtomicReference<>();
		Mockito.when(voteCache.updateTimeVote(vote)).thenAnswer(invocation -> {
			if (vote.getHttpBroadcastDeliveryId("Server2") != null) {
				persistedBeforeSecondSend.set(new VoteTimeQueue(vote.getVoteId(), vote.getName(), vote.getService(),
						vote.getTime(), vote.isProxyBroadcastHandled(), vote.getBroadcastTargets(),
						vote.getBroadcastForwardedServers(), vote.getTotals(), vote.isProcessed(), vote.getUuid(),
						vote.getHttpBroadcastDeliveryIds()));
				throw new IllegalStateException("simulated crash after persisting the second target ID");
			}
			return true;
		});

		assertThrows(IllegalStateException.class, spyProxy::retryPendingTimeBroadcastsForTest);
		VoteTimeQueue restored = persistedBeforeSecondSend.get();
		assertTrue(restored.getBroadcastForwardedServers().contains("Server1"));
		assertFalse(restored.getBroadcastForwardedServers().contains("Server2"));
		String secondDeliveryId = restored.getHttpBroadcastDeliveryId("Server2");
		assertTrue(secondDeliveryId != null && !secondDeliveryId.isBlank());

		java.util.Queue<VoteTimeQueue> restoredQueue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		restoredQueue.add(restored);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(restoredQueue);
		Mockito.when(voteCache.updateTimeVote(restored)).thenReturn(true);
		spyProxy.retryPendingTimeBroadcastsForTest();

		assertEquals(2, votingPluginProxy.getAttemptedVotePartyDeliveryIds().size());
		assertEquals(secondDeliveryId, votingPluginProxy.getAttemptedVotePartyDeliveryIds().get(1));
	}

	@Test
	void failedTimedDeliveryStatePersistenceRemainsDirtyUntilRetrySucceeds() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L);
		Mockito.when(voteCache.updateTimeVote(vote)).thenReturn(false, true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		assertFalse(spyProxy.persistTimeVoteDeliveryForTest(vote));
		assertTrue(vote.isDeliveryStateDirty());
		assertTrue(spyProxy.persistTimeVoteDeliveryForTest(vote));
		assertFalse(vote.isDeliveryStateDirty());
		verify(voteCache, Mockito.times(2)).updateTimeVote(vote);
	}

	@Test
	void cachedDeliveryStatePersistenceRetriesForServerAndOnlineCaches() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote serverVote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 100L, true, "totals");
		OfflineBungeeVote onlineVote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "Player", "uuid",
				"Service", 101L, true, "totals");
		Mockito.when(voteCache.updateServerVote("Server1", serverVote)).thenReturn(false, true);
		Mockito.when(voteCache.updateOnlineVote("uuid", onlineVote)).thenReturn(false, true);
		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();

		assertFalse(spyProxy.persistServerVoteDeliveryForTest("Server1", serverVote));
		assertTrue(serverVote.isDeliveryStateDirty());
		assertTrue(spyProxy.persistServerVoteDeliveryForTest("Server1", serverVote));
		assertFalse(serverVote.isDeliveryStateDirty());
		assertFalse(spyProxy.persistOnlineVoteDeliveryForTest("uuid", onlineVote));
		assertTrue(onlineVote.isDeliveryStateDirty());
		assertTrue(spyProxy.persistOnlineVoteDeliveryForTest("uuid", onlineVote));
		assertFalse(onlineVote.isDeliveryStateDirty());
	}

	@Test
	void standaloneForwardingRequiresProxySideVoteValidation() {
		assertTrue(votingPluginProxy.canForwardStandaloneBroadcastForTest(true));
		assertFalse(votingPluginProxy.canForwardStandaloneBroadcastForTest(false));
	}

	@Test
	void completedBroadcastOnlyOnlineVoteIsRemovedAfterRetry() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), true);
		Mockito.when(voteCache.getOnlineVoteUUIDs()).thenReturn(java.util.Set.of("voter-uuid"));
		Mockito.when(voteCache.getOnlineVotes("voter-uuid"))
				.thenReturn(new java.util.ArrayList<>(java.util.List.of(vote)));

		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers())
				.thenReturn(java.util.Collections.emptyList());

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingOnlineBroadcastsForTest("Server1");

		assertTrue(vote.isProxyBroadcastComplete());
		verify(voteCache).removeOnlineVote("voter-uuid", vote);
		verify(voteCache, never()).updateOnlineVote("voter-uuid", vote);
	}

	private static void setProxyField(VotingPluginProxy target, String name, Object value) throws Exception {
		java.lang.reflect.Field field = VotingPluginProxy.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(target, value);
	}
}
