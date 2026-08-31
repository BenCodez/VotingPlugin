
package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.bencodez.advancedcore.bungeeapi.globaldata.GlobalDataHandlerProxy;
import com.bencodez.simpleapi.servercomm.mysql.MySqlMessenger;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.ProxyMysqlUserTable;
import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.proxy.VotingPluginWire;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.proxy.multiproxy.MultiProxyHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

public class VotingPluginProxyTest {

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
		verify(voteCache).addOnlineVote("player-uuid", vote);
		verify(multiProxyHandler, never()).sendClearVote(Mockito.anyString(), Mockito.anyString());
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
	void timedBroadcastRetriesWhileRolloverIsStillActive() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "OfflineVoter", "Service", 100L, true,
				java.util.Set.of("Server1"), java.util.Collections.emptySet(), "totals", false, "voter-uuid");
		java.util.Queue<VoteTimeQueue> queue = new java.util.concurrent.ConcurrentLinkedQueue<>();
		queue.add(vote);
		Mockito.when(voteCache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(votingPluginProxy.getConfig().getBlockedServers()).thenReturn(java.util.Collections.emptyList());
		votingPluginProxy.setMethod(BungeeMethod.PLUGINMESSAGING);

		VotingPluginProxyTestImpl spyProxy = Mockito.spy(votingPluginProxy);
		Mockito.doReturn(voteCache).when(spyProxy).getVoteCacheHandler();
		spyProxy.retryPendingTimeBroadcastsForTest("Server1");

		assertEquals(java.util.Set.of("Server1"), vote.getBroadcastForwardedServers());
		verify(voteCache).updateTimeVote(vote);
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
}
