
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
	void pendingOnlineBroadcastRetriesWhenTargetGainsAnyCarrier() {
		VoteCacheHandler voteCache = Mockito.mock(VoteCacheHandler.class);
		OfflineBungeeVote vote = new OfflineBungeeVote(java.util.UUID.randomUUID(), "OfflineVoter", "voter-uuid",
				"Service", 100L, true, "totals", false, true, java.util.Set.of("Server1"),
				java.util.Collections.emptySet(), false);
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
