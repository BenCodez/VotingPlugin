package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.proxy.OfflineBungeeVote;
import com.bencodez.votingplugin.proxy.cache.DataNode;
import com.bencodez.votingplugin.proxy.cache.IVoteCache;
import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

/**
 * Regression tests for proxy vote identity across caches and delayed processing.
 */
public class VoteCacheHandlerVoteIdTest {

	private IVoteCache storage;
	private VoteCacheHandler handler;

	@BeforeEach
	public void setUp() {
		storage = mock(IVoteCache.class);
		when(storage.getServerVotes("server")).thenReturn(Collections.emptyList());
		when(storage.getOnlineVotes("player-uuid")).thenReturn(Collections.emptyList());
		handler = newHandler(storage);
	}

	@Test
	public void serverCacheRejectsDuplicateVoteId() {
		UUID voteId = UUID.randomUUID();

		handler.addServerVote("server", vote(voteId, 100L));
		handler.addServerVote("server", vote(voteId, 101L));

		assertEquals(1, handler.getVotes("server").size());
		assertEquals(100L, handler.getVotes("server").get(0).getTime());
		verify(storage).addVote("server", 0, handler.getVotes("server").get(0));
	}

	@Test
	public void onlineCacheRejectsDuplicateVoteId() {
		UUID voteId = UUID.randomUUID();

		handler.addOnlineVote("player-uuid", vote(voteId, 100L));
		handler.addOnlineVote("player-uuid", vote(voteId, 101L));

		assertEquals(1, handler.getOnlineVotes("player-uuid").size());
		verify(storage).addVoteOnline("player-uuid", 0, handler.getOnlineVotes("player-uuid").get(0));
	}

	@Test
	public void concurrentServerDeliveriesReserveVoteIdAtomically() throws Exception {
		UUID voteId = UUID.randomUUID();

		runConcurrently(() -> handler.addServerVote("server", vote(voteId, 100L)),
				() -> handler.addServerVote("server", vote(voteId, 101L)));

		assertEquals(1, handler.getVotes("server").size());
		verify(storage, times(1)).addVote(org.mockito.ArgumentMatchers.eq("server"),
				org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
	}

	@Test
	public void concurrentOnlineDeliveriesReserveVoteIdAtomically() throws Exception {
		UUID voteId = UUID.randomUUID();

		runConcurrently(() -> handler.addOnlineVote("player-uuid", vote(voteId, 100L)),
				() -> handler.addOnlineVote("player-uuid", vote(voteId, 101L)));

		assertEquals(1, handler.getOnlineVotes("player-uuid").size());
		verify(storage, times(1)).addVoteOnline(org.mockito.ArgumentMatchers.eq("player-uuid"),
				org.mockito.ArgumentMatchers.anyInt(), org.mockito.ArgumentMatchers.any(OfflineBungeeVote.class));
	}

	@Test
	public void distinctVoteIdsAreNotCollapsed() {
		handler.addServerVote("server", vote(UUID.randomUUID(), 100L));
		handler.addServerVote("server", vote(UUID.randomUUID(), 101L));

		assertEquals(2, handler.getVotes("server").size());
	}

	@Test
	public void missingVoteIdsRemainLegacyCompatible() {
		handler.addServerVote("server", vote(null, 100L));
		handler.addServerVote("server", vote(null, 101L));

		assertEquals(2, handler.getVotes("server").size());
	}

	@Test
	public void currentVoteIdKeyLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId);

		handler.load();

		assertEquals(voteId, handler.getVotes("server").get(0).getVoteId());
	}

	@Test
	public void legacyVoteIdKeyLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteID", voteId);

		handler.load();

		assertEquals(voteId, handler.getVotes("server").get(0).getVoteId());
	}

	@Test
	public void legacyJsonCacheEntryStillNeedsItsBroadcast() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId);

		handler.load();

		assertFalse(handler.getVotes("server").get(0).isBroadcastForwarded());
	}

	@Test
	public void forwardedBroadcastStateLoadsFromJsonCache() {
		UUID voteId = UUID.randomUUID();
		handler = handlerForStoredVote("VoteId", voteId, true);

		handler.load();

		assertTrue(handler.getVotes("server").get(0).isBroadcastForwarded());
	}

	@Test
	public void timedVoteIsPersistedImmediatelyWithItsId() {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L);
		handler.addTimeVoteToCache(queued);

		verify(storage).addTimedVote(eq(0), same(queued));
		verify(storage).save();
		assertEquals(voteId, handler.getTimeChangeQueue().element().getVoteId());
	}

	@Test
	public void timedVoteDeliveryStateIsUpdatedByVoteId() {
		UUID voteId = UUID.randomUUID();
		DataNode stored = mock(DataNode.class);
		when(stored.isObject()).thenReturn(true);
		stubString(stored, "VoteId", voteId.toString());
		when(storage.getTimedVoteCache()).thenReturn(List.of("2"));
		when(storage.getTimedVoteCache("2")).thenReturn(stored);
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L, true,
				Set.of("Server1"), Set.of("Server1"));

		handler.updateTimeVote(queued);

		verify(storage).addTimedVote(2, queued);
		verify(storage).save();
	}

	@Test
	public void timedVoteIsRemovedFromMemoryOnlyAfterJsonSaveSucceeds() {
		VoteTimeQueue queued = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		assertTrue(handler.addTimeVoteToCache(queued));

		assertTrue(handler.removeTimeVote(queued));

		assertTrue(handler.getTimeChangeQueue().isEmpty());
	}

	@Test
	public void timedVoteRemainsQueuedWhenJsonDeleteCannotBeSaved() {
		VoteTimeQueue queued = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		assertTrue(handler.addTimeVoteToCache(queued));
		doThrow(new RuntimeException("save failed")).when(storage).save();

		assertFalse(handler.removeTimeVote(queued));

		assertTrue(handler.getTimeChangeQueue().contains(queued));
	}

	@Test
	public void legacyTimedVoteConstructorHasNoId() {
		VoteTimeQueue vote = new VoteTimeQueue("Player", "Service", 100L);

		assertNull(vote.getVoteId());
		assertFalse(vote.isProxyBroadcastHandled());
		assertTrue(vote.getBroadcastTargets().isEmpty());
		assertTrue(vote.getBroadcastForwardedServers().isEmpty());
	}

	@Test
	public void timedVoteBroadcastStateRoundTripsThroughStorageEncoding() {
		Set<String> targets = new LinkedHashSet<>(List.of("Server1", "Server,Two", "Server Three"));
		Set<String> forwarded = Set.of("Server1");
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L, true, targets,
				forwarded);

		assertEquals(targets, VoteTimeQueue.decodeBroadcastForwardedServers(vote.encodeBroadcastTargets()));
		assertEquals(forwarded,
				VoteTimeQueue.decodeBroadcastForwardedServers(vote.encodeBroadcastForwardedServers()));
	}

	@Test
	public void cachedBroadcastStateSuppressesNonTargetsAndDeliveredTargets() {
		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", false, true, Set.of("Lobby", "Survival"), Set.of("Lobby"), false);

		assertFalse(vote.needsBroadcastOn("Lobby"));
		assertTrue(vote.needsBroadcastOn("Survival"));
		assertFalse(vote.needsBroadcastOn("Creative"));
		assertFalse(vote.isProxyBroadcastComplete());
	}

	@Test
	public void updatedServerBroadcastStateIsPersistedInPlace() {
		DataNode voteNode = mock(DataNode.class);
		when(storage.getServerVotes("server")).thenReturn(List.of("4"));
		when(storage.getServerVotes("server", "4")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);

		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateServerVote("server", vote);

		verify(storage).addVote("server", 4, vote);
		verify(storage).save();
	}

	@Test
	public void updatedOnlineBroadcastStateIsPersistedInPlace() {
		DataNode voteNode = mock(DataNode.class);
		when(storage.getOnlineVotes("player-uuid")).thenReturn(List.of("3"));
		when(storage.getOnlineVotes("player-uuid", "3")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);

		OfflineBungeeVote vote = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service", 100L,
				true, "totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateOnlineVote("player-uuid", vote);

		verify(storage).addVoteOnline("player-uuid", 3, vote);
		verify(storage).save();
	}

	@Test
	public void serverBroadcastStateUpdateUsesVoteIdWhenTimestampsCollide() {
		UUID voteId = UUID.randomUUID();
		DataNode matching = storedVoteNode(voteId, 100L);
		DataNode other = storedVoteNode(UUID.randomUUID(), 100L);
		when(storage.getServerVotes("server")).thenReturn(List.of("4", "5"));
		when(storage.getServerVotes("server", "4")).thenReturn(other);
		when(storage.getServerVotes("server", "5")).thenReturn(matching);

		OfflineBungeeVote vote = new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true,
				"totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateServerVote("server", vote);

		verify(storage, never()).addVote("server", 4, vote);
		verify(storage).addVote("server", 5, vote);
	}

	@Test
	public void onlineBroadcastStateUpdateUsesVoteIdWhenTimestampsCollide() {
		UUID voteId = UUID.randomUUID();
		DataNode matching = storedVoteNode(voteId, 100L);
		DataNode other = storedVoteNode(UUID.randomUUID(), 100L);
		when(storage.getOnlineVotes("player-uuid")).thenReturn(List.of("3", "7"));
		when(storage.getOnlineVotes("player-uuid", "3")).thenReturn(other);
		when(storage.getOnlineVotes("player-uuid", "7")).thenReturn(matching);

		OfflineBungeeVote vote = new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", 100L, true,
				"totals", true, true, Set.of("Server1"), Set.of("Server1"), false);
		handler.updateOnlineVote("player-uuid", vote);

		verify(storage, never()).addVoteOnline("player-uuid", 3, vote);
		verify(storage).addVoteOnline("player-uuid", 7, vote);
	}

	@Test
	public void globalRewardClearRetainsOnlyIncompleteForwardBroadcasts() {
		OfflineBungeeVote pending = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service",
				100L, true, "totals", false, true, Set.of("Lobby", "Survival"), Set.of("Lobby"), false);
		OfflineBungeeVote complete = new OfflineBungeeVote(UUID.randomUUID(), "Player", "player-uuid", "Service",
				101L, true, "totals", true, true, Set.of("Lobby"), Set.of("Lobby"), false);
		handler.addOnlineVote("player-uuid", pending);
		handler.addOnlineVote("player-uuid", complete);

		handler.clearOnlineVoteRewards("player-uuid");

		assertEquals(List.of(pending), handler.getOnlineVotes("player-uuid"));
		assertTrue(pending.isRewardDelivered());
		assertTrue(pending.needsBroadcastOn("Survival"));
	}

	@Test
	public void completedOnlineVoteRemovalKeepsCollidingVoteId() {
		UUID removedId = UUID.randomUUID();
		OfflineBungeeVote removed = vote(removedId, 100L);
		OfflineBungeeVote retained = vote(UUID.randomUUID(), 100L);
		handler.addOnlineVote("player-uuid", removed);
		handler.addOnlineVote("player-uuid", retained);

		handler.removeOnlineVote("player-uuid", removed);

		assertEquals(List.of(retained), handler.getOnlineVotes("player-uuid"));
	}

	@Test
	public void timedVoteBroadcastStateLoadsFromJsonCache() {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode timedNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(List.of("0"));
		when(stored.getTimedVoteCache("0")).thenReturn(timedNode);
		when(stored.getServers()).thenReturn(Collections.emptyList());
		when(stored.getPlayers()).thenReturn(Collections.emptyList());
		when(timedNode.isObject()).thenReturn(true);

		UUID voteId = UUID.randomUUID();
		stubString(timedNode, "Name", "Player");
		stubString(timedNode, "Service", "Service");
		stubLong(timedNode, "Time", 100L);
		stubString(timedNode, "VoteId", voteId.toString());
		stubBoolean(timedNode, "ProxyBroadcastHandled", true);
		stubString(timedNode, "BroadcastTargets",
				VoteTimeQueue.encodeBroadcastServers(Set.of("Server1", "Server2")));
		stubString(timedNode, "BroadcastForwardedServers",
				new VoteTimeQueue(voteId, "Player", "Service", 100L, true, Set.of("Server1"))
						.encodeBroadcastForwardedServers());
		String totals = new com.bencodez.votingplugin.proxy.VoteTotalsSnapshot(10, 2, 2, 1, 4, 0, 20, 2)
				.toStorageString();
		stubString(timedNode, "Totals", totals);
		stubBoolean(timedNode, "Processed", true);

		handler = newHandler(stored);
		handler.load();

		VoteTimeQueue loaded = handler.getTimeChangeQueue().element();
		assertEquals(voteId, loaded.getVoteId());
		assertTrue(loaded.isProxyBroadcastHandled());
		assertEquals(Set.of("Server1", "Server2"), loaded.getBroadcastTargets());
		assertEquals(Set.of("Server1"), loaded.getBroadcastForwardedServers());
		assertEquals(totals, loaded.getTotals());
		assertTrue(loaded.isProcessed());
	}

	@Test
	public void onlineVoteBroadcastTargetsAndRewardStateLoadFromJsonCache() {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode voteNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(Collections.emptyList());
		when(stored.getServers()).thenReturn(Collections.emptyList());
		when(stored.getPlayers()).thenReturn(List.of("player-uuid"));
		when(stored.getOnlineVotes("player-uuid")).thenReturn(List.of("0"));
		when(stored.getOnlineVotes("player-uuid", "0")).thenReturn(voteNode);
		when(voteNode.isObject()).thenReturn(true);

		UUID voteId = UUID.randomUUID();
		stubString(voteNode, "Name", "Player");
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);
		stubBoolean(voteNode, "Real", true);
		stubString(voteNode, "Text", "totals");
		stubString(voteNode, "VoteId", voteId.toString());
		stubBoolean(voteNode, "ProxyBroadcastHandled", true);
		stubString(voteNode, "BroadcastTargets", VoteTimeQueue.encodeBroadcastServers(Set.of("Lobby", "Survival")));
		stubString(voteNode, "BroadcastForwardedServers", VoteTimeQueue.encodeBroadcastServers(Set.of("Lobby")));
		stubBoolean(voteNode, "RewardDelivered", true);

		handler = newHandler(stored);
		handler.load();

		OfflineBungeeVote loaded = handler.getOnlineVotes("player-uuid").get(0);
		assertTrue(loaded.isProxyBroadcastHandled());
		assertEquals(Set.of("Lobby", "Survival"), loaded.getBroadcastTargets());
		assertEquals(Set.of("Lobby"), loaded.getBroadcastForwardedServers());
		assertTrue(loaded.isRewardDelivered());
		assertTrue(loaded.needsBroadcastOn("Survival"));
	}

	private VoteCacheHandler handlerForStoredVote(String idKey, UUID voteId) {
		return handlerForStoredVote(idKey, voteId, null);
	}

	private VoteCacheHandler handlerForStoredVote(String idKey, UUID voteId, Boolean broadcastForwarded) {
		IVoteCache stored = mock(IVoteCache.class);
		DataNode voteNode = mock(DataNode.class);
		when(stored.getTimedVoteCache()).thenReturn(Collections.emptyList());
		when(stored.getServers()).thenReturn(List.of("server"));
		when(stored.getServerVotes("server")).thenReturn(List.of("0"));
		when(stored.getServerVotes("server", "0")).thenReturn(voteNode);
		when(stored.getPlayers()).thenReturn(Collections.emptyList());
		when(voteNode.isObject()).thenReturn(true);

		stubString(voteNode, "Name", "Player");
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", 100L);
		stubBoolean(voteNode, "Real", true);
		stubString(voteNode, "Text", "totals");
		stubString(voteNode, idKey, voteId.toString());
		if (broadcastForwarded != null) {
			stubBoolean(voteNode, "BroadcastForwarded", broadcastForwarded.booleanValue());
		}
		if ("VoteID".equals(idKey)) {
			when(voteNode.has("VoteId")).thenReturn(false);
		}

		return newHandler(stored);
	}

	private static void stubString(DataNode parent, String key, String value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asString()).thenReturn(value);
	}

	private static void stubLong(DataNode parent, String key, long value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asLong()).thenReturn(value);
	}

	private static void stubBoolean(DataNode parent, String key, boolean value) {
		DataNode child = mock(DataNode.class);
		when(parent.has(key)).thenReturn(true);
		when(parent.get(key)).thenReturn(child);
		when(child.asBoolean()).thenReturn(value);
	}

	private static DataNode storedVoteNode(UUID voteId, long time) {
		DataNode voteNode = mock(DataNode.class);
		when(voteNode.isObject()).thenReturn(true);
		stubString(voteNode, "UUID", "player-uuid");
		stubString(voteNode, "Service", "Service");
		stubLong(voteNode, "Time", time);
		stubString(voteNode, "VoteId", voteId.toString());
		return voteNode;
	}

	private static void runConcurrently(Runnable first, Runnable second) throws Exception {
		ExecutorService executor = Executors.newFixedThreadPool(2);
		CountDownLatch ready = new CountDownLatch(2);
		CountDownLatch start = new CountDownLatch(1);
		try {
			Future<?> firstFuture = executor.submit(() -> {
				ready.countDown();
				await(start);
				first.run();
			});
			Future<?> secondFuture = executor.submit(() -> {
				ready.countDown();
				await(start);
				second.run();
			});
			ready.await();
			start.countDown();
			firstFuture.get();
			secondFuture.get();
		} finally {
			executor.shutdownNow();
		}
	}

	private static void await(CountDownLatch latch) {
		try {
			latch.await();
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			throw new IllegalStateException(e);
		}
	}

	private static OfflineBungeeVote vote(UUID voteId, long time) {
		return new OfflineBungeeVote(voteId, "Player", "player-uuid", "Service", time, true, "totals");
	}

	private static VoteCacheHandler newHandler(IVoteCache storage) {
		return new VoteCacheHandler(null, false, false, null, false, storage) {
			@Override
			public void logInfo1(String msg) {
			}

			@Override
			public void logSevere1(String msg) {
			}

			@Override
			public void debug1(Exception e) {
			}

			@Override
			public void debug1(Throwable e) {
			}

			@Override
			public void debug1(String msg) {
			}
		};
	}
}
