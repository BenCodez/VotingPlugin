package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.List;
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
	public void timedVoteIdIsPassedToJsonStorage() {
		UUID voteId = UUID.randomUUID();
		VoteTimeQueue queued = new VoteTimeQueue(voteId, "Player", "Service", 100L);
		handler.addTimeVoteToCache(queued);

		handler.saveVoteCache();

		verify(storage).addTimedVote(eq(0), same(queued));
		verify(storage).save();
		assertEquals(voteId, handler.getTimeChangeQueue().element().getVoteId());
	}

	@Test
	public void legacyTimedVoteConstructorHasNoId() {
		assertNull(new VoteTimeQueue("Player", "Service", 100L).getVoteId());
	}

	private VoteCacheHandler handlerForStoredVote(String idKey, UUID voteId) {
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
