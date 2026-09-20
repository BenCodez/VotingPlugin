package com.bencodez.votingplugin.proxy;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collections;
import java.util.Queue;
import java.util.UUID;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import com.bencodez.votingplugin.proxy.cache.VoteCacheHandler;
import com.bencodez.votingplugin.tests.VotingPluginProxyTestImpl;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class VotingPluginProxyQueueProgressTest {
	@Test
	void nonblockingQueuedRetryAllowsLaterProcessedRowToDrain() {
		VoteCacheHandler cache = Mockito.mock(VoteCacheHandler.class);
		Queue<VoteTimeQueue> queue = new ConcurrentLinkedQueue<>();
		VoteTimeQueue first = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		VoteTimeQueue second = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 101L);
		second.setProcessed(true);
		queue.add(first);
		queue.add(second);
		Mockito.when(cache.getTimeChangeQueue()).thenReturn(queue);
		Mockito.when(cache.removeTimeVote(second)).thenAnswer(invocation -> queue.remove(second));
		VotingPluginProxy proxy = Mockito.spy(new VotingPluginProxyTestImpl());
		Mockito.doReturn(cache).when(proxy).getVoteCacheHandler();
		Mockito.doReturn(VotingPluginProxy.QueuedVoteResult.RETRY_NONBLOCKING)
				.when(proxy).replayQueuedVote(Mockito.eq(first), Mockito.isNull(), Mockito.anyBoolean());

		proxy.processQueue();

		assertTrue(queue.contains(first));
		assertFalse(queue.contains(second));
	}

	@Test
	void ordinaryQueuedRetryStillStopsBeforeLaterRow() {
		VoteCacheHandler cache = Mockito.mock(VoteCacheHandler.class);
		Queue<VoteTimeQueue> queue = new ConcurrentLinkedQueue<>();
		VoteTimeQueue first = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 100L);
		VoteTimeQueue second = new VoteTimeQueue(UUID.randomUUID(), "Player", "Service", 101L);
		second.setProcessed(true);
		queue.add(first);
		queue.add(second);
		Mockito.when(cache.getTimeChangeQueue()).thenReturn(queue);
		VotingPluginProxy proxy = Mockito.spy(new VotingPluginProxyTestImpl());
		Mockito.doReturn(cache).when(proxy).getVoteCacheHandler();
		Mockito.doReturn(VotingPluginProxy.QueuedVoteResult.RETRY)
				.when(proxy).replayQueuedVote(Mockito.eq(first), Mockito.isNull(), Mockito.anyBoolean());

		proxy.processQueue();

		assertTrue(queue.contains(first));
		assertTrue(queue.contains(second));
	}
}
