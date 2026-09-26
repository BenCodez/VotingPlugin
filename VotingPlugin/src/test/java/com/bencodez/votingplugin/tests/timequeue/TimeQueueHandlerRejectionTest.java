package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class TimeQueueHandlerRejectionTest {
	private VotingPluginMain plugin;
	private ServerData serverData;
	private ScheduledExecutorService voteTimer;
	private Logger logger;
	private ConfigurationSection cachedVote;

	@BeforeEach
	void setUp() {
		plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		serverData = mock(ServerData.class);
		voteTimer = mock(ScheduledExecutorService.class);
		logger = mock(Logger.class);
		cachedVote = mock(ConfigurationSection.class);

		when(plugin.getServerData()).thenReturn(serverData);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		when(plugin.getLogger()).thenReturn(logger);
		when(plugin.isEnabled()).thenReturn(true);
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of("0"));
		when(serverData.getTimedVoteCacheSection("0")).thenReturn(cachedVote);
		when(cachedVote.getString("Name")).thenReturn("Steve");
		when(cachedVote.getString("Service")).thenReturn("example.org");
		when(cachedVote.getLong("Time")).thenReturn(123L);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
	}

	@Test
	void loadRetainsPersistentEntriesWhenSchedulingIsRejected() {
		TimeQueueHandler handler = assertDoesNotThrow(() -> new TimeQueueHandler(plugin));

		assertEquals(1, handler.getTimeChangeQueue().size());
		verify(serverData, never()).clearTimedVoteCache();
	}

	@Test
	void dateChangeRejectionDoesNotEscapeBukkitEventHandler() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		handler.addVote("Alex", "example.org");

		assertDoesNotThrow(() -> handler.postTimeChange((DateChangedEvent) null));
		assertEquals(2, handler.getTimeChangeQueue().size());
		verify(logger, org.mockito.Mockito.atLeastOnce()).warning(anyString());
	}

	@Test
	void queueAdmissionPreservesTheReceptionVoteId() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		UUID voteId = UUID.randomUUID();

		handler.addVote(voteId, "Alex", "example.org");

		assertEquals(voteId, handler.getTimeChangeQueue().peek().getVoteId());
	}

	@Test
	void durableAdmissionPersistsBeforeReportingSuccess() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		UUID voteId = UUID.randomUUID();

		assertTrue(handler.addVoteDurably(voteId, "Alex", "example.org"));

		@SuppressWarnings("unchecked")
		org.mockito.ArgumentCaptor<List<VoteTimeQueue>> persisted = org.mockito.ArgumentCaptor.forClass(List.class);
		verify(serverData).replaceTimedVoteCache(persisted.capture());
		assertEquals(voteId, persisted.getValue().getFirst().getVoteId());
	}

	@Test
	void processingRetiresEachTimedVoteFromTheDurableSnapshot() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		VoteTimeQueue first = new VoteTimeQueue(UUID.randomUUID(), "Alex", "first.example", 123L);
		VoteTimeQueue second = new VoteTimeQueue(UUID.randomUUID(), "Steve", "second.example", 124L);
		handler.getTimeChangeQueue().add(first);
		handler.getTimeChangeQueue().add(second);

		handler.processQueue();

		org.mockito.InOrder retirement = org.mockito.Mockito.inOrder(serverData);
		retirement.verify(serverData).replaceTimedVoteCache(List.of(second));
		retirement.verify(serverData).clearVotePartyAccounting(first.getVoteId());
		retirement.verify(serverData).replaceTimedVoteCache(List.of());
		retirement.verify(serverData).clearVotePartyAccounting(second.getVoteId());
		verify(serverData, never()).clearTimedVoteCache();
	}

	@Test
	void failedDurableAdmissionLeavesTheVoteWithItsSourceOwner() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		doThrow(new IllegalStateException("disk unavailable")).when(serverData).replaceTimedVoteCache(any());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);

		org.junit.jupiter.api.Assertions.assertFalse(
				handler.addVoteDurably(UUID.randomUUID(), "Alex", "example.org"));

		assertTrue(handler.getTimeChangeQueue().isEmpty());
	}

	@Test
	void rejectedProcessingSchedulesOneBoundedRetry() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		org.mockito.ArgumentCaptor<Runnable> retry = org.mockito.ArgumentCaptor.forClass(Runnable.class);
		verify(plugin.getBukkitScheduler()).runTaskLaterAsynchronously(
				org.mockito.ArgumentMatchers.eq(plugin), retry.capture(), org.mockito.ArgumentMatchers.eq(20L));

		reset(voteTimer);
		retry.getValue().run();

		verify(voteTimer).schedule(any(Runnable.class), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.eq(TimeUnit.SECONDS));
		assertEquals(1, handler.getTimeChangeQueue().size());
	}

	@Test
	void accountingAdmissionFailureRetainsTheSameQueuedVote() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		VoteTimeQueue vote = new VoteTimeQueue(java.util.UUID.randomUUID(), "Alex", "example.org", 123L);
		VoteTimeQueue following = new VoteTimeQueue(java.util.UUID.randomUUID(), "Steve", "second.example", 124L);
		handler.getTimeChangeQueue().add(vote);
		handler.getTimeChangeQueue().add(following);
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setAccountingAdmissionFailed(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		handler.processQueue();

		assertEquals(2, handler.getTimeChangeQueue().size());
		assertEquals(vote, handler.getTimeChangeQueue().peek());
		@SuppressWarnings("unchecked")
		org.mockito.ArgumentCaptor<List<VoteTimeQueue>> persisted = org.mockito.ArgumentCaptor.forClass(List.class);
		verify(serverData).replaceTimedVoteCache(persisted.capture());
		assertEquals(List.of(vote, following), persisted.getValue());
	}

	@Test
	void postAdmissionProcessingFailureRetainsTheSameQueuedVote() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Alex", "example.org", 123L);
		handler.getTimeChangeQueue().add(vote);
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setProcessingFailed(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		handler.processQueue();

		assertEquals(vote, handler.getTimeChangeQueue().peek());
		verify(serverData).replaceTimedVoteCache(List.of(vote));
	}

	@Test
	void shutdownSnapshotIncludesTheVoteCurrentlyBeingProcessed() throws Exception {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		VoteTimeQueue first = new VoteTimeQueue(UUID.randomUUID(), "Alex", "first.example", 123L);
		VoteTimeQueue second = new VoteTimeQueue(UUID.randomUUID(), "Steve", "second.example", 124L);
		handler.getTimeChangeQueue().add(first);
		handler.getTimeChangeQueue().add(second);
		CountDownLatch processing = new CountDownLatch(1);
		CountDownLatch release = new CountDownLatch(1);
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			processing.countDown();
			release.await();
			throw new CancellationException("shutdown");
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));
		AtomicReference<Throwable> failure = new AtomicReference<>();
		Thread worker = new Thread(() -> {
			try {
				handler.processQueue();
			} catch (Throwable thrown) {
				failure.set(thrown);
			}
		});
		worker.start();
		assertTrue(processing.await(1, TimeUnit.SECONDS));

		handler.save();

		verify(serverData).replaceTimedVoteCache(List.of(first, second));
		release.countDown();
		worker.join(1000L);
		assertFalse(worker.isAlive());
		assertTrue(failure.get() instanceof CancellationException);
	}

	@Test
	void ambiguousPostEffectFailureIsQuarantinedInsteadOfReplayed() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Alex", "example.org", 123L);
		handler.getTimeChangeQueue().add(vote);
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setProcessingFailed(true);
			event.setReplayUnsafe(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		handler.processQueue();

		assertTrue(handler.getTimeChangeQueue().isEmpty());
		verify(serverData).quarantineTimedVote(vote);
		verify(plugin.getBukkitScheduler(), never()).runTaskLaterAsynchronously(
				org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class), anyLong());
	}

	@Test
	void failedAmbiguousQuarantineReturnsVoteToDurableRetryQueue() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		clearInvocations(plugin.getBukkitScheduler());
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Alex", "example.org", 123L);
		handler.getTimeChangeQueue().add(vote);
		doThrow(new IllegalStateException("disk unavailable")).when(serverData).quarantineTimedVote(vote);
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setProcessingFailed(true);
			event.setReplayUnsafe(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		assertDoesNotThrow(handler::processQueue);

		assertEquals(vote, handler.getTimeChangeQueue().peek());
		verify(serverData).replaceTimedVoteCache(List.of(vote));
		verify(plugin.getBukkitScheduler()).runTaskLaterAsynchronously(
				org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class), anyLong());
	}

	@Test
	void timedVoteSnapshotPersistsStableIdsWithOneSave() {
		VotingPluginMain snapshotPlugin = mock(VotingPluginMain.class);
		com.bencodez.advancedcore.data.ServerData coreData =
				mock(com.bencodez.advancedcore.data.ServerData.class);
		YamlConfiguration yaml = new YamlConfiguration();
		when(snapshotPlugin.getServerDataFile()).thenReturn(coreData);
		when(coreData.getData()).thenReturn(yaml);
		ServerData data = new ServerData(snapshotPlugin);
		UUID firstId = UUID.randomUUID();
		UUID secondId = UUID.randomUUID();

		data.replaceTimedVoteCache(List.of(
				new VoteTimeQueue(firstId, "Alex", "first.example", 123L),
				new VoteTimeQueue(secondId, "Steve", "second.example", 124L)));

		assertEquals(firstId.toString(), yaml.getString("VotingPlugin.TimedVoteCache.0.VoteId"));
		assertEquals(secondId.toString(), yaml.getString("VotingPlugin.TimedVoteCache.1.VoteId"));
		assertEquals("Alex", yaml.getString("VotingPlugin.TimedVoteCache.0.Name"));
		verify(coreData).saveData();
	}

	@Test
	void processingRetryWaitsForFailedSnapshotPersistence() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		reset(voteTimer);
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		reset(voteTimer);
		VoteTimeQueue vote = new VoteTimeQueue(UUID.randomUUID(), "Alex", "example.org", 123L);
		handler.getTimeChangeQueue().add(vote);
		doThrow(new IllegalStateException("disk unavailable")).doNothing()
				.when(serverData).replaceTimedVoteCache(any());
		org.bukkit.plugin.PluginManager pluginManager = plugin.getServer().getPluginManager();
		doAnswer(invocation -> {
			PlayerVoteEvent event = invocation.getArgument(0);
			event.setAccountingAdmissionFailed(true);
			return null;
		}).when(pluginManager).callEvent(any(PlayerVoteEvent.class));

		handler.processQueue();

		org.mockito.ArgumentCaptor<Runnable> retry = org.mockito.ArgumentCaptor.forClass(Runnable.class);
		verify(plugin.getBukkitScheduler()).runTaskLaterAsynchronously(
				org.mockito.ArgumentMatchers.eq(plugin), retry.capture(), org.mockito.ArgumentMatchers.eq(20L));
		verify(voteTimer, never()).schedule(any(Runnable.class), org.mockito.ArgumentMatchers.eq(0L),
				org.mockito.ArgumentMatchers.eq(TimeUnit.SECONDS));

		retry.getValue().run();

		org.mockito.InOrder persistedBeforeProcessing = org.mockito.Mockito.inOrder(serverData, voteTimer);
		persistedBeforeProcessing.verify(serverData, org.mockito.Mockito.times(2)).replaceTimedVoteCache(any());
		persistedBeforeProcessing.verify(voteTimer).schedule(any(Runnable.class),
				org.mockito.ArgumentMatchers.eq(0L), org.mockito.ArgumentMatchers.eq(TimeUnit.SECONDS));
	}

	@Test
	void disabledPluginDoesNotRegisterABukkitRetry() {
		when(plugin.isEnabled()).thenReturn(false);

		new TimeQueueHandler(plugin);

		verify(plugin.getBukkitScheduler(), never()).runTaskLaterAsynchronously(
				org.mockito.ArgumentMatchers.eq(plugin), any(Runnable.class), anyLong());
	}

	@Test
	void cancelledDurableTransitionFailsItsLeaseWithoutScheduling() {
		when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		reset(voteTimer);
		TimeChangeTransition transition = mock(TimeChangeTransition.class);
		TimeChangeTransition.Lease lease = mock(TimeChangeTransition.Lease.class);
		when(transition.retain()).thenReturn(lease);
		when(transition.isCancellationRequested()).thenReturn(true);

		handler.postTimeChange(new DateChangedEvent(com.bencodez.advancedcore.api.time.TimeType.DAY, transition));

		verify(voteTimer, never()).schedule(any(Runnable.class), anyLong(), any(TimeUnit.class));
		verify(lease).fail(any(java.util.concurrent.CancellationException.class));
		verify(lease, never()).complete();
	}
}
