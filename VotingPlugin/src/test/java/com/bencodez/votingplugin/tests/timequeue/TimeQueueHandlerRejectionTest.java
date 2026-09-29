package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.reset;

import java.util.Set;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.Server;
import org.bukkit.plugin.PluginManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;

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
	void newlyQueuedVoteIsPersistedImmediately() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		org.mockito.Mockito.clearInvocations(serverData);

		handler.addVote("Alex", "example.org");

		org.mockito.ArgumentCaptor<java.util.Collection<com.bencodez.votingplugin.timequeue.VoteTimeQueue>> snapshot =
				org.mockito.ArgumentCaptor.forClass(java.util.Collection.class);
		verify(serverData).replaceTimedVoteCache(snapshot.capture());
		assertEquals(2, snapshot.getValue().size());
	}

	@Test
	void processingFailureRetainsDurableQueueHead() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		Server server = mock(Server.class);
		PluginManager manager = mock(PluginManager.class);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(manager);
		when(plugin.getVoteSiteManager().getVoteSiteName(true, "example.org")).thenReturn("example.org");
		doThrow(new IllegalStateException("listener failed")).when(manager).callEvent(any(PlayerVoteEvent.class));
		org.mockito.Mockito.clearInvocations(serverData);

		handler.processQueue();

		assertEquals(1, handler.getTimeChangeQueue().size());
		verify(serverData, never()).replaceTimedVoteCache(any());
	}

	@Test
	void completedVoteDoesNotRepeatEffectsWhenRetirementPersistenceRetries() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		Server server = mock(Server.class);
		PluginManager manager = mock(PluginManager.class);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(manager);
		when(plugin.getVoteSiteManager().getVoteSiteName(true, "example.org")).thenReturn("example.org");
		doThrow(new IllegalStateException("save failed")).doNothing()
				.when(serverData).replaceTimedVoteCache(any());

		handler.processQueue();
		assertEquals(1, handler.getTimeChangeQueue().size());
		verify(manager, times(1)).callEvent(any(PlayerVoteEvent.class));

		handler.processQueue();
		assertEquals(0, handler.getTimeChangeQueue().size());
		verify(manager, times(1)).callEvent(any(PlayerVoteEvent.class));
	}

	@Test
	void cancelledVoteDoesNotStrandLaterQueuedVotes() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		handler.addVote("Alex", "second.example.org");
		Server server = mock(Server.class);
		PluginManager manager = mock(PluginManager.class);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(manager);
		when(plugin.getVoteSiteManager().getVoteSiteName(true, anyString())).thenAnswer(invocation -> invocation.getArgument(1));
		doAnswer(invocation -> {
			((PlayerVoteEvent) invocation.getArgument(0)).setCancelled(true);
			return null;
		}).when(manager).callEvent(any(PlayerVoteEvent.class));
		org.mockito.Mockito.clearInvocations(serverData);

		handler.processQueue();

		assertEquals(0, handler.getTimeChangeQueue().size());
		verify(manager, times(2)).callEvent(any(PlayerVoteEvent.class));
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
}
