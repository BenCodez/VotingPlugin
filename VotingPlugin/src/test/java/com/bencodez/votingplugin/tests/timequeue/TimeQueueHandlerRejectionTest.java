package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.reset;

import java.util.Set;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
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
