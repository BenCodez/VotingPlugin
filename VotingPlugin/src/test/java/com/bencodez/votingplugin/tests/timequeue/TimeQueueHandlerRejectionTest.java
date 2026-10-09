package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
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

import org.bukkit.Server;
import org.bukkit.configuration.ConfigurationSection;
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
	void queuedVoteRestoresOriginalTimestampOnReplay() {
		TimeQueueHandler handler = new TimeQueueHandler(plugin);
		Server server = mock(Server.class);
		PluginManager manager = mock(PluginManager.class);
		when(plugin.getServer()).thenReturn(server);
		when(server.getPluginManager()).thenReturn(manager);
		when(plugin.getVoteSiteManager().getVoteSiteName(true, "example.org")).thenReturn("example.org");
		org.mockito.ArgumentCaptor<PlayerVoteEvent> event = org.mockito.ArgumentCaptor.forClass(PlayerVoteEvent.class);

		handler.processQueue();

		verify(manager).callEvent(event.capture());
		assertNotNull(event.getValue());
		assertEquals(123L, event.getValue().getTime());
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
    void actualCacheWriterAndReplayKeepProcessingAndOccurrenceClocksSeparate() throws Exception {
        var stored = new org.bukkit.configuration.file.YamlConfiguration();
        ServerData writer = org.mockito.Mockito.spy(new ServerData(plugin));
        org.mockito.Mockito.doReturn(stored).when(writer).getData();
        org.mockito.Mockito.doNothing().when(writer).saveData();
        var captured = new com.bencodez.votingplugin.timequeue.VoteTimeQueue("Steve", "example.org", 789L);
        java.util.UUID occurrenceId = java.util.UUID.randomUUID();
        captured.setLocalOccurrenceId(occurrenceId);
        captured.setCanonicalOccurrenceTime(456L);
        writer.addTimeVoted(0, captured);
        writer.addTimeVoted(1, new com.bencodez.votingplugin.timequeue.VoteTimeQueue("Alex", "example.org", 123L));
        assertEquals(occurrenceId.toString(), stored.getString("TimedVoteCache.0.LocalOccurrenceId"));
        assertEquals(789L, stored.getLong("TimedVoteCache.0.Time"));
        assertEquals(456L, stored.getLong("TimedVoteCache.0.CanonicalOccurrenceTime"));
        org.junit.jupiter.api.Assertions.assertFalse(stored.contains("TimedVoteCache.1.CanonicalOccurrenceTime"));
        var restored = new org.bukkit.configuration.file.YamlConfiguration();
        restored.loadFromString(stored.saveToString());
        // Malformed optional metadata fails Date provenance, retaining ordinary replay.
        restored.set("TimedVoteCache.2.Name", "Invalid"); restored.set("TimedVoteCache.2.Service", "example.org");
        restored.set("TimedVoteCache.2.Time", 321L); restored.set("TimedVoteCache.2.CanonicalOccurrenceTime", "bad");
        restored.set("TimedVoteCache.3.Name", "Corrupt"); restored.set("TimedVoteCache.3.Service", "example.org");
        restored.set("TimedVoteCache.3.Time", 654L); restored.set("TimedVoteCache.3.LocalOccurrenceId", "bad");
        restored.set("TimedVoteCache.4.Name", "ShortId"); restored.set("TimedVoteCache.4.Service", "example.org");
        restored.set("TimedVoteCache.4.Time", 654L); restored.set("TimedVoteCache.4.LocalOccurrenceId", "1-1-1-1-1");
        org.mockito.Mockito.doReturn(restored).when(writer).getData();
        when(plugin.getServerData()).thenReturn(writer);
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        var replayed = new java.util.ArrayList<PlayerVoteEvent>();
        var eventManager = plugin.getServer().getPluginManager();
        org.mockito.Mockito.doAnswer(call -> { replayed.add(call.getArgument(0)); return null; })
                .when(eventManager).callEvent(any());
        var migratedId = handler.getTimeChangeQueue().stream().filter(vote -> "Alex".equals(vote.getName()))
                .findFirst().orElseThrow().getLocalOccurrenceId();
        assertEquals(migratedId.toString(), restored.getString("TimedVoteCache.1.LocalOccurrenceId"));
        var secondResume = new TimeQueueHandler(plugin);
        assertEquals(migratedId, secondResume.getTimeChangeQueue().stream().filter(vote -> "Alex".equals(vote.getName()))
                .findFirst().orElseThrow().getLocalOccurrenceId());
        handler.processQueue();
        assertEquals(3, replayed.size());
        var byName = replayed.stream().collect(java.util.stream.Collectors.toMap(PlayerVoteEvent::getPlayer, event -> event));
        assertEquals(occurrenceId, byName.get("Steve").getLocalOccurrenceId());
        org.junit.jupiter.api.Assertions.assertNull(byName.get("Steve").getProxyVoteId());
        org.junit.jupiter.api.Assertions.assertFalse(byName.get("Steve").isBungee());
        assertEquals(migratedId, byName.get("Alex").getLocalOccurrenceId());
        assertEquals(789L, byName.get("Steve").getTime()); assertEquals(Long.valueOf(456L), byName.get("Steve").getCanonicalOccurrenceTime());
        assertEquals(123L, byName.get("Alex").getTime()); org.junit.jupiter.api.Assertions.assertNull(byName.get("Alex").getCanonicalOccurrenceTime());
        assertEquals(321L, byName.get("Invalid").getTime()); assertEquals(Long.valueOf(-1L), byName.get("Invalid").getCanonicalOccurrenceTime());
    }

    @Test
    void actualAdmissionSaveAndRestartReplayRetainLocalId() throws Exception {
        var stored = new org.bukkit.configuration.file.YamlConfiguration();
        ServerData writer = org.mockito.Mockito.spy(new ServerData(plugin));
        org.mockito.Mockito.doReturn(stored).when(writer).getData();
        org.mockito.Mockito.doNothing().when(writer).saveData();
        when(plugin.getServerData()).thenReturn(writer);
        TimeQueueHandler original = new TimeQueueHandler(plugin);
        java.util.UUID occurrenceId = java.util.UUID.randomUUID();
        original.addVote("Steve", "example.org", 456L, occurrenceId);
        long queueTime = original.getTimeChangeQueue().element().getTime();
        original.save();
        var restored = new org.bukkit.configuration.file.YamlConfiguration();
        restored.loadFromString(stored.saveToString());
        org.mockito.Mockito.doReturn(restored).when(writer).getData();
        TimeQueueHandler resumed = new TimeQueueHandler(plugin);
        org.mockito.ArgumentCaptor<PlayerVoteEvent> replay = org.mockito.ArgumentCaptor.forClass(PlayerVoteEvent.class);
        resumed.processQueue();
        verify(plugin.getServer().getPluginManager()).callEvent(replay.capture());
        assertEquals(occurrenceId, replay.getValue().getLocalOccurrenceId());
        assertEquals(queueTime, replay.getValue().getTime());
        assertEquals(Long.valueOf(456L), replay.getValue().getCanonicalOccurrenceTime());
        org.junit.jupiter.api.Assertions.assertNull(replay.getValue().getProxyVoteId());
    }

    @Test
    void legacyTimedIdsAreStableEvenIfSaveHasNotPublishedAndIdenticalRowsRemainDistinct() throws Exception {
        var legacy = new org.bukkit.configuration.file.YamlConfiguration();
        for (String key : java.util.List.of("0", "1")) {
            legacy.set("TimedVoteCache." + key + ".Name", "Steve");
            legacy.set("TimedVoteCache." + key + ".Service", "example.org");
            legacy.set("TimedVoteCache." + key + ".Time", 123L);
        }
        String beforeMigration = legacy.saveToString();
        ServerData writer = org.mockito.Mockito.spy(new ServerData(plugin));
        org.mockito.Mockito.doReturn(legacy).when(writer).getData();
        org.mockito.Mockito.doNothing().when(writer).saveData();
        when(plugin.getServerData()).thenReturn(writer);
        var first = new TimeQueueHandler(plugin).getTimeChangeQueue().stream()
                .map(com.bencodez.votingplugin.timequeue.VoteTimeQueue::getLocalOccurrenceId).toList();
        assertEquals(2, first.stream().distinct().count());
        org.mockito.Mockito.verify(writer, org.mockito.Mockito.times(2)).saveData();
        // Simulate a restart that sees only the old on-disk representation.
        var unpublished = new org.bukkit.configuration.file.YamlConfiguration();
        unpublished.loadFromString(beforeMigration);
        org.mockito.Mockito.doReturn(unpublished).when(writer).getData();
        var second = new TimeQueueHandler(plugin).getTimeChangeQueue().stream()
                .map(com.bencodez.votingplugin.timequeue.VoteTimeQueue::getLocalOccurrenceId).toList();
        assertEquals(first, second);
    }

    @Test
    void timeChangeAdmissionPreservesCapturedReceiptWithoutReplacingQueueTime() {
        when(serverData.getTimedVoteCacheKeys()).thenReturn(Set.of());
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        java.util.UUID occurrenceId = java.util.UUID.randomUUID();
        long before = System.currentTimeMillis(); handler.addVote("Steve", "example.org", 456L, occurrenceId); long after = System.currentTimeMillis();
        var queued = handler.getTimeChangeQueue().element();
        org.junit.jupiter.api.Assertions.assertTrue(queued.getTime() >= before && queued.getTime() <= after);
        assertEquals(456L, queued.getCanonicalOccurrenceTime());
        assertEquals(occurrenceId, queued.getLocalOccurrenceId());
        org.junit.jupiter.api.Assertions.assertNull(queued.getVoteId());
    }

}
