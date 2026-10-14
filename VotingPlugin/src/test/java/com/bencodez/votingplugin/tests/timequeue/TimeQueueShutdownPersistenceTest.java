package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.*;
import java.util.concurrent.*;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.bukkit.configuration.file.YamlConfiguration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;

class TimeQueueShutdownPersistenceTest {
    private record Fixture(VotingPluginMain plugin, ServerData data, TimeQueueHandler handler) {}
    private Fixture fixture() {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        ServerData data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
        return new Fixture(plugin, data, new TimeQueueHandler(plugin));
    }

    @Test void failureRetainsOnlyUnsavedSuffixAndRetriesAtSameIndex() {
        Fixture f = fixture();
        Map<Integer, String> saved = new TreeMap<>();
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(call -> {
            int index = call.getArgument(0);
            VoteTimeQueue vote = call.getArgument(1);
            if (index == 1 && fail.getAndSet(false)) throw new IllegalStateException("disk unavailable");
            saved.put(index, vote.getName()); return null;
        }).when(f.data).addTimeVoted(anyInt(), any());
        f.handler.addVote("First", "site"); f.handler.addVote("Second", "site");
        assertThrows(IllegalStateException.class, f.handler::saveForShutdown);
        assertEquals(Map.of(0, "First"), saved);
        assertEquals("Second", f.handler.getTimeChangeQueue().peek().getName());
        f.handler.saveForShutdown(); f.handler.saveForShutdown();
        f.handler.addVote("Late", "site");
        assertEquals(Map.of(0, "First", 1, "Second", 2, "Late"), saved);
        assertTrue(f.handler.getTimeChangeQueue().isEmpty());
        verify(f.data, times(1)).addTimeVoted(eq(0), any());
        verify(f.data, times(2)).addTimeVoted(eq(1), any());
    }

    @Test void lateAdmissionDuringSaveIsAppendedWithoutBeingClearedOrOverwriting() throws Exception {
        Fixture f = fixture();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        Map<Integer, String> saved = new ConcurrentHashMap<>();
        doAnswer(call -> {
            int index = call.getArgument(0);
            if (index == 0) { entered.countDown(); assertTrue(release.await(5, TimeUnit.SECONDS)); }
            saved.put(index, ((VoteTimeQueue) call.getArgument(1)).getName()); return null;
        }).when(f.data).addTimeVoted(anyInt(), any());
        f.handler.addVote("First", "site");
        ExecutorService workers = Executors.newFixedThreadPool(2);
        try {
            Future<?> save = workers.submit(f.handler::saveForShutdown);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            var adding = new CountDownLatch(1);
            Future<?> late = workers.submit(() -> { adding.countDown(); f.handler.addVote("Late", "site"); });
            assertTrue(adding.await(5, TimeUnit.SECONDS)); release.countDown();
            save.get(5, TimeUnit.SECONDS); late.get(5, TimeUnit.SECONDS);
            assertEquals(Map.of(0, "First", 1, "Late"), saved);
            assertTrue(f.handler.getTimeChangeQueue().isEmpty());
        } finally { release.countDown(); workers.shutdownNow(); assertTrue(workers.awaitTermination(5, TimeUnit.SECONDS)); }
    }

    @Test void admittedProcessingCallbackCannotClearShutdownSnapshot() {
        Fixture f = fixture();
        var scheduled = org.mockito.ArgumentCaptor.forClass(Runnable.class);
        verify(f.plugin.getVoteTimer()).schedule(scheduled.capture(), eq(120L), eq(TimeUnit.SECONDS));
        f.handler.addVote("First", "site"); f.handler.saveForShutdown();
        scheduled.getValue().run(); f.handler.processQueue();
        verify(f.data, never()).clearTimedVoteCache();
        verify(f.plugin.getServer().getPluginManager(), never()).callEvent(any());
    }

    @Test void ordinarySaveDoesNotSealFutureProcessing() {
        Fixture f = fixture();
        f.handler.addVote("Saved", "site"); f.handler.save();
        f.handler.addVote("New", "site"); f.handler.processQueue();
        verify(f.plugin.getServer().getPluginManager(), times(2)).callEvent(any());
    }

    @Test void shutdownRetainsInFlightEventForReviewWithoutWaitingOrReplaying() throws Exception {
        Fixture f = fixture();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        var events = f.plugin.getServer().getPluginManager();
        doAnswer(call -> { entered.countDown(); assertTrue(release.await(5, TimeUnit.SECONDS)); return null; })
                .when(events).callEvent(any());
        f.handler.addVote("InFlight", "site");
        ExecutorService worker = Executors.newSingleThreadExecutor();
        try {
            Future<?> delivery = worker.submit(f.handler::processQueue);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            f.handler.saveForShutdown(); f.handler.saveForShutdown();
            f.handler.processQueue();
            var vote = org.mockito.ArgumentCaptor.forClass(VoteTimeQueue.class);
            verify(f.data, times(1)).addUncertainTimedVote(any(UUID.class), vote.capture());
            assertEquals("InFlight", vote.getValue().getName());
            verify(f.data, never()).addTimeVoted(anyInt(), any());
            assertTrue(f.handler.getTimeChangeQueue().isEmpty());
            release.countDown(); delivery.get(5, TimeUnit.SECONDS);
            verify(f.plugin.getServer().getPluginManager(), times(1)).callEvent(any());
        } finally { release.countDown(); worker.shutdownNow(); assertTrue(worker.awaitTermination(5, TimeUnit.SECONDS)); }
    }

    @Test void shutdownBeforeEventDispatchPersistsVoteForNormalRecovery() throws Exception {
        Fixture f = fixture();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        var sites = f.plugin.getVoteSiteManager();
        doAnswer(call -> { entered.countDown(); assertTrue(release.await(5, TimeUnit.SECONDS)); return null; })
                .when(sites).getVoteSite(any(), eq(true));
        f.handler.addVote("NotDispatched", "site");
        ExecutorService worker = Executors.newSingleThreadExecutor();
        try {
            Future<?> delivery = worker.submit(f.handler::processQueue);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            f.handler.saveForShutdown();
            release.countDown(); delivery.get(5, TimeUnit.SECONDS);
            verify(f.data).addTimeVoted(eq(0), any());
            verify(f.data, never()).addUncertainTimedVote(any(), any());
            verify(f.plugin.getServer().getPluginManager(), never()).callEvent(any());
        } finally { release.countDown(); worker.shutdownNow(); assertTrue(worker.awaitTermination(5, TimeUnit.SECONDS)); }
    }

    @Test void failedEventAndFailedQuarantineRemainRetryableWithoutRepeatingEvent() {
        Fixture f = fixture();
        RuntimeException original = new IllegalStateException("event failure");
        var events = f.plugin.getServer().getPluginManager();
        doThrow(original).when(events).callEvent(any());
        doThrow(new IllegalStateException("disk failure")).doNothing()
                .when(f.data).addUncertainTimedVote(any(), any());
        f.handler.addVote("Ambiguous", "site");
        assertSame(original, assertThrows(IllegalStateException.class, f.handler::processQueue));
        assertEquals(1, original.getSuppressed().length);
        assertEquals(1, f.handler.getTimeChangeQueue().size());
        f.handler.processQueue();
        f.handler.saveForShutdown(); f.handler.saveForShutdown();
        var ids = org.mockito.ArgumentCaptor.forClass(UUID.class);
        verify(f.data, times(2)).addUncertainTimedVote(ids.capture(), any());
        assertEquals(ids.getAllValues().get(0), ids.getAllValues().get(1));
        verify(f.plugin.getServer().getPluginManager(), times(1)).callEvent(any());
        verify(f.data, never()).addTimeVoted(anyInt(), any());
    }

    @Test void directProcessingPublishesPendingSuffixBeforeHandingOffRecoveredVote() {
        Fixture f = fixture();
        f.handler.addVote("Recovered", "site");
        f.handler.addVote("Later", "site");
        VoteTimeQueue first = f.handler.getTimeChangeQueue().peek();
        f.handler.processQueue();
        var calls = inOrder(f.data, f.plugin.getServer().getPluginManager());
        calls.verify(f.data).prepareTimedVoteDispatch(eq(first.getVoteId()), eq(first),
                argThat(pending -> pending.size() == 1 && pending.iterator().next().getName().equals("Later")));
        calls.verify(f.plugin.getServer().getPluginManager()).callEvent(any());
        verify(f.data, never()).clearTimedVoteCache();
        f.handler.addVote("New", "site");
        f.handler.saveForShutdown();
        verify(f.data).addTimeVoted(eq(0), any());
    }

    @TempDir java.nio.file.Path directory;
    @Test void actualTimedCacheWriteReportsIoFailureAndSucceedsAfterRepair() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class);
        var file = mock(com.bencodez.advancedcore.data.ServerData.class);
        when(plugin.getServerDataFile()).thenReturn(file);
        YamlConfiguration yaml = new YamlConfiguration();
        when(file.getData()).thenReturn(yaml);
        java.nio.file.Path target = directory.resolve("ServerData.yml");
        java.nio.file.Files.createDirectory(target);
        when(file.getdFile()).thenReturn(target.toFile());
        ServerData data = new ServerData(plugin);
        VoteTimeQueue vote = new VoteTimeQueue("First", "site", 123L);
        assertThrows(java.io.UncheckedIOException.class, () -> data.addTimeVoted(0, vote));
        java.nio.file.Files.delete(target);
        data.addTimeVoted(0, vote);
        YamlConfiguration stored = YamlConfiguration.loadConfiguration(target.toFile());
        assertEquals("First", stored.getString("VotingPlugin.TimedVoteCache.0.Name"));
        assertEquals(123L, stored.getLong("VotingPlugin.TimedVoteCache.0.Time"));
        UUID uncertain = UUID.randomUUID();
        data.addUncertainTimedVote(uncertain, vote);
        stored = YamlConfiguration.loadConfiguration(target.toFile());
        assertEquals("First", stored.getString("VotingPlugin.UncertainTimedVotes." + uncertain + ".Name"));
        assertEquals(Set.of("0"), data.getTimedVoteCacheKeys());
    }
}
