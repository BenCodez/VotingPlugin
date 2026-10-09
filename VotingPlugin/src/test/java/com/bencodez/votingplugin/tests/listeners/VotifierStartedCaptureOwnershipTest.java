package com.bencodez.votingplugin.tests.listeners;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.logging.Logger;
import org.bukkit.plugin.PluginManager;
import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.listeners.VotiferEvent;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue;
import com.vexsoftware.votifier.model.Vote;

/** Real ingress callback, real executor cancellation, real overflow publication/restart. */
class VotifierStartedCaptureOwnershipTest {
    @TempDir Path directory;
    private VotingPluginMain plugin;
    private PluginManager events;
    private com.bencodez.votingplugin.data.ServerData serverData;
    private ScheduledExecutorService timer;
    private VotiferEvent listener;
    private VotifierVoteOverflowQueue overflow;
    private final AtomicBoolean ready = new AtomicBoolean(true);
    private final List<PlayerVoteEvent> processed = new CopyOnWriteArrayList<>();
    private static final String SITE = "example.org";
    private static ScheduledExecutorService timer() {
        return Executors.newSingleThreadScheduledExecutor(task -> {
            var thread = new Thread(task, "test-native-vote-callback"); thread.setDaemon(true); return thread;
        });
    }
    @BeforeEach void setup() throws Exception {
        plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getDataFolder()).thenReturn(directory.toFile()); when(plugin.getLogger()).thenReturn(Logger.getAnonymousLogger());
        when(plugin.getOptions().getBedrockPlayerPrefix()).thenReturn(".");
        when(plugin.getBungeeSettings().isUseBungeecoord()).thenReturn(false);
        when(plugin.getVoteSiteManager().getVoteSiteName(anyBoolean(), anyString(), anyString())).thenAnswer(call -> call.getArgument(1));
        when(plugin.getVoteSiteManager().hasVoteSite(anyString())).thenReturn(true);
        events = mock(PluginManager.class); when(plugin.getServer().getPluginManager()).thenReturn(events);
        doAnswer(call -> { processed.add(call.getArgument(0)); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
        serverData = plugin.getServerData();
        timer = timer(); when(plugin.getVoteTimer()).thenReturn(timer);
        listener = new VotiferEvent(plugin, ready::get);
        overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
        when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow);
    }
    @AfterEach void close() { timer.shutdownNow(); overflow.close(); }
    private com.vexsoftware.votifier.model.VotifierEvent receipt() {
        var vote = mock(Vote.class); when(vote.getServiceName()).thenReturn(SITE);
        when(vote.getUsername()).thenReturn("Steve"); when(vote.getAddress()).thenReturn("127.0.0.1");
        var event = mock(com.vexsoftware.votifier.model.VotifierEvent.class); when(event.getVote()).thenReturn(vote); return event;
    }
    private static ScheduledThreadPoolExecutor worker(VotifierVoteOverflowQueue queue) throws Exception {
        var field = VotifierVoteOverflowQueue.class.getDeclaredField("worker"); field.setAccessible(true);
        return (ScheduledThreadPoolExecutor) field.get(queue);
    }
    private static void workerTurn(VotifierVoteOverflowQueue queue) throws Exception { worker(queue).submit(() -> {}).get(5, TimeUnit.SECONDS); }
    private void processingTurn() throws Exception { timer.submit(() -> {}).get(5, TimeUnit.SECONDS); }
    private List<Map<?, ?>> disk() { return YamlConfiguration.loadConfiguration(directory.resolve("VotifierVoteQueue.yml").toFile()).getMapList("Votes"); }
    @Test void startedBlockedBeforeAcceptedPipelineSurvivesStopCancellationAndRestart() throws Exception {
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            try { if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout"); }
            catch (InterruptedException interrupted) {
                Thread.currentThread().interrupt(); throw new IllegalStateException("native storage interrupted before vote consumer", interrupted);
            }
            return null;
        }).when(serverData).addServiceSite(SITE);
        long before = System.currentTimeMillis(); listener.onVotiferEvent(receipt()); long after = System.currentTimeMillis();
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS)); assertEquals(1, listener.getPendingCaptureCount());
            assertTrue(processed.isEmpty()); ready.set(false); listener.stop(); listener.stop(); workerTurn(overflow);
            assertEquals(0, listener.getPendingCaptureCount()); assertEquals(1, overflow.size());
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS)); overflow.close();
            var rows = disk(); assertEquals(1, rows.size()); var row = rows.getFirst();
            UUID id = UUID.fromString((String) row.get("LocalOccurrenceId")); long time = ((Number) row.get("Time")).longValue();
            assertTrue(time >= before && time <= after); assertTrue(processed.isEmpty());
            doAnswer(call -> null).when(serverData).addServiceSite(SITE);
            timer = timer(); when(plugin.getVoteTimer()).thenReturn(timer);
            try (var restarted = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, () -> true)) {
                workerTurn(restarted); assertEquals(1, restarted.size());
                var delivered = new CountDownLatch(1);
                doAnswer(call -> { processed.add(call.getArgument(0)); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
                restarted.start(); assertTrue(delivered.await(5, TimeUnit.SECONDS)); processingTurn(); workerTurn(restarted);
                assertEquals(0, restarted.size()); assertEquals(1, processed.size());
                assertEquals(id, processed.getFirst().getLocalOccurrenceId()); assertEquals(time, processed.getFirst().getCanonicalOccurrenceTime());
            }
            try (var again = new VotifierVoteOverflowQueue(plugin, (site, user) -> fail("completed receipt replayed"))) { assertEquals(0, again.size()); }
        } finally { release.countDown(); }
    }
    @Test void completedBeforeTransferAdmissionReconcilesExactOccurrence() throws Exception {
        completedCurrentCallbackReconcilesExactTransferredOccurrence("before-admission");
    }
    @Test void completedAfterTransferAdmissionReconcilesExactOccurrence() throws Exception {
        completedCurrentCallbackReconcilesExactTransferredOccurrence("after-admission");
    }
    private void completedCurrentCallbackReconcilesExactTransferredOccurrence(String completionOrder) throws Exception {
        UUID unrelated = UUID.randomUUID(); assertTrue(overflow.enqueue("Steve", SITE, 123L, unrelated));
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            return null;
        }).when(serverData).addServiceSite(SITE);
        listener.onVotiferEvent(receipt()); assertTrue(entered.await(5, TimeUnit.SECONDS)); assertEquals(1, listener.getPendingCaptureCount());
        var transferEntered = new CountDownLatch(1); var releaseTransfer = new CountDownLatch(1);
        if (completionOrder.equals("before-admission")) {
            worker(overflow).execute(() -> {
                transferEntered.countDown();
                try { if (!releaseTransfer.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test transfer timeout"); }
                catch (InterruptedException failure) { throw new CompletionException(failure); }
            });
            assertTrue(transferEntered.await(5, TimeUnit.SECONDS));
        }
        try {
            ready.set(false); listener.stop();
            if (completionOrder.equals("after-admission")) { workerTurn(overflow); assertEquals(2, overflow.size()); }
            release.countDown(); processingTurn(); assertEquals(1, processed.size());
            UUID completed = processed.getFirst().getLocalOccurrenceId(); assertNotEquals(unrelated, completed);
            releaseTransfer.countDown(); workerTurn(overflow); assertEquals(0, listener.getPendingCaptureCount());
            assertEquals(1, overflow.size()); overflow.close(); assertEquals(1, disk().size());
            assertEquals(unrelated.toString(), disk().getFirst().get("LocalOccurrenceId"));
            var delivered = new CountDownLatch(1);
            doAnswer(call -> { processed.add(call.getArgument(0)); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
            try (var restarted = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, () -> true)) {
                workerTurn(restarted); restarted.start(); assertTrue(delivered.await(5, TimeUnit.SECONDS)); processingTurn(); workerTurn(restarted);
                assertEquals(0, restarted.size()); assertEquals(2, processed.size());
                assertEquals(1, processed.stream().filter(event -> completed.equals(event.getLocalOccurrenceId())).count());
                assertEquals(unrelated, processed.getLast().getLocalOccurrenceId());
            }
        } finally { release.countDown(); releaseTransfer.countDown(); }
    }
    @Test void uncertainPartialProcessingKeepsItsTransferredRecoveryRecord() throws Exception {
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1); var effects = new AtomicInteger();
        doAnswer(call -> {
            effects.incrementAndGet(); entered.countDown();
            if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            throw new IllegalStateException("consumer failed after a partial effect");
        }).when(events).callEvent(any(PlayerVoteEvent.class));
        listener.onVotiferEvent(receipt());
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS)); ready.set(false); listener.stop(); workerTurn(overflow);
            release.countDown(); processingTurn(); assertEquals(1, effects.get()); assertEquals(1, overflow.size());
            overflow.close(); assertEquals(1, disk().size());
        } finally { release.countDown(); }
    }
    @Test void standaloneStopCannotDrainStartedTransferWhileItsOriginalCallbackRuns() throws Exception {
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            return null;
        }).when(serverData).addServiceSite(SITE);
        overflow.start(); listener.onVotiferEvent(receipt());
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS)); listener.stop();
            // Readiness remains true: the per-entry release fence must protect this
            // path even when a caller stops capture without the main shutdown fence.
            workerTurn(overflow); workerTurn(overflow); assertEquals(1, overflow.size());
            assertTrue(processed.isEmpty()); release.countDown(); processingTurn(); workerTurn(overflow);
            assertEquals(1, processed.size(), "an eagerly submitted overflow retry would duplicate normal effects");
            assertEquals(0, overflow.size()); overflow.close(); assertTrue(disk().isEmpty());
        } finally { release.countDown(); }
    }
    @Test void ordinaryRejectedAdmissionTransfersAnUnstartedReceiptThroughTheSameQueue() throws Exception {
        var admissions = new AtomicInteger(); var wrapper = mock(ScheduledExecutorService.class);
        doAnswer(call -> {
            if (admissions.getAndIncrement() == 0) throw new RejectedExecutionException("initial admission rejected");
            return timer.submit(call.getArgument(0, Runnable.class));
        }).when(wrapper).submit(any(Runnable.class)); when(plugin.getVoteTimer()).thenReturn(wrapper);
        var delivered = new CountDownLatch(1);
        doAnswer(call -> { processed.add(call.getArgument(0)); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
        overflow.start(); listener.onVotiferEvent(receipt());
        assertTrue(delivered.await(5, TimeUnit.SECONDS)); processingTurn(); workerTurn(overflow);
        assertEquals(1, processed.size()); assertEquals(0, listener.getPendingCaptureCount()); assertEquals(0, overflow.size());
    }
    @Test void failedFullQueueTransferRetainsStartedOwnershipAndExactCapacity() throws Exception {
        for (int index = 0; index < 256; index++) assertTrue(overflow.enqueue("Steve", SITE, 123L));
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            try { if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout"); }
            catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); throw new IllegalStateException("interrupted", interrupted); }
            return null;
        }).when(serverData).addServiceSite(SITE);
        listener.onVotiferEvent(receipt());
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS)); ready.set(false); listener.stop(); workerTurn(overflow);
            assertEquals(1, listener.getPendingCaptureCount()); assertEquals(256, overflow.size());
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS));
            assertEquals(1, listener.getPendingCaptureCount()); assertTrue(processed.isEmpty());
            overflow.close(); assertEquals(256, disk().size());
        } finally { release.countDown(); }
    }
    @Test void completionAfterSealedSnapshotReportsUncertaintyWithoutRetiredOverwrite() throws Exception {
        var logger = mock(Logger.class); when(plugin.getLogger()).thenReturn(logger);
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            return null;
        }).when(serverData).addServiceSite(SITE);
        listener.onVotiferEvent(receipt());
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS)); ready.set(false); listener.stop(); workerTurn(overflow); overflow.close();
            byte[] sealed = Files.readAllBytes(directory.resolve("VotifierVoteQueue.yml")); assertEquals(1, disk().size());
            release.countDown(); processingTurn(); assertEquals(1, processed.size());
            verify(logger).warning(contains("outlived queue shutdown"));
            assertArrayEquals(sealed, Files.readAllBytes(directory.resolve("VotifierVoteQueue.yml")));
        } finally { release.countDown(); }
    }
}
