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
import com.bencodez.votingplugin.util.BoundedScheduledExecutor;
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
        // Finish mock creation before overflow admission and normal processing
        // concurrently use this plugin; deep-stub creation itself is not safe then.
        when(plugin.getConfigFile()).thenReturn(mock(com.bencodez.votingplugin.config.Config.class));
        when(plugin.getTimeChecker()).thenReturn(mock(com.bencodez.advancedcore.api.time.TimeChecker.class));
        when(plugin.getVoteSiteManager().getVoteSiteName(anyBoolean(), anyString(), anyString())).thenAnswer(call -> call.getArgument(1));
        when(plugin.getVoteSiteManager().hasVoteSite(anyString())).thenReturn(true);
        when(plugin.getVoteSiteManager().getVoteSite(anyString(), anyBoolean()))
                .thenReturn(mock(com.bencodez.votingplugin.votesites.VoteSite.class));
        events = mock(PluginManager.class); when(plugin.getServer().getPluginManager()).thenReturn(events);
        doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
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
    private record CapturedReceipt(UUID id, long time) { }
    private CapturedReceipt retainedReceipt() throws Exception {
        var field = VotiferEvent.class.getDeclaredField("captured"); field.setAccessible(true);
        var entries = (Map<?, ?>) field.get(listener); assertEquals(1, entries.size());
        var vote = entries.values().iterator().next();
        var time = vote.getClass().getDeclaredField("occurredAt"); time.setAccessible(true);
        return new CapturedReceipt((UUID) entries.keySet().iterator().next(), time.getLong(vote));
    }
    private List<UUID> recoverFullQueue() throws Exception {
        var ids = new ArrayList<UUID>();
        for (int index = 0; index < 256; index++) {
            UUID id = UUID.randomUUID(); ids.add(id);
            assertTrue(overflow.enqueue("Steve", SITE, 123L, id));
        }
        overflow.close();
        overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
        when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow);
        assertEquals(256, overflow.size());
        return ids;
    }
    @Test void saturatedCapturesUseAllIndependentOverflowSlotsWithoutChangingReceiptIdentity() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        timer.shutdownNow(); timer = new BoundedScheduledExecutor(1, 256);
        when(plugin.getVoteTimer()).thenReturn(timer);
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            if (!release.await(10, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            return null;
        }).when(serverData).addServiceSite(SITE);
        var delivered = new CountDownLatch(512);
        doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null; })
                .when(events).callEvent(any(PlayerVoteEvent.class));
        try {
            listener.onVotiferEvent(receipt()); assertTrue(entered.await(5, TimeUnit.SECONDS));
            for (int index = 1; index < 256; index++) listener.onVotiferEvent(receipt());
            assertEquals(256, listener.getPendingCaptureCount());
            long before = System.currentTimeMillis();
            for (int index = 0; index < 256; index++) listener.onVotiferEvent(receipt());
            long after = System.currentTimeMillis(); workerTurn(overflow);
            assertEquals(256, overflow.size(), "Normal capture saturation must use the independent overflow capacity");
            assertEquals(256, listener.getPendingCaptureCount()); assertTrue(processed.isEmpty());
            listener.onVotiferEvent(receipt()); workerTurn(overflow);
            assertEquals(256, overflow.size()); assertEquals(256, listener.getPendingCaptureCount());
            overflow.close(); var rows = disk(); assertEquals(256, rows.size());
            var occurrences = new HashMap<UUID, Long>();
            for (var row : rows) {
                long time = ((Number) row.get("Time")).longValue(); assertTrue(time >= before && time <= after);
                assertNull(occurrences.put(UUID.fromString((String) row.get("LocalOccurrenceId")), time));
            }
            overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
            when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow); overflow.start();
            release.countDown(); assertTrue(delivered.await(10, TimeUnit.SECONDS)); processingTurn(); workerTurn(overflow);
            assertEquals(512, processed.size()); assertEquals(512, processed.stream().map(PlayerVoteEvent::getLocalOccurrenceId).distinct().count());
            for (var event : processed) if (occurrences.containsKey(event.getLocalOccurrenceId()))
                assertEquals(occurrences.get(event.getLocalOccurrenceId()).longValue(), event.getCanonicalOccurrenceTime());
            assertEquals(0, listener.getPendingCaptureCount()); assertEquals(0, overflow.size());
        } finally { release.countDown(); }
    }
    @Test void rejectedTransferRetriesAfterRecoveredFullQueueDrains() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        var recovered = recoverFullQueue();
        var admissions = new AtomicInteger(); var wrapper = mock(ScheduledExecutorService.class);
        doAnswer(call -> {
            if (admissions.getAndIncrement() == 0) throw new RejectedExecutionException("initial admission rejected");
            return timer.submit(call.getArgument(0, Runnable.class));
        }).when(wrapper).submit(any(Runnable.class)); when(plugin.getVoteTimer()).thenReturn(wrapper);
        var delivered = new CountDownLatch(257);
        doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null; })
                .when(events).callEvent(any(PlayerVoteEvent.class));
        listener.onVotiferEvent(receipt()); workerTurn(overflow);
        var original = retainedReceipt(); assertEquals(256, overflow.size()); assertTrue(processed.isEmpty());
        overflow.start(); assertTrue(delivered.await(5, TimeUnit.SECONDS), "Rejected capture must retry when recovered entries drain");
        processingTurn(); workerTurn(overflow);
        assertEquals(0, listener.getPendingCaptureCount()); assertEquals(0, overflow.size());
        assertEquals(257, processed.size()); assertEquals(257, processed.stream().map(PlayerVoteEvent::getLocalOccurrenceId).distinct().count());
        assertTrue(processed.stream().map(PlayerVoteEvent::getLocalOccurrenceId).toList().containsAll(recovered));
        var retry = processed.stream().filter(event -> original.id().equals(event.getLocalOccurrenceId())).toList();
        assertEquals(1, retry.size()); assertEquals(original.time(), retry.getFirst().getCanonicalOccurrenceTime());
    }
    @Test void gracefulDisableWhichSkipsConsumerRetainsTransferredReceipt() throws Exception {
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        doAnswer(call -> {
            entered.countDown();
            if (!release.await(5, TimeUnit.SECONDS)) throw new IllegalStateException("test processing timeout");
            return null;
        }).when(serverData).addServiceSite(SITE);
        // Bukkit's disabled-listener path returns normally without dispatching the consumer.
        doAnswer(call -> null).when(events).callEvent(any(PlayerVoteEvent.class));
        listener.onVotiferEvent(receipt());
        try {
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            var original = retainedReceipt();
            ready.set(false); listener.stop(); workerTurn(overflow);
            release.countDown(); processingTurn(); workerTurn(overflow);
            assertTrue(processed.isEmpty());
            assertEquals(1, overflow.size());
            overflow.close();
            var rows = disk(); assertEquals(1, rows.size());
            assertEquals(original.id().toString(), rows.getFirst().get("LocalOccurrenceId"));
            assertEquals(original.time(), ((Number) rows.getFirst().get("Time")).longValue());
        } finally { release.countDown(); }
    }
    @Test void confirmedDispatchRetainsFailureSwallowedByEventManager() {
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getUserManager().getProperName(anyString())).thenThrow(new IllegalStateException("synthetic consumer failure"));
        var consumer = new com.bencodez.votingplugin.listeners.PlayerVoteListener(plugin);
        doAnswer(call -> {
            try { consumer.onplayerVote(call.getArgument(0, PlayerVoteEvent.class)); }
            catch (RuntimeException ignored) { /* Bukkit reports and swallows listener failures. */ }
            return null;
        }).when(events).callEvent(any(PlayerVoteEvent.class));
        assertFalse(listener.processVoteConfirmed(SITE, "TestPlayer", 1234L, UUID.randomUUID()));
        assertTrue(processed.isEmpty());
    }
    @Test void intentionalCancellationBeforeConsumerRemainsACompletedNoOp() {
        doAnswer(call -> { call.getArgument(0, PlayerVoteEvent.class).setCancelled(true); return null; })
                .when(events).callEvent(any(PlayerVoteEvent.class));
        assertTrue(listener.processVoteConfirmed(SITE, "TestPlayer", 1234L, UUID.randomUUID()));
        assertTrue(processed.isEmpty());
    }
    @Test void acceptedConsumerDoesNotConfirmDisabledOrExceptionalProcessing() {
        var consumer = new com.bencodez.votingplugin.listeners.PlayerVoteListener(plugin);
        var disabled = new PlayerVoteEvent(mock(com.bencodez.votingplugin.votesites.VoteSite.class), "TestPlayer", SITE, true);
        when(plugin.isEnabled()).thenReturn(false);
        consumer.onplayerVote(disabled);
        assertFalse(disabled.isNativeProcessingStarted());
        assertFalse(disabled.isNativeProcessingCompleted());
        when(plugin.isEnabled()).thenReturn(true);
        when(plugin.getUserManager().getProperName(anyString())).thenThrow(new IllegalStateException("synthetic consumer failure"));
        var failed = new PlayerVoteEvent(mock(com.bencodez.votingplugin.votesites.VoteSite.class), "TestPlayer", SITE, true);
        assertThrows(IllegalStateException.class, () -> consumer.onplayerVote(failed));
        assertTrue(failed.isNativeProcessingStarted());
        assertFalse(failed.isNativeProcessingCompleted());
    }
    @Test void stopRetriesRejectedTransferAfterCapacityReturnsWithExactReceipt() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        var recovered = recoverFullQueue();
        var rejecting = mock(ScheduledExecutorService.class);
        when(rejecting.submit(any(Runnable.class))).thenThrow(new RejectedExecutionException("vote worker full"));
        when(plugin.getVoteTimer()).thenReturn(rejecting);
        listener.onVotiferEvent(receipt()); workerTurn(overflow); var original = retainedReceipt();
        overflow.acknowledgeOccurrence(recovered.getFirst());
        listener.stop(); workerTurn(overflow);
        assertEquals(0, listener.getPendingCaptureCount(), "Stop must retry a transfer previously rejected for capacity");
        assertEquals(256, overflow.size()); assertTrue(processed.isEmpty()); overflow.close();
        var rows = disk(); assertEquals(256, rows.size());
        var retained = rows.stream().filter(row -> original.id().toString().equals(row.get("LocalOccurrenceId"))).toList();
        assertEquals(1, retained.size()); assertEquals(original.time(), ((Number) retained.getFirst().get("Time")).longValue());
        assertTrue(rows.stream().noneMatch(row -> recovered.getFirst().toString().equals(row.get("LocalOccurrenceId"))));
    }
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
                doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
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
            doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
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
        doAnswer(call -> { var event = call.getArgument(0, PlayerVoteEvent.class); event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null; }).when(events).callEvent(any(PlayerVoteEvent.class));
        overflow.start(); listener.onVotiferEvent(receipt());
        assertTrue(delivered.await(5, TimeUnit.SECONDS)); processingTurn(); workerTurn(overflow);
        assertEquals(1, processed.size()); assertEquals(0, listener.getPendingCaptureCount()); assertEquals(0, overflow.size());
    }
    @Test void shutdownRetainsAnUnstartedCaptureBesideAFullRecoveredQueueAndRestartsItsExactOccurrence() throws Exception {
        var recovered = recoverFullQueue();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        timer.submit(() -> {
            entered.countDown();
            try { release.await(10, TimeUnit.SECONDS); }
            catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
        });
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        listener.onVotiferEvent(receipt());
        var original = retainedReceipt();
        try {
            ready.set(false); listener.stop();
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS));
            workerTurn(overflow); overflow.close();
            var rows = disk();
            assertEquals(257, rows.size(), "Closing a full overflow must retain its accepted normal capture too");
            assertTrue(rows.stream().map(row -> UUID.fromString((String) row.get("LocalOccurrenceId"))).toList()
                    .containsAll(recovered));
            var retained = rows.stream().filter(row -> original.id().toString().equals(row.get("LocalOccurrenceId"))).toList();
            assertEquals(1, retained.size()); assertEquals(original.time(), ((Number) retained.getFirst().get("Time")).longValue());
            timer = timer(); when(plugin.getVoteTimer()).thenReturn(timer);
            ready.set(true);
            var delivered = new CountDownLatch(257);
            doAnswer(call -> {
                var event = call.getArgument(0, PlayerVoteEvent.class);
                event.setNativeProcessingCompleted(true); processed.add(event); delivered.countDown(); return null;
            }).when(events).callEvent(any(PlayerVoteEvent.class));
            overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
            when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow);
            overflow.start(); assertTrue(delivered.await(10, TimeUnit.SECONDS));
            processingTurn(); workerTurn(overflow);
            assertEquals(257, processed.stream().map(PlayerVoteEvent::getLocalOccurrenceId).distinct().count());
            var replay = processed.stream().filter(event -> original.id().equals(event.getLocalOccurrenceId())).toList();
            assertEquals(1, replay.size()); assertEquals(original.time(), replay.getFirst().getCanonicalOccurrenceTime());
        } finally { release.countDown(); }
    }

    @Test void failedFinalWriteRetainsOwnershipUntilRepairAndPreservesTheCaptureForASuccessor() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        recoverFullQueue();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        timer.submit(() -> {
            entered.countDown();
            try { release.await(10, TimeUnit.SECONDS); }
            catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
        });
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        listener.onVotiferEvent(receipt()); var original = retainedReceipt();
        Path target = directory.resolve("VotifierVoteQueue.yml");
        Files.delete(target); Files.createDirectory(target);
        Path blocker = target.resolve("synthetic-blocker"); Files.writeString(blocker, "synthetic");
        VotifierVoteOverflowQueue successor = null;
        try {
            ready.set(false); listener.stop();
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS));
            overflow.close();
            successor = VotifierVoteOverflowQueue.initializeAsync(plugin, (site, user, time, id) -> {}, () -> false);
            workerTurn(successor);
            assertFalse(successor.isInitializationComplete(), "successor cannot initialize while final publication is uncertain");
            Files.delete(blocker); Files.delete(target);
            overflow.close(); assertTrue(worker(overflow).awaitTermination(5, TimeUnit.SECONDS));
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (!successor.isInitializationComplete() && System.nanoTime() < deadline) Thread.sleep(10);
            assertTrue(successor.isInitializationComplete()); assertEquals(257, successor.size());
            var rows = disk(); assertEquals(257, rows.size());
            var row = rows.stream().filter(value -> original.id().toString().equals(value.get("LocalOccurrenceId"))).toList();
            assertEquals(1, row.size()); assertEquals(original.time(), ((Number) row.getFirst().get("Time")).longValue());
        } finally {
            release.countDown();
            if (Files.isDirectory(target)) { Files.deleteIfExists(blocker); Files.delete(target); }
            overflow.close();
            if (successor != null) successor.close();
        }
    }

    @Test void closingReplacementKeepsProvisionalCaptureUntilPredecessorReleasesOwnership() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        VotifierVoteOverflowQueue predecessor = overflow;
        UUID recovered = UUID.randomUUID();
        assertTrue(predecessor.enqueue("Steve", SITE, 123L, recovered));
        workerTurn(predecessor);
        overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
        when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow);
        workerTurn(overflow);
        assertFalse(overflow.isInitializationComplete());
        listener.onVotiferEvent(receipt());
        var original = retainedReceipt();
        try {
            ready.set(false); listener.stop();
            long began = System.nanoTime(); overflow.close();
            assertTrue(System.nanoTime() - began < TimeUnit.SECONDS.toNanos(2));
            assertFalse(worker(overflow).isShutdown(), "closing worker must retain provisional retirement ownership");
            predecessor.close();
            assertTrue(worker(overflow).awaitTermination(5, TimeUnit.SECONDS));
            var rows = disk(); assertEquals(2, rows.size());
            assertEquals(Set.of(recovered, original.id()), rows.stream()
                    .map(row -> UUID.fromString((String) row.get("LocalOccurrenceId")))
                    .collect(java.util.stream.Collectors.toSet()));
            var retained = rows.stream().filter(row -> original.id().toString().equals(row.get("LocalOccurrenceId"))).findFirst().orElseThrow();
            assertEquals(original.time(), ((Number) retained.get("Time")).longValue());
        } finally {
            predecessor.close(); overflow.close();
        }
    }

    @Test void closingReplacementRetainsCapturesAcrossUnknownStorageUntilValidRepair() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        VotifierVoteOverflowQueue predecessor = overflow;
        UUID recovered = UUID.randomUUID();
        assertTrue(predecessor.enqueue("Steve", SITE, 123L, recovered)); workerTurn(predecessor);
        overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
        when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow);
        listener.onVotiferEvent(receipt()); var original = retainedReceipt();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        Path file = directory.resolve("VotifierVoteQueue.yml"); byte[] valid = null;
        try {
            ready.set(false); listener.stop(); overflow.close();
            worker(overflow).execute(() -> {
                entered.countDown();
                try { release.await(10, TimeUnit.SECONDS); }
                catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
            });
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            predecessor.close(); valid = Files.readAllBytes(file);
            String broken = "Votes: [unterminated"; Files.writeString(file, broken); release.countDown();
            var failed = VotifierVoteOverflowQueue.class.getDeclaredField("loadFailed"); failed.setAccessible(true);
            long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5);
            while (!failed.getBoolean(overflow) && System.nanoTime() < deadline) Thread.sleep(10);
            assertTrue(failed.getBoolean(overflow)); assertFalse(worker(overflow).isShutdown());
            assertEquals(broken, Files.readString(file));
            Files.delete(file); Thread.sleep(600);
            assertFalse(Files.exists(file), "observed unknown history cannot become an empty install");
            assertFalse(worker(overflow).isShutdown());
            Files.write(file, valid);
            assertTrue(worker(overflow).awaitTermination(5, TimeUnit.SECONDS));
            assertEquals(Set.of(recovered, original.id()), disk().stream()
                    .map(row -> UUID.fromString((String) row.get("LocalOccurrenceId")))
                    .collect(java.util.stream.Collectors.toSet()));
        } finally {
            release.countDown(); predecessor.close();
            if (!Files.exists(file) && valid != null) Files.write(file, valid);
            overflow.close();
        }
    }

    @Test void legacyConstructorRetirementRunsBeforeItsFinalSnapshot() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class)); overflow.close();
        overflow = new VotifierVoteOverflowQueue(plugin, (site, user) -> {});
        UUID id = UUID.randomUUID();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        worker(overflow).execute(() -> {
            entered.countDown();
            try { release.await(10, TimeUnit.SECONDS); }
            catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
        });
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        var accepted = new java.util.concurrent.atomic.AtomicBoolean();
        overflow.retireCaptures(List.of(new VotifierVoteOverflowQueue.RetiringCapture("Steve", SITE, 123L, id,
                () -> true, () -> false, accepted::set)));
        try {
            overflow.close(); assertFalse(worker(overflow).isShutdown()); release.countDown();
            assertTrue(worker(overflow).awaitTermination(5, TimeUnit.SECONDS));
            assertTrue(accepted.get()); assertEquals(id.toString(), disk().getFirst().get("LocalOccurrenceId"));
        } finally { release.countDown(); overflow.close(); }
    }

    @Test void waitingRetirementAdmissionIsGloballyBounded() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        VotifierVoteOverflowQueue predecessor = overflow;
        overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, (site, user, time, id) -> {}, () -> false);
        workerTurn(overflow);
        var batch = new ArrayList<VotifierVoteOverflowQueue.RetiringCapture>();
        for (int i = 0; i < 256; i++) batch.add(new VotifierVoteOverflowQueue.RetiringCapture("Steve", SITE, 123L,
                UUID.randomUUID(), () -> true, () -> false, accepted -> {}));
        var rejected = new java.util.concurrent.atomic.AtomicReference<Boolean>();
        try {
            overflow.retireCaptures(batch);
            overflow.retireCaptures(List.of(new VotifierVoteOverflowQueue.RetiringCapture("Steve", SITE, 456L,
                    UUID.randomUUID(), () -> true, () -> false, rejected::set)));
            assertEquals(Boolean.FALSE, rejected.get()); overflow.close(); predecessor.close();
            assertTrue(worker(overflow).awaitTermination(5, TimeUnit.SECONDS)); assertEquals(256, disk().size());
        } finally { predecessor.close(); overflow.close(); }
    }

    @Test void throwingRetirementCallbackDoesNotPreventTheRestOfTheBatchBeingRetained() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        UUID first = UUID.randomUUID(), second = UUID.randomUUID();
        assertEquals(VotifierVoteOverflowQueue.CaptureAdmission.ACCEPTED, overflow.reserveCapture(first));
        assertEquals(VotifierVoteOverflowQueue.CaptureAdmission.ACCEPTED, overflow.reserveCapture(second));
        var completed = new CountDownLatch(1);
        overflow.retireCaptures(List.of(
                new VotifierVoteOverflowQueue.RetiringCapture("Steve", SITE, 123L, first, () -> true, () -> false,
                        accepted -> { throw new IllegalStateException("synthetic callback failure"); }),
                new VotifierVoteOverflowQueue.RetiringCapture("Steve", SITE, 456L, second, () -> true, () -> false,
                        accepted -> { assertTrue(accepted); completed.countDown(); })));
        assertTrue(completed.await(5, TimeUnit.SECONDS));
        overflow.close();
        assertEquals(Set.of(first, second), disk().stream().map(row -> UUID.fromString((String) row.get("LocalOccurrenceId")))
                .collect(java.util.stream.Collectors.toSet()));
    }

    @Test void combinedFullSnapshotRemainsBoundedAcrossRepeatedRecoveryAndFreshIngress() throws Exception {
        when(plugin.getLogger()).thenReturn(mock(Logger.class));
        var recovered = recoverFullQueue();
        var entered = new CountDownLatch(1); var release = new CountDownLatch(1);
        timer.submit(() -> {
            entered.countDown();
            try { release.await(10, TimeUnit.SECONDS); }
            catch (InterruptedException interrupted) { Thread.currentThread().interrupt(); }
        });
        assertTrue(entered.await(5, TimeUnit.SECONDS));
        for (int index = 0; index < 256; index++) listener.onVotiferEvent(receipt());
        assertEquals(256, listener.getPendingCaptureCount());
        try {
            ready.set(false); listener.stop(); listener.stop();
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS));
            workerTurn(overflow); overflow.close();
            var rows = disk(); assertEquals(512, rows.size());
            var ids = rows.stream().map(row -> UUID.fromString((String) row.get("LocalOccurrenceId")))
                    .collect(java.util.stream.Collectors.toSet());
            assertEquals(512, ids.size()); assertTrue(ids.containsAll(recovered));
            for (int cycle = 0; cycle < 2; cycle++) {
                timer = timer(); when(plugin.getVoteTimer()).thenReturn(timer);
                listener = new VotiferEvent(plugin, ready::get);
                overflow = VotifierVoteOverflowQueue.initializeAsync(plugin, listener::processVote, ready::get);
                when(plugin.getVotifierVoteOverflowQueue()).thenReturn(overflow); workerTurn(overflow);
                assertEquals(512, overflow.size());
                listener.onVotiferEvent(receipt());
                assertEquals(0, listener.getPendingCaptureCount(), "full recovered ownership cannot admit another normal capture");
                assertFalse(overflow.enqueue("Steve", SITE, 123L));
                listener.stop(); workerTurn(overflow); overflow.close();
                assertEquals(ids, disk().stream().map(row -> UUID.fromString((String) row.get("LocalOccurrenceId")))
                        .collect(java.util.stream.Collectors.toSet()));
                assertTrue(processed.isEmpty()); timer.shutdownNow();
            }
        } finally { release.countDown(); }
    }

    @Test void fullQueueRetirementTransfersStartedOwnershipWithoutLosingTheReceipt() throws Exception {
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
            assertEquals(0, listener.getPendingCaptureCount()); assertEquals(257, overflow.size());
            timer.shutdownNow(); assertTrue(timer.awaitTermination(5, TimeUnit.SECONDS));
            assertEquals(0, listener.getPendingCaptureCount()); assertTrue(processed.isEmpty());
            overflow.close(); assertEquals(257, disk().size());
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
