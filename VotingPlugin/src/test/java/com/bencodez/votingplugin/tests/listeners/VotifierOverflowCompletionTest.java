package com.bencodez.votingplugin.tests.listeners;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import org.bukkit.configuration.file.YamlConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue;

class VotifierOverflowCompletionTest {
    private static final UUID OCCURRENCE = UUID.fromString("00000000-0000-0000-0000-000000000019");
    private static final long OCCURRED_AT = 123L;
    private static final String SERVICE = "example.org";

    private VotingPluginMain plugin(Path directory, ScheduledExecutorService timer) {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        when(plugin.getDataFolder()).thenReturn(directory.toFile());
        when(plugin.getVoteTimer()).thenReturn(timer);
        when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
        when(plugin.getOptions().getBedrockPlayerPrefix()).thenReturn(".");
        return plugin;
    }

    private ScheduledExecutorService inlineTimer() {
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        doAnswer(call -> {
            call.getArgument(0, Runnable.class).run();
            return CompletableFuture.completedFuture(null);
        }).when(timer).submit(any(Runnable.class));
        return timer;
    }

    private ScheduledExecutorService capturedTimer(BlockingQueue<Runnable> tasks) {
        ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
        doAnswer(call -> {
            tasks.add(call.getArgument(0, Runnable.class));
            return CompletableFuture.completedFuture(null);
        }).when(timer).submit(any(Runnable.class));
        return timer;
    }

    private void enqueue(VotifierVoteOverflowQueue queue) throws Exception {
        var admitted = new CompletableFuture<Boolean>();
        queue.enqueueAfterInitialization("Steve", SERVICE, OCCURRED_AT, OCCURRENCE, admitted::complete);
        assertTrue(admitted.get(3, TimeUnit.SECONDS));
    }

    private List<Map<?, ?>> rows(Path directory) {
        return new ArrayList<>(YamlConfiguration.loadConfiguration(directory.resolve("VotifierVoteQueue.yml").toFile()).getMapList("Votes"));
    }

    private Runnable take(BlockingQueue<Runnable> tasks) throws Exception {
        Runnable task = tasks.poll(3, TimeUnit.SECONDS);
        assertNotNull(task, "overflow did not admit a pending receipt");
        return task;
    }

    @Test void inlineFailureRetriesAfterDelayWithSameIdentityAndAcknowledgesOnlyCompletion(@TempDir Path directory) throws Exception {
        AtomicInteger attempts = new AtomicInteger();
        AtomicInteger prefixEffects = new AtomicInteger();
        AtomicLong firstAttempt = new AtomicLong();
        CompletableFuture<Long> retried = new CompletableFuture<>();
        VotingPluginMain plugin = plugin(directory, inlineTimer());
        try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (site, user, time, id) -> {
            assertEquals(SERVICE, site); assertEquals("Steve", user);
            assertEquals(OCCURRED_AT, time); assertEquals(OCCURRENCE, id);
            prefixEffects.incrementAndGet(); // Ordinary effects can precede an uncertain failure.
            if (attempts.incrementAndGet() == 1) { firstAttempt.set(System.nanoTime()); return false; }
            retried.complete(System.nanoTime() - firstAttempt.get());
            return true;
        }, () -> true)) {
            enqueue(queue); queue.start();
            assertTrue(retried.get(3, TimeUnit.SECONDS) >= TimeUnit.MILLISECONDS.toNanos(250),
                    "inline failure cannot retry in the same drain or bypass its backoff");
            queue.close();
            assertEquals(2, attempts.get());
            assertEquals(2, prefixEffects.get(), "the existing at-least-once contract does not roll back prefix effects");
            assertTrue(rows(directory).isEmpty());
        }
    }

    @Test void legacyOccurrenceCallbackExceptionRetainsReceiptAndDoesNotStrandInlineDrain(@TempDir Path directory) throws Exception {
        AtomicInteger attempts = new AtomicInteger();
        var completed = new CompletableFuture<Void>();
        VotingPluginMain plugin = plugin(directory, inlineTimer());
        try (var queue = new VotifierVoteOverflowQueue(plugin, (site, user, time, id) -> {
            assertEquals(OCCURRED_AT, time); assertEquals(OCCURRENCE, id);
            if (attempts.incrementAndGet() == 1) throw new IllegalStateException("synthetic callback failure");
            completed.complete(null);
        }, () -> true)) {
            assertTrue(queue.enqueue("Steve", SERVICE, OCCURRED_AT, OCCURRENCE)); queue.start();
            completed.get(3, TimeUnit.SECONDS);
            queue.close();
            assertEquals(2, attempts.get());
            assertTrue(rows(directory).isEmpty());
        }
    }

    @Test void failedConfirmedAttemptSurvivesRestartWithOriginalIdentity(@TempDir Path directory) throws Exception {
        for (boolean exceptional : List.of(false, true)) {
            Path data = directory.resolve(exceptional ? "exception" : "unconfirmed");
            BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
            VotingPluginMain plugin = plugin(data, capturedTimer(tasks));
            try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (site, user, time, id) -> {
                if (exceptional) throw new IllegalStateException("synthetic callback failure");
                return false;
            }, () -> true)) {
                enqueue(queue); queue.start(); Runnable attempt = take(tasks);
                if (exceptional) assertThrows(IllegalStateException.class, attempt::run); else attempt.run();
                assertEquals(1, queue.size());
            }
            assertEquals(OCCURRENCE.toString(), rows(data).getFirst().get("LocalOccurrenceId"));
            assertEquals(OCCURRED_AT, ((Number) rows(data).getFirst().get("Time")).longValue());
            tasks.clear();
            AtomicInteger processed = new AtomicInteger();
            try (var restarted = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (site, user, time, id) -> {
                assertEquals(OCCURRED_AT, time); assertEquals(OCCURRENCE, id); processed.incrementAndGet(); return true;
            }, () -> true)) {
                restarted.start(); take(tasks).run();
                assertEquals(0, restarted.size());
            }
            assertEquals(1, processed.get()); assertTrue(rows(data).isEmpty());
        }
    }

    @Test void interruptedReturnDoesNotConfirmReceiptCompletion(@TempDir Path directory) throws Exception {
        BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
        VotingPluginMain plugin = plugin(directory, capturedTimer(tasks));
        try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin, (site, user, time, id) -> {
            Thread.currentThread().interrupt(); return true;
        }, () -> true)) {
            enqueue(queue); queue.start();
            try {
                take(tasks).run();
                assertTrue(Thread.currentThread().isInterrupted());
                assertEquals(1, queue.size(), "interrupted completion retains the uncertain receipt");
            } finally { Thread.interrupted(); }
        }
        assertEquals(OCCURRENCE.toString(), rows(directory).getFirst().get("LocalOccurrenceId"));
    }

    @Test void callbackAdmittedBeforeCloseCannotRunOrOverwriteSuccessorQueue(@TempDir Path directory) throws Exception {
        BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
        VotingPluginMain plugin = plugin(directory, capturedTimer(tasks));
        AtomicInteger processed = new AtomicInteger();
        Runnable stale;
        try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin,
                (site, user, time, id) -> { processed.incrementAndGet(); return true; }, () -> true)) {
            enqueue(queue); queue.start(); stale = take(tasks);
        }
        try (var successor = new VotifierVoteOverflowQueue(plugin, (site, user) -> { })) {
            assertTrue(successor.enqueue("Alex", "second.example.org", 456L));
            stale.run(); assertEquals(0, processed.get()); assertEquals(2, successor.size());
        }
        assertEquals(2, rows(directory).size());
        assertEquals(OCCURRENCE.toString(), rows(directory).getFirst().get("LocalOccurrenceId"));
    }

    @Test void retiredGenerationRetainsAdmittedReceiptWithoutInvokingProcessor(@TempDir Path directory) throws Exception {
        BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
        VotingPluginMain plugin = plugin(directory, capturedTimer(tasks));
        var active = new java.util.concurrent.atomic.AtomicBoolean(true);
        AtomicInteger processed = new AtomicInteger();
        try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin,
                (site, user, time, id) -> { processed.incrementAndGet(); return true; }, active::get)) {
            enqueue(queue); queue.start(); Runnable admitted = take(tasks);
            active.set(false); admitted.run();
            assertEquals(0, processed.get()); assertEquals(1, queue.size());
            assertTrue(tasks.isEmpty(), "a retired callback must not schedule an immediate retry");
        }
        assertEquals(OCCURRENCE.toString(), rows(directory).getFirst().get("LocalOccurrenceId"));
    }

    @Test void retiredInlineGenerationParksBeforeSubmittingCallbacks(@TempDir Path directory) throws Exception {
        ScheduledExecutorService timer = inlineTimer();
        VotingPluginMain plugin = plugin(directory, timer);
        var checked = new CompletableFuture<Void>();
        try (var queue = VotifierVoteOverflowQueue.initializeConfirmedAsync(plugin,
                (site, user, time, id) -> { fail("retired generation cannot process receipts"); return true; },
                () -> { checked.complete(null); return false; })) {
            enqueue(queue); queue.start(); checked.get(3, TimeUnit.SECONDS);
            assertEquals(1, queue.size());
            verify(timer, never()).submit(any(Runnable.class));
        }
        assertEquals(1, rows(directory).size());
    }

    @Test void legacyTwoAndThreeArgumentCallbacksStillCompleteNormally(@TempDir Path directory) throws Exception {
        for (int arity : List.of(2, 3)) {
            Path data = directory.resolve("callback-" + arity);
            BlockingQueue<Runnable> tasks = new LinkedBlockingQueue<>();
            VotingPluginMain plugin = plugin(data, capturedTimer(tasks));
            AtomicInteger processed = new AtomicInteger();
            VotifierVoteOverflowQueue queue = arity == 2
                    ? new VotifierVoteOverflowQueue(plugin, (site, user) -> processed.incrementAndGet())
                    : new VotifierVoteOverflowQueue(plugin, (site, user, time) -> {
                        assertEquals(OCCURRED_AT, time); processed.incrementAndGet();
                    }, () -> true);
            try (queue) {
                assertTrue(queue.enqueue("Steve", SERVICE, OCCURRED_AT, OCCURRENCE));
                queue.start(); take(tasks).run(); assertEquals(0, queue.size());
            }
            assertEquals(1, processed.get()); assertTrue(rows(data).isEmpty());
        }
    }
}
