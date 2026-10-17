package com.bencodez.votingplugin.tests.proxy;

import com.bencodez.votingplugin.tests.proxy.PluginMessageDispatcherAccess;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import org.junit.jupiter.api.Test;

class PluginMessageDispatcherTest {
    private record Wake(long due, Runnable action, ScheduledFuture<?> future) { }
    private static final class Harness {
        final AtomicLong clock = new AtomicLong();
        final ScheduledExecutorService scheduler = mock(ScheduledExecutorService.class);
        final List<Wake> wakes = new ArrayList<>();
        Harness() {
            when(scheduler.schedule(any(Runnable.class), anyLong(), eq(TimeUnit.NANOSECONDS)))
                    .thenAnswer(call -> {
                        ScheduledFuture<?> future = mock(ScheduledFuture.class);
                        when(future.cancel(false)).thenAnswer(cancel -> {
                            when(future.isCancelled()).thenReturn(true); return true;
                        });
                        wakes.add(new Wake(clock.get() + call.getArgument(1, Long.class), call.getArgument(0), future));
                        return future;
                    });
        }
        void tick(long milliseconds) {
            clock.addAndGet(TimeUnit.MILLISECONDS.toNanos(milliseconds));
            List<Wake> ready = wakes.stream().filter(w -> w.due <= clock.get()).toList();
            wakes.removeAll(ready);
            for (Wake wake : ready) {
                if (!wake.future.isCancelled()) { when(wake.future.isDone()).thenReturn(true); wake.action.run(); }
            }
        }
        PluginMessageDispatcherAccess dispatcher(java.util.function.Consumer<String> replay) {
            return new PluginMessageDispatcherAccess(clock::get, () -> scheduler, replay);
        }
    }

    @Test void expiredMetadataReleasesCapacityWithoutRetiringEffectBearingControls() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        assertTrue(d.enqueue("backend-a", 1, 0, false, () -> false));
        assertTrue(d.enqueue("backend-a", 1, 0, true, () -> false));
        h.tick(20);
        assertEquals(2, d.pendingControls());
        h.tick(30_000);
        assertEquals(1, d.pendingControls());
        assertTrue(d.hasPendingEffects());
        assertTrue(d.enqueue("backend-b", 1, 0, false, () -> true));
        h.tick(20);
        assertEquals(1, d.pendingControls());
    }

    @Test void cheapBudgetCheckDoesNotReserveOrBypassTheActualAttempt() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        assertTrue(d.hasBudget("backend-a", "vote-a", false));
        assertTrue(d.hasBudget("backend-a", "vote-a", false));
        assertTrue(d.trySend("backend-a", "vote-a", false, () -> true));
        assertFalse(d.hasBudget("backend-a", "vote-b", false));
        h.tick(20);
        assertFalse(d.hasBudget("backend-a", "vote-a", false));
        assertTrue(d.hasBudget("backend-a", "vote-b", false));
        assertTrue(d.trySend("backend-a", "vote-b", false, () -> true));
        d.close();
        assertFalse(d.hasBudget("backend-a", "vote-c", false));
    }

    @Test void concurrentProducersShareOneBackendBudget() throws Exception {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        CountDownLatch gate = new CountDownLatch(1);
        AtomicInteger accepted = new AtomicInteger();
        List<Thread> workers = new ArrayList<>();
        for (int i = 0; i < 24; i++) {
            Thread worker = new Thread(() -> {
                try { gate.await(); } catch (InterruptedException e) { throw new AssertionError(e); }
                d.trySend("backend-a", () -> { accepted.incrementAndGet(); return true; });
            });
            workers.add(worker); worker.start();
        }
        gate.countDown();
        for (Thread worker : workers) { worker.join(2000); assertFalse(worker.isAlive()); }
        assertEquals(1, accepted.get());
        h.tick(19);
        assertFalse(d.trySend("BACKEND-A", () -> { accepted.incrementAndGet(); return true; }));
        assertEquals(1, accepted.get());
        h.tick(1);
        assertTrue(d.trySend("backend-a", () -> true));
        assertTrue(d.trySend("backend-b", () -> true));
    }

    @Test void immediateAndDelayedControlsDoNotCatchUpAfterStall() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        List<Long> attempts = new ArrayList<>();
        assertTrue(d.trySend("backend-a", () -> { attempts.add(h.clock.get()); return true; }));
        for (int i = 0; i < 5; i++) assertTrue(d.enqueue("backend-a", 1, 0, () -> {
            attempts.add(h.clock.get()); return true;
        }));
        h.tick(1000); // Many overdue producers must still release one handoff.
        assertTrue(attempts.size() <= 2);
        int afterStall = attempts.size();
        h.tick(19); assertEquals(afterStall, attempts.size());
        h.tick(1); assertTrue(attempts.size() <= afterStall + 1);
        for (int i = 0; i < 10; i++) h.tick(20);
        assertEquals(6, attempts.size());
        for (int i = 1; i < attempts.size(); i++) assertTrue(attempts.get(i) - attempts.get(i - 1) >= PluginMessageDispatcherAccess.INTERVAL);
    }

    @Test void failedCarrierIsRetainedAndDoesNotBlockOtherBackend() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        AtomicInteger a = new AtomicInteger(), b = new AtomicInteger();
        assertTrue(d.enqueue("backend-a", 4, 0, () -> a.incrementAndGet() > 1));
        assertTrue(d.enqueue("backend-b", 4, 0, () -> { b.incrementAndGet(); return true; }));
        h.tick(20); assertEquals(1, a.get()); assertEquals(1, b.get());
        h.tick(999); assertEquals(1, a.get());
        h.tick(1); assertEquals(2, a.get());
    }

    @Test void delayedControlDoesNotBlockReadyMessageAndIdenticalMessagesRemainDistinct() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        List<String> seen = new ArrayList<>();
        d.enqueue("backend-a", 1, TimeUnit.SECONDS.toNanos(10), () -> { seen.add("delayed"); return true; });
        d.enqueue("backend-a", 1, 0, () -> { seen.add("ready"); return true; });
        h.tick(20);
        assertEquals(List.of("ready"), seen);
        d.enqueue("backend-a", 1, 0, () -> { seen.add("ready"); return true; });
        h.tick(20); assertEquals(List.of("ready", "ready"), seen);
    }

    @Test void queueCapacityAndSchedulerRejectionAreObservable() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_LANE_CONTROLS; i++) assertTrue(d.enqueue("backend-a", 1, 0, () -> false));
        assertFalse(d.enqueue("backend-a", 1, 0, () -> false));
        assertTrue(d.enqueue("backend-b", 1, 0, () -> true));
        d.close(); assertFalse(d.enqueue("backend-b", 1, 0, () -> true));
        h.tick(1000);
        Harness rejected = new Harness();
        when(rejected.scheduler.schedule(any(Runnable.class), anyLong(), any(TimeUnit.class)))
                .thenThrow(new java.util.concurrent.RejectedExecutionException());
        assertFalse(rejected.dispatcher(server -> { }).enqueue("backend-a", 1, 0, () -> true));
    }

    @Test void reliableWindowDoesNotPreventReceiptReleaseAndAcknowledgementCanBeReentrant() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_IN_FLIGHT; i++) {
            assertTrue(d.trySend("backend-a", "vote-" + i, false, () -> true)); h.tick(20);
        }
        assertFalse(d.trySend("backend-a", "extra", false, () -> true));
        assertTrue(d.trySend("backend-a", "release", true, () -> { d.acknowledge("backend-a", "release"); return true; }));
        h.tick(20);
        d.acknowledge("backend-a", "vote-0");
        assertTrue(d.trySend("backend-a", "extra", false, () -> true));
        h.tick(20);
        assertFalse(d.trySend("backend-a", "extra", false, () -> true));
        h.tick(10000);
        assertTrue(d.trySend("backend-a", "extra", false, () -> true));
    }

    @Test void closeInvalidatesScheduledCallbacksAndIsIdempotent() {
        Harness h = new Harness();
        AtomicInteger sends = new AtomicInteger();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> fail("closed owner callback"));
        d.enqueue("backend-a", 1, 0, () -> { sends.incrementAndGet(); return true; });
        d.close(); d.close(); h.tick(1000);
        assertEquals(0, sends.get()); assertFalse(d.trySend("backend-a", () -> true));
    }
    @Test void failedRetryKeepsPreviouslyAcceptedWindowOccupancy() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_IN_FLIGHT; i++) {
            assertTrue(d.trySend("backend-a", "vote-" + i, false, () -> true)); h.tick(20);
        }
        h.tick(10000);
        assertFalse(d.trySend("backend-a", "vote-0", false, () -> false));
        h.tick(1000);
        assertFalse(d.trySend("backend-a", "new-vote", false, () -> true));
    }

    @Test void outstandingReleasesDoNotConsumeVoteWindow() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        for (int i = 0; i < PluginMessageDispatcherAccess.MAX_IN_FLIGHT; i++) {
            assertTrue(d.trySend("backend-a", "release-" + i, true, () -> true)); h.tick(20);
        }
        assertTrue(d.trySend("backend-a", "new-vote", false, () -> true));
    }

    @Test void fullWindowWaitsForRetryDeadlineInsteadOfRescanningEveryInterval() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess[] dispatcher = new PluginMessageDispatcherAccess[1];
        AtomicInteger replays = new AtomicInteger();
        dispatcher[0] = h.dispatcher(server -> {
            replays.incrementAndGet();
            dispatcher[0].trySend(server, "vote", false, () -> fail("retry window was bypassed"));
        });
        assertTrue(dispatcher[0].trySend("backend-a", "vote", false, () -> true));
        h.tick(20); assertEquals(1, replays.get());
        for (int i = 0; i < 100; i++) h.tick(20);
        assertEquals(1, replays.get());
    }

    @Test void batchReservationDoesNotSendBeforeCommitAndCancellationReclaimsCapacity() {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        AtomicInteger sends = new AtomicInteger();
        PluginMessageDispatcherAccess.Batch batch = d.reserveBatch(java.util.Map.of("backend-a",
                () -> { sends.incrementAndGet(); return true; }), 4);
        assertNotNull(batch); h.tick(20); assertEquals(0, sends.get());
        batch.cancel(); h.tick(1000); assertEquals(0, sends.get());
        PluginMessageDispatcherAccess.Batch committed = d.reserveBatch(java.util.Map.of("backend-a",
                () -> { sends.incrementAndGet(); return true; }), 4);
        assertNotNull(committed); committed.activate(); h.tick(20); assertEquals(1, sends.get());
    }

    @Test void aBlockedBackendDoesNotHoldTheGlobalDispatcherMonitor() throws Exception {
        Harness h = new Harness();
        PluginMessageDispatcherAccess d = h.dispatcher(server -> { });
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        Thread blocked = new Thread(() -> d.trySend("backend-a", () -> {
            entered.countDown();
            try { return release.await(2, TimeUnit.SECONDS); } catch (InterruptedException e) { return false; }
        }));
        blocked.start(); assertTrue(entered.await(1, TimeUnit.SECONDS));
        try { assertTrue(d.trySend("backend-b", () -> true)); }
        finally { release.countDown(); blocked.join(2000); }
        assertFalse(blocked.isAlive());
    }
}
