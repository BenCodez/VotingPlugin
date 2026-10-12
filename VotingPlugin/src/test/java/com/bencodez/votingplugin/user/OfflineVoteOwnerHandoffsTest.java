package com.bencodez.votingplugin.user;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BooleanSupplier;

import org.junit.jupiter.api.Test;

class OfflineVoteOwnerHandoffsTest {
    @Test void reloadInvalidatesOldAdmissionAndResubmitsOriginalCompletionOnce() {
        var handoffs = new OfflineVoteOwnerHandoffs();
        var callbacks = new ArrayList<BooleanSupplier>();
        var result = new CompletableFuture<Void>();
        handoffs.submit(result, callbacks::add);
        assertEquals(1, handoffs.pendingCount());
        handoffs.pause();
        assertFalse(callbacks.getFirst().getAsBoolean());
        assertFalse(result.isDone());
        handoffs.resume();
        assertEquals(2, callbacks.size());
        assertFalse(callbacks.getFirst().getAsBoolean());
        assertTrue(callbacks.getLast().getAsBoolean());
        assertFalse(callbacks.getLast().getAsBoolean());
        assertEquals(0, handoffs.pendingCount());
    }

    @Test void admissionsDuringReloadWaitAndAlreadyStartedCallbacksAreNotReplayed() {
        var handoffs = new OfflineVoteOwnerHandoffs();
        var submissions = new AtomicInteger();
        handoffs.pause();
        handoffs.submit(new CompletableFuture<>(), begin -> {
            submissions.incrementAndGet();
            assertTrue(begin.getAsBoolean());
        });
        assertEquals(0, submissions.get());
        handoffs.resume();
        handoffs.pause(); handoffs.resume();
        assertEquals(1, submissions.get());
    }

    @Test void shutdownSettlesCanceledAdmissionAndCannotRunStaleCallback() {
        var handoffs = new OfflineVoteOwnerHandoffs();
        var callbacks = new ArrayList<BooleanSupplier>();
        var result = new CompletableFuture<Void>();
        handoffs.submit(result, callbacks::add);
        handoffs.close(); handoffs.close(); handoffs.resume();
        assertTrue(result.isCompletedExceptionally());
        assertFalse(callbacks.getFirst().getAsBoolean());
        assertEquals(0, handoffs.pendingCount());
        var late = new CompletableFuture<Void>();
        handoffs.submit(late, callbacks::add);
        assertTrue(late.isCompletedExceptionally());
        assertEquals(1, callbacks.size());
    }

    @Test void admissionFailureIsExplicitAndCapacityIsBounded() {
        var handoffs = new OfflineVoteOwnerHandoffs();
        var failed = new CompletableFuture<Void>();
        handoffs.submit(failed, begin -> { throw new IllegalStateException("rejected"); });
        assertTrue(failed.isCompletedExceptionally());
        assertEquals(0, handoffs.pendingCount());
        for (int i = 0; i < 4096; i++) handoffs.submit(new CompletableFuture<>(), begin -> { });
        var overflow = new CompletableFuture<Void>();
        handoffs.submit(overflow, begin -> fail("capacity cannot admit another task"));
        assertTrue(overflow.isCompletedExceptionally());
        assertEquals(4096, handoffs.pendingCount());
        handoffs.close();
    }
}
