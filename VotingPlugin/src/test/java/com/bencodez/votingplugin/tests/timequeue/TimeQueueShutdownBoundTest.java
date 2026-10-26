package com.bencodez.votingplugin.tests.timequeue;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

import java.util.Set;
import java.util.concurrent.*;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.timequeue.TimeQueueHandler;

class TimeQueueShutdownBoundTest {
    @Test void storageFailureAfterTimeoutIsReportedAndCanRetryAtTheSameIndex() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        // Resolve logging before parallel callbacks; avoid lazy deep-stub creation racing the interrupted caller.
        when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
        ServerData data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1), failed = new CountDownLatch(1);
        java.util.concurrent.atomic.AtomicBoolean fail = new java.util.concurrent.atomic.AtomicBoolean(true);
        doAnswer(call -> {
            if (fail.getAndSet(false)) {
                entered.countDown();
                assertTrue(release.await(10, TimeUnit.SECONDS));
                failed.countDown();
                throw new IllegalStateException("synthetic storage failure");
            }
            return null;
        }).when(data).addTimeVoted(anyInt(), any());
        handler.addVote("First", "site");
        ExecutorService caller = Executors.newSingleThreadExecutor();
        try {
            Future<?> shutdown = caller.submit(handler::saveForShutdown);
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            shutdown.get(2, TimeUnit.SECONDS);
            release.countDown();
            assertTrue(failed.await(5, TimeUnit.SECONDS));
            verify(plugin.getLogger(), timeout(5000)).warning(
                    "Timed vote shutdown persistence failed; pending votes remain owned for retry. Recovery is not confirmed.");
            assertEquals(1, handler.getTimeChangeQueue().size());
            try { handler.saveForShutdown(); }
            catch (IllegalStateException observedCompletion) {
                assertEquals("synthetic storage failure", observedCompletion.getMessage());
                handler.saveForShutdown();
            }
            assertTrue(handler.getTimeChangeQueue().isEmpty());
            verify(data, times(2)).addTimeVoted(eq(0), any());
        } finally {
            release.countDown();
            caller.shutdownNow();
            assertTrue(caller.awaitTermination(5, TimeUnit.SECONDS));
        }
    }

    @Test void interruptedWaitPreservesTheFlagAndLeavesPersistenceOwned() throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        // Resolve logging before parallel callbacks; avoid lazy deep-stub creation racing the interrupted caller.
        when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
        ServerData data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        CountDownLatch entered = new CountDownLatch(1), release = new CountDownLatch(1);
        doAnswer(call -> { entered.countDown(); assertTrue(release.await(5, TimeUnit.SECONDS)); return null; })
                .when(data).addTimeVoted(anyInt(), any());
        handler.addVote("First", "site");
        try {
            Thread.currentThread().interrupt();
            handler.saveForShutdown();
            assertTrue(Thread.currentThread().isInterrupted());
            Thread.interrupted();
            assertTrue(entered.await(5, TimeUnit.SECONDS));
            assertEquals(1, handler.getTimeChangeQueue().size());
        } finally {
            Thread.interrupted();
            release.countDown();
            handler.saveForShutdown();
        }
        assertTrue(handler.getTimeChangeQueue().isEmpty());
        verify(data, times(1)).addTimeVoted(eq(0), any());
    }

    @Test void stalledPreparationDoesNotBlockShutdownOrAuthorizeAnEventAfterStopping() throws Exception {
        exerciseStalledWrite("prepare");
    }

    @Test void stalledCompletionDoesNotBlockShutdownOrRepeatTheCompletedEvent() throws Exception {
        exerciseStalledWrite("complete");
    }

    @Test void stalledShutdownWriteRetainsOneOwnerAndDoesNotBlockTheLifecycleCaller() throws Exception {
        exerciseStalledWrite("shutdown");
    }

    private void exerciseStalledWrite(String phase) throws Exception {
        VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
        // Resolve logging before parallel callbacks; avoid lazy deep-stub creation racing the interrupted caller.
        when(plugin.getLogger()).thenReturn(mock(java.util.logging.Logger.class));
        ServerData data = mock(ServerData.class);
        when(plugin.getServerData()).thenReturn(data);
        when(data.getTimedVoteCacheKeys()).thenReturn(Set.of());
        TimeQueueHandler handler = new TimeQueueHandler(plugin);
        CountDownLatch entered = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        org.mockito.stubbing.Answer<Object> stalled = call -> {
            entered.countDown();
            boolean interrupted = false;
            try {
                while (true) {
                    try { assertTrue(release.await(10, TimeUnit.SECONDS)); break; }
                    catch (InterruptedException ignored) { interrupted = true; }
                }
            } finally { if (interrupted) Thread.currentThread().interrupt(); }
            return null;
        };
        if (phase.equals("prepare")) doAnswer(stalled).when(data).prepareTimedVoteDispatch(any(), any(), any());
        if (phase.equals("complete")) doAnswer(stalled).when(data).completeTimedVoteDispatch(any());
        if (phase.equals("shutdown")) doAnswer(stalled).when(data).addTimeVoted(anyInt(), any());
        handler.addVote("First", "site");
        handler.addVote("Later", "site");
        ExecutorService callers = Executors.newFixedThreadPool(3);
        try {
            Future<?> processing = phase.equals("shutdown") ? null : callers.submit(handler::processQueue);
            Future<?> shutdown;
            if (processing == null) {
                shutdown = callers.submit(handler::saveForShutdown);
                assertTrue(entered.await(5, TimeUnit.SECONDS));
            } else {
                assertTrue(entered.await(5, TimeUnit.SECONDS));
                shutdown = callers.submit(handler::saveForShutdown);
            }
            shutdown.get(2, TimeUnit.SECONDS);
            // Repeated unload must join the same retained persistence owner.
            Future<?> repeated = callers.submit(handler::saveForShutdown);
            repeated.get(2, TimeUnit.SECONDS);
            assertFalse(handler.getTimeChangeQueue().isEmpty());
            release.countDown();
            if (processing != null) processing.get(5, TimeUnit.SECONDS);
            handler.saveForShutdown();
            assertTrue(handler.getTimeChangeQueue().isEmpty());
            verify(plugin.getServer().getPluginManager(), times(phase.equals("complete") ? 1 : 0)).callEvent(any());
            if (phase.equals("prepare")) verify(data, times(1)).abortTimedVoteDispatch(any(),
                    argThat(pending -> pending.stream().map(vote -> vote.getName()).toList()
                            .equals(java.util.List.of("First", "Later"))));
            else verify(data, times(1)).addTimeVoted(eq(phase.equals("shutdown") ? 1 : 0),
                    argThat(vote -> vote.getName().equals("Later")));
            if (phase.equals("shutdown")) verify(data, times(1)).addTimeVoted(eq(0),
                    argThat(vote -> vote.getName().equals("First")));
        } finally {
            release.countDown();
            callers.shutdownNow();
            assertTrue(callers.awaitTermination(5, TimeUnit.SECONDS));
            handler.saveForShutdown();
        }
    }
}
