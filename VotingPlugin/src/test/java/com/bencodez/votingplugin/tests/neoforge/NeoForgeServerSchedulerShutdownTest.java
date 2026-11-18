package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Queue;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.neoforge.NeoForgeServerScheduler;

class NeoForgeServerSchedulerShutdownTest {
    @Test
    void closeRejectsSelectedAndQueuedTasksBeforeTickResumes() throws Exception {
        var scheduler = new NeoForgeServerScheduler();
        AtomicInteger effects = new AtomicInteger();
        var first = NeoForgeServerSchedulerAccess.executeAsync(scheduler, effects::incrementAndGet);
        var second = NeoForgeServerSchedulerAccess.executeAsync(scheduler, effects::incrementAndGet);
        var queued = NeoForgeServerSchedulerAccess.executeAsync(scheduler, effects::incrementAndGet);
        Runnable selectedFirst = removeNextTask(scheduler);
        Runnable selectedSecond = removeNextTask(scheduler);
        scheduler.close();
        for (var result : java.util.List.of(first, second, queued)) {
            assertTrue(result.isDone(), "Shutdown must resolve tasks without waiting for the tick to resume");
            var failure = assertThrows(java.util.concurrent.CompletionException.class, result::join);
            assertInstanceOf(java.util.concurrent.RejectedExecutionException.class, failure.getCause());
        }
        selectedFirst.run();
        selectedSecond.run();
        assertEquals(0, effects.get());
    }

    @Test
    void closeFromRunningTaskCancelsRemainingTasksWithoutDeadlock() {
        var scheduler = new NeoForgeServerScheduler();
        AtomicInteger effects = new AtomicInteger();
        scheduler.execute(scheduler::close);
        var remaining = NeoForgeServerSchedulerAccess.executeAsync(scheduler, effects::incrementAndGet);
        scheduler.onServerTick();
        assertEquals(0, effects.get());
        var failure = assertThrows(java.util.concurrent.CompletionException.class, remaining::join);
        assertInstanceOf(java.util.concurrent.RejectedExecutionException.class, failure.getCause());
    }

    @Test
    void taskSelectedBeforeShutdownCannotStartAfterClose() throws Exception {
        var scheduler = new NeoForgeServerScheduler();
        AtomicInteger effects = new AtomicInteger();
        scheduler.execute(effects::incrementAndGet);
        Runnable selected = removeNextTask(scheduler);
        scheduler.close();
        selected.run();
        assertEquals(0, effects.get(), "Shutdown must reject a dequeued but not yet started task");
    }

    static Runnable removeNextTask(NeoForgeServerScheduler scheduler) throws Exception {
        var field = NeoForgeServerScheduler.class.getDeclaredField("pending");
        field.setAccessible(true);
        return (Runnable) ((Queue<?>) field.get(scheduler)).remove();
    }
}
