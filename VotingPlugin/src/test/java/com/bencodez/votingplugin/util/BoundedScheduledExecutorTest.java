package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

class BoundedScheduledExecutorTest {
	@Test
	void rejectsExcessTasksAndReleasesCapacityAfterCompletion() throws Exception {
		BoundedScheduledExecutor executor = new BoundedScheduledExecutor(1, 2);
		CountDownLatch blocker = new CountDownLatch(1);
		try {
			executor.submit(() -> await(blocker));
			executor.submit(() -> { });
			assertThrows(RejectedExecutionException.class, () -> executor.submit(() -> { }));
			blocker.countDown();
			executor.shutdown();
			assertTrueAwait(executor);
		} finally {
			blocker.countDown();
			executor.shutdownNow();
		}
	}

	@Test
	void cancellationReleasesCapacity() {
		BoundedScheduledExecutor executor = new BoundedScheduledExecutor(1, 1);
		try {
			java.util.concurrent.ScheduledFuture<?> future = executor.schedule(() -> { }, 1, TimeUnit.HOURS);
			future.cancel(false);
			org.junit.jupiter.api.Assertions.assertDoesNotThrow(
					() -> executor.schedule(() -> { }, 1, TimeUnit.HOURS));
		} finally {
			executor.shutdownNow();
		}
	}

	private static void await(CountDownLatch latch) {
		try { latch.await(); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
	}

	private static void assertTrueAwait(BoundedScheduledExecutor executor) throws Exception {
		org.junit.jupiter.api.Assertions.assertTrue(executor.awaitTermination(2, TimeUnit.SECONDS));
	}
}
