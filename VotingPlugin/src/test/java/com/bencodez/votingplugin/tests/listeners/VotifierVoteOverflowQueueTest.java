package com.bencodez.votingplugin.tests.listeners;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.listeners.VotifierVoteOverflowQueue;

class VotifierVoteOverflowQueueTest {
	@Test
	void enqueueCannotChangeVersionDuringDurableAdmission(@TempDir Path dataFolder) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		CountDownLatch admissionStarted = new CountDownLatch(1);
		CountDownLatch releaseAdmission = new CountDownLatch(1);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		doAnswer(invocation -> {
			admissionStarted.countDown();
			releaseAdmission.await(2, TimeUnit.SECONDS);
			return null;
		}).when(voteTimer).submit(any(Runnable.class));

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		java.util.concurrent.ExecutorService enqueuer = Executors.newSingleThreadExecutor();
		try {
			assertTrue(queue.enqueue("Steve", "first.example.org"));
			queue.start();
			assertTrue(admissionStarted.await(2, TimeUnit.SECONDS));
			java.util.concurrent.Future<Boolean> second = enqueuer.submit(
					() -> queue.enqueue("Alex", "second.example.org"));
			assertThrows(TimeoutException.class, () -> second.get(100, TimeUnit.MILLISECONDS));
			releaseAdmission.countDown();
			assertTrue(second.get(2, TimeUnit.SECONDS));
		} finally {
			releaseAdmission.countDown();
			enqueuer.shutdownNow();
			queue.close();
		}
	}

	@Test
	void waitsForFullPipelineStartupBeforeDrainingPersistedVotes(@TempDir Path dataFolder) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		CountDownLatch admissionStarted = new CountDownLatch(1);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getVoteTimer()).thenReturn(voteTimer);

		VotifierVoteOverflowQueue seed = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		assertTrue(seed.enqueue("Steve", "example.org"));
		seed.close();

		doAnswer(invocation -> {
			admissionStarted.countDown();
			return null;
		}).when(voteTimer).submit(any(Runnable.class));

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertEquals(1, queue.size());
			verify(voteTimer, org.mockito.Mockito.never()).submit(any(Runnable.class));

			queue.start();
			assertTrue(admissionStarted.await(2, TimeUnit.SECONDS),
					"persisted vote was not admitted after the full pipeline started");
		} finally {
			queue.close();
		}
	}

	@Test
	void retainsUnacknowledgedCallbackAcrossShutdown(@TempDir Path dataFolder) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		ScheduledExecutorService voteTimer = Executors.newSingleThreadScheduledExecutor();
		CountDownLatch processingStarted = new CountDownLatch(1);
		CountDownLatch finishProcessing = new CountDownLatch(1);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getVoteTimer()).thenReturn(voteTimer);

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> {
			processingStarted.countDown();
			try {
				finishProcessing.await();
			} catch (InterruptedException interrupted) {
				Thread.currentThread().interrupt();
			}
		});
		try {
			assertTrue(queue.enqueue("Steve", "example.org"));
			queue.start();
			assertTrue(processingStarted.await(2, TimeUnit.SECONDS));
			queue.close();
			finishProcessing.countDown();
			voteTimer.shutdown();
			assertTrue(voteTimer.awaitTermination(2, TimeUnit.SECONDS));

			ScheduledExecutorService rejectingTimer = mock(ScheduledExecutorService.class);
			when(plugin.getVoteTimer()).thenReturn(rejectingTimer);
			doThrow(new RejectedExecutionException("capacity exhausted"))
					.when(rejectingTimer).submit(any(Runnable.class));
			VotifierVoteOverflowQueue restarted = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
			try {
				assertEquals(1, restarted.size());
			} finally {
				restarted.close();
			}
		} finally {
			finishProcessing.countDown();
			queue.close();
			voteTimer.shutdownNow();
		}
	}

	@Test
	void retriesPersistenceAfterTransientWriteFailure(@TempDir Path temporaryDirectory) throws Exception {
		Path dataFolder = temporaryDirectory.resolve("data");
		Files.writeString(dataFolder, "temporarily blocking the data directory");
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Logger logger = mock(Logger.class);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getLogger()).thenReturn(logger);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertTrue(queue.enqueue("Steve", "example.org"));
			verify(logger, timeout(2_000)).warning(org.mockito.ArgumentMatchers.contains(
					"Unable to persist queued Votifier votes"));
			Files.delete(dataFolder);
			Files.createDirectory(dataFolder);
			waitForFile(dataFolder.resolve("VotifierVoteQueue.yml"));
		} finally {
			queue.close();
		}
	}

	@Test
	void doesNotDrainUntilWriteFailureIsRecovered(@TempDir Path temporaryDirectory) throws Exception {
		Path dataFolder = temporaryDirectory.resolve("data");
		Files.writeString(dataFolder, "temporarily blocking the data directory");
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		Logger logger = mock(Logger.class);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		CountDownLatch submissionAccepted = new CountDownLatch(1);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getLogger()).thenReturn(logger);
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		doAnswer(invocation -> {
			submissionAccepted.countDown();
			return null;
		}).when(voteTimer).submit(any(Runnable.class));

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertTrue(queue.enqueue("Steve", "example.org"));
			queue.start();
			verify(logger, timeout(2_000)).warning(org.mockito.ArgumentMatchers.contains(
					"Unable to persist queued Votifier votes"));
			verify(voteTimer, org.mockito.Mockito.never()).submit(any(Runnable.class));

			Files.delete(dataFolder);
			Files.createDirectory(dataFolder);
			waitForFile(dataFolder.resolve("VotifierVoteQueue.yml"));
			assertTrue(submissionAccepted.await(2, TimeUnit.SECONDS),
					"queued vote was not admitted after persistence recovered");
		} finally {
			queue.close();
		}
	}

	@Test
	void retainsRejectedVoteAcrossQueueRestart(@TempDir Path dataFolder) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));

		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertTrue(queue.enqueue("Steve", "example.org"));
			waitForFile(dataFolder.resolve("VotifierVoteQueue.yml"));
		} finally {
			queue.close();
		}

		VotifierVoteOverflowQueue restarted = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertEquals(1, restarted.size());
		} finally {
			restarted.close();
		}
	}

	@Test
	void stalePersistenceWorkerCannotOverwriteShutdownSnapshot(@TempDir Path dataFolder) throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		when(plugin.getDataFolder()).thenReturn(dataFolder.toFile());
		when(plugin.getVoteTimer()).thenReturn(voteTimer);
		doThrow(new RejectedExecutionException("capacity exhausted"))
				.when(voteTimer).submit(any(Runnable.class));
		VotifierVoteOverflowQueue queue = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		java.lang.reflect.Field writeLockField = VotifierVoteOverflowQueue.class
				.getDeclaredField("persistenceWriteLock");
		writeLockField.setAccessible(true);
		Object writeLock = writeLockField.get(queue);
		Thread closer;
		synchronized (writeLock) {
			assertTrue(queue.enqueue("Steve", "first.example.org"));
			java.lang.reflect.Field dirtyField = VotifierVoteOverflowQueue.class.getDeclaredField("persistenceDirty");
			dirtyField.setAccessible(true);
			long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
			while ((boolean) dirtyField.get(queue) && System.nanoTime() < deadline) Thread.onSpinWait();
			assertEquals(false, dirtyField.get(queue), "persistence worker did not capture the older snapshot");
			assertTrue(queue.enqueue("Alex", "second.example.org"));
			closer = new Thread(queue::close);
			closer.start();
			Thread.sleep(1_100L);
		}
		closer.join(TimeUnit.SECONDS.toMillis(3));

		VotifierVoteOverflowQueue restarted = new VotifierVoteOverflowQueue(plugin, (site, user) -> { });
		try {
			assertEquals(2, restarted.size());
		} finally {
			restarted.close();
		}
	}

	private static void waitForFile(Path file) throws Exception {
		long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(2);
		while (!Files.exists(file) && System.nanoTime() < deadline) {
			Thread.sleep(10);
		}
		assertTrue(Files.exists(file), "overflow queue was not persisted");
	}
}
