package com.bencodez.votingplugin.maintenance;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.AbstractExecutorService;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Delayed;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.core.maintenance.AutoCreateVoteSitesReminderState.Snapshot;
import com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService.Settings;
import com.bencodez.votingplugin.util.DurableFiles;
import com.bencodez.votingplugin.util.DurableFiles.PublishedException;

class AutoCreateVoteSitesReminderServiceTest {

	@TempDir
	Path directory;
	private final List<Harness> harnesses = new ArrayList<>();

	@AfterEach
	void closeHarnessesAndVerifyLeasesAreReleased() throws Exception {
		for (Harness h : harnesses) {
			h.onPublication = null;
			h.onBeforePublication = null;
			h.onClockRead = null;
			h.failure = Failure.NONE;
			h.factoryFailure = false;
			h.service.closeAsync();
			while (!h.executor.queued.isEmpty()) h.executor.runNext();
		}
		for (Path path : harnesses.stream().map(h -> h.directory).distinct().toList()) {
			AutoCreateVoteSitesReminderStore probe = new AutoCreateVoteSitesReminderStore(path);
			try {
				assertTrue(probe.tryAcquireLease(), "Final checkpoint must release the worker lease");
			} finally {
				probe.releaseLease();
			}
		}
	}

	@Test
	void constructorStartReloadAndCloseDeferFileWorkToExecutor() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		assertEquals(0, h.factories);
		h.service.start();
		h.service.reload(new Settings(false, false, 1000, 500));
		CompletableFuture<Void> closed = h.service.closeAsync();
		assertFalse(closed.isDone());
		assertEquals(0, h.factories);
		assertEquals(0, h.publications);
		assertFalse(Files.exists(h.target()));
		assertEquals(1, h.executor.queued.size());
		h.executor.runNext();
		assertTrue(closed.isDone());
		closed.join();
		assertEquals(1, h.factories);
		assertEquals(new Snapshot(0, -1), h.saved());
	}

	@Test
	void repeatedStartAndReloadCreateExactlyOnePeriodicTask() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.service.start();
		h.service.reload(enabled());
		h.service.start();
		h.service.reload(new Settings(false, true, 1000, 500));
		assertEquals(1, h.executor.schedules);
		assertEquals(0, h.executor.initialDelay);
		assertEquals(60, h.executor.delay);
		assertEquals(TimeUnit.SECONDS, h.executor.unit);
		assertEquals(0, h.factories);
		h.executor.firePeriodic();
		assertEquals(1, h.factories);
		h.service.reload(enabled());
		h.executor.firePeriodic();
		assertEquals(1, h.factories);
		assertEquals(1, h.executor.schedules);
	}

	@Test
	void firstAndRepeatedWarningsFollowInclusiveRuntimeBoundaries() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.tickAt(999);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(999, -1), h.saved());
		h.tickAt(1000);
		assertEquals(1, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
		h.tickAt(1499);
		assertEquals(1, h.reminders());
		h.tickAt(1500);
		assertEquals(2, h.reminders());
		assertEquals(new Snapshot(1500, 1500), h.saved());
	}

	@Test
	void disabledFlagsRetainRuntimeAndWarnAfterBothBecomeEnabled() throws Exception {
		Settings[] disabled = { new Settings(false, false, 1000, 500), new Settings(false, true, 1000, 500),
				new Settings(true, false, 1000, 500) };
		for (int i = 0; i < disabled.length; i++) {
			Harness h = new Harness(Files.createDirectory(directory.resolve("case-" + i)), 0, disabled[i]);
			h.service.start();
			h.tickAt(1500);
			assertEquals(0, h.reminders());
			assertEquals(new Snapshot(1500, -1), h.saved());
			h.service.reload(enabled());
			h.tickAt(1500);
			assertEquals(1, h.reminders());
			assertEquals(new Snapshot(1500, 1500), h.saved());
		}
	}

	@Test
	void storePublicationCompletesBeforeConsoleReminder() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.tickAt(1000);
		assertEquals(List.of("store-created", "persisted", "reminder"), h.events);
		assertEquals(List.of(new Snapshot(1000, 1000)), h.snapshotsAtReminder);
	}

	@Test
	void ordinarySaveFailureSuppressesReminderAndPausesUntilReload() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.tickAt(100);
		h.failure = Failure.BEFORE_PUBLICATION;
		h.tickAt(1000);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(100, -1), h.saved());
		assertEquals(1, h.warnings.size());
		assertEquals(2, h.publications);
		h.tickAt(2000);
		assertEquals(1, h.warnings.size());
		assertEquals(2, h.publications);
		h.failure = Failure.NONE;
		h.service.reload(enabled());
		h.tickAt(2000);
		assertEquals(new Snapshot(100, -1), h.saved());
		assertEquals(0, h.reminders());
		h.tickAt(2900);
		assertEquals(1, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
	}

	@Test
	void postPublicationFailureDoesNotWarnAndRecoveryPreservesPublishedMarker() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.failure = Failure.AFTER_PUBLICATION;
		h.tickAt(1000);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
		assertEquals(1, h.warnings.size());
		h.tickAt(2000);
		assertEquals(1, h.publications);
		assertEquals(1, h.warnings.size());
		h.failure = Failure.NONE;
		h.service.reload(enabled());
		h.tickAt(2000);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
		h.tickAt(2499);
		assertEquals(0, h.reminders());
		h.tickAt(2500);
		assertEquals(1, h.reminders());
		assertEquals(new Snapshot(1500, 1500), h.saved());
	}

	@Test
	void corruptInitialLoadIsNotOverwrittenAndDoesNotLogOnEveryTick() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		byte[] invalid = "corrupt-checkpoint".getBytes(java.nio.charset.StandardCharsets.US_ASCII);
		Files.write(h.target(), invalid);
		h.service.start();
		h.tickAt(1000);
		assertEquals(0, h.publications);
		assertEquals(0, h.reminders());
		assertEquals(1, h.warnings.size());
		assertArrayEquals(invalid, Files.readAllBytes(h.target()));
		h.tickAt(2000);
		assertEquals(0, h.publications);
		assertEquals(1, h.warnings.size());
		assertArrayEquals(invalid, Files.readAllBytes(h.target()));
		Files.delete(h.target());
		new AutoCreateVoteSitesReminderStore(directory).save(new Snapshot(1000, 1000));
		h.service.reload(enabled());
		h.tickAt(2000);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
		h.tickAt(2500);
		assertEquals(1, h.reminders());
	}

	@Test
	void factoryFailureIsDeferredAndRecoveryRetriesOnlyAfterReload() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.factoryFailure = true;
		h.service.start();
		assertEquals(0, h.factories);
		h.tickAt(1000);
		assertEquals(1, h.factories);
		assertEquals(1, h.warnings.size());
		h.tickAt(2000);
		assertEquals(1, h.factories);
		assertEquals(1, h.warnings.size());
		h.factoryFailure = false;
		h.service.reload(enabled());
		h.tickAt(2000);
		assertEquals(2, h.factories);
		assertEquals(new Snapshot(0, -1), h.saved());
		assertEquals(0, h.reminders());
	}

	@Test
	void settingsChangeDuringPublicationSuppressesStaleConsoleReminder() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.onPublication = () -> h.service.reload(new Settings(false, false, 1000, 500));
		h.tickAt(1000);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(1000, 1000), h.saved());
		h.tickAt(1500);
		assertEquals(0, h.reminders());
		assertEquals(new Snapshot(1500, 1000), h.saved());
	}

	@Test
	void shutdownDuringPublicationSuppressesWarningAndQueuesOneFinalCheckpoint() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		List<CompletableFuture<Void>> closeResults = new ArrayList<>();
		h.onPublication = () -> {
			h.clock.set(nanos(1500));
			closeResults.add(h.service.closeAsync());
		};
		h.tickAt(1000);
		assertEquals(0, h.reminders());
		assertTrue(h.executor.periodicFuture.cancelled);
		assertEquals(1, h.executor.queued.size());
		assertFalse(closeResults.get(0).isDone());
		h.clock.set(nanos(9000));
		h.executor.runNext();
		closeResults.get(0).join();
		assertEquals(new Snapshot(1500, 1000), h.saved());
		assertEquals(0, h.reminders());
	}

	@Test
	void repeatedCloseSharesFutureAndFinalSampleExcludesQueuedDelay() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.tickAt(100);
		h.clock.set(nanos(300));
		CompletableFuture<Void> first = h.service.closeAsync();
		assertSame(first, h.service.closeAsync());
		assertFalse(first.isDone());
		assertTrue(h.executor.periodicFuture.cancelled);
		assertEquals(1, h.executor.executions);
		assertEquals(new Snapshot(100, -1), h.saved());
		h.clock.set(nanos(TimeUnit.DAYS.toMillis(30)));
		h.executor.firePeriodic();
		assertEquals(1, h.publications);
		h.executor.runNext();
		assertTrue(first.isDone());
		first.join();
		assertSame(first, h.service.closeAsync());
		h.service.start();
		h.service.reload(enabled());
		assertEquals(1, h.executor.schedules);
		assertEquals(1, h.executor.executions);
		assertEquals(2, h.publications);
		assertEquals(new Snapshot(300, -1), h.saved());
		assertEquals(0, h.reminders());
	}

	@Test
	void tickSamplingAfterCloseCannotAccruePastCapturedStopTime() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.clock.set(nanos(1000));
		List<CompletableFuture<Void>> closeResults = new ArrayList<>();
		h.onClockRead = () -> {
			closeResults.add(h.service.closeAsync());
			h.clock.set(nanos(2000));
		};
		h.executor.firePeriodic();
		assertEquals(0, h.reminders());
		h.executor.runNext();
		closeResults.get(0).join();
		assertEquals(new Snapshot(1000, -1), h.saved());
	}

	@Test
	void closeBeforeStartCompletesWithoutSchedulingOrFileWork() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		CompletableFuture<Void> closed = h.service.closeAsync();
		assertTrue(closed.isDone());
		closed.join();
		assertSame(closed, h.service.closeAsync());
		h.service.start();
		assertEquals(0, h.executor.schedules);
		assertEquals(0, h.executor.executions);
		assertEquals(0, h.factories);
		assertFalse(Files.exists(h.target()));
	}

	@Test
	void failedFinalCheckpointCompletesExceptionallyWithoutSetupReminder() throws Exception {
		Harness h = new Harness(directory, 0, enabled());
		h.service.start();
		h.tickAt(100);
		h.failure = Failure.BEFORE_PUBLICATION;
		h.clock.set(nanos(1000));
		CompletableFuture<Void> closed = h.service.closeAsync();
		h.executor.runNext();
		assertTrue(closed.isCompletedExceptionally());
		assertSame(closed, h.service.closeAsync());
		assertEquals(new Snapshot(100, -1), h.saved());
		assertEquals(0, h.reminders());
		assertEquals(1, h.warnings.size());
		assertEquals(1, h.executor.executions);
		AutoCreateVoteSitesReminderStore probe = new AutoCreateVoteSitesReminderStore(directory);
		try {
			assertTrue(probe.tryAcquireLease(), "A failed final checkpoint must release its lease");
		} finally {
			probe.releaseLease();
		}
	}

	@Test
	void retiringWriterDefersSuccessorUntilFinalPublicationAndLeaseRelease() throws Exception {
		Settings settings = new Settings(true, true, 2000, 1000);
		Harness retiring = new Harness(directory, 0, settings);
		retiring.service.start();
		retiring.tickAt(0);
		retiring.clock.set(nanos(1000));
		CompletableFuture<Void> retired = retiring.service.closeAsync();
		Harness successor = new Harness(directory, nanos(1000), settings);
		successor.service.start();
		retiring.onBeforePublication = () -> {
			successor.tickAt(2000);
			successor.tickAt(2000);
			assertEquals(0, successor.publications, "Busy successor must not publish over its predecessor");
			assertEquals(0, successor.reminders());
			assertEquals(1, successor.warnings.size(), "Lease waiting should be reported once");
			assertTrue(successor.warnings.get(0).contains("waiting"));
			assertFalse(retired.isDone());
		};
		retiring.executor.runNext();
		assertTrue(retired.isDone());
		retired.join();
		assertEquals(new Snapshot(1000, -1), retiring.saved());
		assertEquals(0, retiring.reminders());
		successor.tickAt(2000);
		assertEquals(new Snapshot(2000, 2000), successor.saved());
		assertEquals(1, successor.reminders());
		assertEquals(1, successor.factories);
		successor.tickAt(2000);
		assertEquals(new Snapshot(2000, 2000), successor.saved());
		assertEquals(1, successor.reminders());
	}

	@Test
	void restartRetainsActiveRuntimeAndExcludesOfflinePeriodUsingRealStore() throws Exception {
		Harness first = new Harness(directory, 0, enabled());
		first.service.start();
		first.tickAt(600);
		first.clock.set(nanos(800));
		CompletableFuture<Void> closed = first.service.closeAsync();
		first.executor.runNext();
		closed.join();
		assertEquals(new Snapshot(800, -1), first.saved());
		long restartClock = nanos(TimeUnit.DAYS.toMillis(30));
		Harness restarted = new Harness(directory, restartClock, enabled());
		restarted.service.start();
		restarted.clock.set(restartClock + nanos(199));
		restarted.executor.firePeriodic();
		assertEquals(0, restarted.reminders());
		assertEquals(new Snapshot(999, -1), restarted.saved());
		restarted.clock.set(restartClock + nanos(200));
		restarted.executor.firePeriodic();
		assertEquals(1, restarted.reminders());
		assertEquals(new Snapshot(1000, 1000), restarted.saved());
	}

	@Test
	void reloadUsesNewThresholdAndRepeatWithoutResettingCommittedRuntime() throws Exception {
		Harness h = new Harness(directory, 0, new Settings(true, true, 2000, 1000));
		h.service.start();
		h.tickAt(1500);
		assertEquals(0, h.reminders());
		h.service.reload(new Settings(true, true, 1000, 1000));
		h.tickAt(1500);
		assertEquals(1, h.reminders());
		h.tickAt(2000);
		assertEquals(1, h.reminders());
		h.service.reload(new Settings(true, true, 1000, 500));
		h.tickAt(2000);
		assertEquals(2, h.reminders());
		assertEquals(new Snapshot(2000, 2000), h.saved());
	}

	private static Settings enabled() {
		return new Settings(true, true, 1000, 500);
	}

	private static long nanos(long millis) {
		return Math.multiplyExact(millis, 1_000_000);
	}

	private enum Failure { NONE, BEFORE_PUBLICATION, AFTER_PUBLICATION }

	private final class Harness {
		final Path directory;
		final AtomicLong clock;
		final ControlledExecutor executor = new ControlledExecutor();
		final List<String> warnings = new ArrayList<>();
		final List<String> events = new ArrayList<>();
		final List<Snapshot> snapshotsAtReminder = new ArrayList<>();
		final AutoCreateVoteSitesReminderService service;
		int factories;
		int publications;
		boolean factoryFailure;
		Failure failure = Failure.NONE;
		Runnable onPublication;
		Runnable onBeforePublication;
		Runnable onClockRead;

		Harness(Path directory, long initialNanos, Settings settings) {
			this.directory = directory;
			this.clock = new AtomicLong(initialNanos);
			harnesses.add(this);
			Logger logger = Logger.getAnonymousLogger();
			logger.setUseParentHandlers(false);
			logger.addHandler(new Handler() {
				@Override
				public void publish(LogRecord record) {
					String message = record.getMessage();
					warnings.add(message);
					if (isReminder(message)) {
						events.add("reminder");
						try {
							snapshotsAtReminder.add(saved());
						} catch (IOException failure) {
							throw new AssertionError("Reminder emitted before readable durable checkpoint", failure);
						}
					}
				}

				@Override public void flush() { }
				@Override public void close() { }
			});
			service = new AutoCreateVoteSitesReminderService(directory, executor, logger, settings, () -> {
				Runnable action = onClockRead;
				onClockRead = null;
				if (action != null) action.run();
				return clock.get();
			}, path -> {
				assertTrue(executor.running, "Store construction must run on the worker");
				factories++;
				if (factoryFailure) throw new IOException("Simulated store construction failure");
				events.add("store-created");
				return new AutoCreateVoteSitesReminderStore(path, (staged, target) -> {
					assertTrue(executor.running, "Store publication must run on the worker");
					publications++;
					Runnable before = onBeforePublication;
					onBeforePublication = null;
					if (before != null) before.run();
					if (failure == Failure.BEFORE_PUBLICATION) throw new IOException("Simulated pre-publication failure");
					DurableFiles.publishStagedFile(staged, target);
					events.add("persisted");
					Runnable action = onPublication;
					onPublication = null;
					if (action != null) action.run();
					if (failure == Failure.AFTER_PUBLICATION) {
						throw new PublishedException(new IOException("Simulated directory metadata failure"));
					}
				});
			});
		}

		void tickAt(long millis) {
			clock.set(nanos(millis));
			executor.firePeriodic();
		}

		long reminders() {
			return warnings.stream().filter(Harness::isReminder).count();
		}

		Snapshot saved() throws IOException {
			return new AutoCreateVoteSitesReminderStore(directory).load();
		}

		Path target() {
			return directory.resolve(".auto-create-sites-reminder.dat");
		}

		private static boolean isReminder(String message) {
			return message.contains("set AutoCreateVoteSites: false in Config.yml");
		}
	}

	private static final class ControlledExecutor extends AbstractExecutorService implements ScheduledExecutorService {
		final Queue<Runnable> queued = new ArrayDeque<>();
		final ControlledFuture periodicFuture = new ControlledFuture();
		Runnable periodic;
		boolean running;
		boolean shutdown;
		int schedules;
		int executions;
		long initialDelay;
		long delay;
		TimeUnit unit;

		@Override
		public ScheduledFuture<?> scheduleWithFixedDelay(Runnable command, long initialDelay, long delay, TimeUnit unit) {
			this.schedules++;
			this.periodic = command;
			this.initialDelay = initialDelay;
			this.delay = delay;
			this.unit = unit;
			return periodicFuture;
		}

		@Override
		public void execute(Runnable command) {
			executions++;
			queued.add(command);
		}

		void firePeriodic() {
			assertTrue(periodic != null, "Periodic work must be scheduled first");
			if (!periodicFuture.cancelled) run(periodic);
		}

		void runNext() {
			assertFalse(queued.isEmpty(), "Expected deferred work");
			run(queued.remove());
		}

		private void run(Runnable command) {
			assertFalse(running, "Fake worker must remain serialized");
			running = true;
			try {
				command.run();
			} finally {
				running = false;
			}
		}

		@Override public void shutdown() { shutdown = true; }
		@Override public List<Runnable> shutdownNow() { shutdown = true; return List.copyOf(queued); }
		@Override public boolean isShutdown() { return shutdown; }
		@Override public boolean isTerminated() { return shutdown && queued.isEmpty(); }
		@Override public boolean awaitTermination(long timeout, TimeUnit unit) { return isTerminated(); }
		@Override public ScheduledFuture<?> schedule(Runnable command, long delay, TimeUnit unit) { throw new UnsupportedOperationException(); }
		@Override public <V> ScheduledFuture<V> schedule(Callable<V> callable, long delay, TimeUnit unit) { throw new UnsupportedOperationException(); }
		@Override public ScheduledFuture<?> scheduleAtFixedRate(Runnable command, long initialDelay, long period, TimeUnit unit) { throw new UnsupportedOperationException(); }
	}

	private static final class ControlledFuture implements ScheduledFuture<Object> {
		boolean cancelled;
		@Override public boolean cancel(boolean mayInterruptIfRunning) { cancelled = true; return true; }
		@Override public boolean isCancelled() { return cancelled; }
		@Override public boolean isDone() { return cancelled; }
		@Override public Object get() { throw new UnsupportedOperationException(); }
		@Override public Object get(long timeout, TimeUnit unit) { throw new UnsupportedOperationException(); }
		@Override public long getDelay(TimeUnit unit) { return 0; }
		@Override public int compareTo(Delayed other) { return 0; }
	}
}
