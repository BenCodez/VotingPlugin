package com.bencodez.votingplugin.maintenance;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.maintenance.AutoCreateVoteSitesReminderService.Settings;

class AutoCreateVoteSitesReminderLifecycleTest {

	@Test
	void cleanupBeforeReminderInitializationDoesNotTouchVoteProcessingTimer() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		ScheduledExecutorService voteTimer = mock(ScheduledExecutorService.class);
		set(plugin, "voteTimer", voteTimer);
		assertDoesNotThrow(() -> stop(plugin));
		verifyNoInteractions(voteTimer);
	}

	@Test
	void partiallyCreatedWorkerIsClosedEvenWithoutAService() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
		set(plugin, "autoCreateVoteSitesReminderWorker", worker);
		stop(plugin);
		verify(worker).shutdown();
		assertNull(get(plugin, "autoCreateVoteSitesReminderWorker"));
	}

	@Test
	void finalCheckpointIsQueuedBeforeWorkerShutdownAndCleanupIsIdempotent() throws Exception {
		VotingPluginMain plugin = mock(VotingPluginMain.class, CALLS_REAL_METHODS);
		ScheduledExecutorService worker = mock(ScheduledExecutorService.class);
		ScheduledFuture<?> task = mock(ScheduledFuture.class);
		when(worker.scheduleWithFixedDelay(any(Runnable.class), anyLong(), anyLong(), eq(TimeUnit.SECONDS)))
				.thenAnswer(invocation -> task);
		AutoCreateVoteSitesReminderService service = new AutoCreateVoteSitesReminderService(Path.of("unused"),
				worker, Logger.getAnonymousLogger(), new Settings(true, true, 1000, 1000));
		service.start();
		set(plugin, "autoCreateVoteSitesSetupReminder", service);
		set(plugin, "autoCreateVoteSitesReminderWorker", worker);
		stop(plugin);
		stop(plugin);
		InOrder order = inOrder(task, worker);
		order.verify(task).cancel(false);
		order.verify(worker).execute(any(Runnable.class));
		order.verify(worker).shutdown();
		verify(worker, times(1)).shutdown();
		assertNull(get(plugin, "autoCreateVoteSitesSetupReminder"));
		assertNull(get(plugin, "autoCreateVoteSitesReminderWorker"));
	}

	private static void stop(VotingPluginMain plugin) throws Exception {
		Method method = VotingPluginMain.class.getDeclaredMethod("stopAutoCreateVoteSitesSetupReminder");
		method.setAccessible(true);
		method.invoke(plugin);
	}

	private static void set(VotingPluginMain plugin, String name, Object value) throws Exception {
		Field field = VotingPluginMain.class.getDeclaredField(name);
		field.setAccessible(true);
		field.set(plugin, value);
	}

	private static Object get(VotingPluginMain plugin, String name) throws Exception {
		Field field = VotingPluginMain.class.getDeclaredField(name);
		field.setAccessible(true);
		return field.get(plugin);
	}
}
