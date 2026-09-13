package com.bencodez.votingplugin.voteshop;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doThrow;
import org.mockito.ArgumentCaptor;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.VotingPluginMain;

class VoteShopManagerTest {
	@Test
	void schedulesStartupAndBoundedPeriodicSharedPurchaseRecovery() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(timer);

		VoteShopManager.scheduleSharedPurchaseRecovery(plugin);

		verify(timer).execute(any(Runnable.class));
		verify(timer).scheduleWithFixedDelay(any(Runnable.class), anyLong(), anyLong(),
				org.mockito.ArgumentMatchers.eq(TimeUnit.MINUTES));
	}

	@Test
	void periodicRecoveryContainsRuntimeFailures() {
		VotingPluginMain plugin = mock(VotingPluginMain.class, org.mockito.Mockito.RETURNS_DEEP_STUBS);
		ScheduledExecutorService timer = mock(ScheduledExecutorService.class);
		when(plugin.getTimer()).thenReturn(timer);
		doThrow(new IllegalStateException("storage unavailable")).when(plugin).getStorageType();

		VoteShopManager.scheduleSharedPurchaseRecovery(plugin);
		ArgumentCaptor<Runnable> scheduled = ArgumentCaptor.forClass(Runnable.class);
		verify(timer).scheduleWithFixedDelay(scheduled.capture(), anyLong(), anyLong(),
				org.mockito.ArgumentMatchers.eq(TimeUnit.MINUTES));
		org.junit.jupiter.api.Assertions.assertDoesNotThrow(scheduled.getValue()::run);
		verify(plugin.getLogger()).severe(
				"Unable to recover shared MySQL vote shop purchases: IllegalStateException");
	}
}
