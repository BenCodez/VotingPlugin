package com.bencodez.votingplugin.voteshop;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
}
