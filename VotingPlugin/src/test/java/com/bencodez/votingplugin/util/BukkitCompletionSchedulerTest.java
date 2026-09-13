package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;

import org.bukkit.entity.Player;
import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.folialib.FoliaLib;
import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.simpleapi.folialib.impl.ServerImplementation;
import com.bencodez.simpleapi.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;

class BukkitCompletionSchedulerTest {
	@Test
	void retiredEntityRunsCompletionOnceOnGlobalFallback() {
		Fixture fixture = fixture();
		when(fixture.entityScheduler.runAtEntityWithFallback(eq(fixture.player), any(), any(Runnable.class)))
				.thenAnswer(invocation -> {
					invocation.getArgument(2, Runnable.class).run();
					return CompletableFuture.completedFuture(EntityTaskResult.ENTITY_RETIRED);
				});
		AtomicInteger completions = new AtomicInteger();

		BukkitCompletionScheduler.run(fixture.plugin, fixture.player, completions::incrementAndGet);

		assertEquals(1, completions.get());
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class));
	}

	@Test
	void alreadyRetiredSchedulerRunsCompletionOnceOnGlobalFallback() {
		Fixture fixture = fixture();
		when(fixture.entityScheduler.runAtEntityWithFallback(eq(fixture.player), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(EntityTaskResult.SCHEDULER_RETIRED));
		AtomicInteger completions = new AtomicInteger();

		BukkitCompletionScheduler.run(fixture.plugin, fixture.player, completions::incrementAndGet);

		assertEquals(1, completions.get());
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class));
	}

	@Test
	void missingSchedulerStatusRunsCompletionOnceOnGlobalFallback() {
		Fixture fixture = fixture();
		when(fixture.entityScheduler.runAtEntityWithFallback(eq(fixture.player), any(), any(Runnable.class)))
				.thenReturn(CompletableFuture.completedFuture(null));
		AtomicInteger completions = new AtomicInteger();

		BukkitCompletionScheduler.run(fixture.plugin, fixture.player, completions::incrementAndGet);

		assertEquals(1, completions.get());
		verify(fixture.scheduler).runTask(eq(fixture.plugin), any(Runnable.class));
	}

	private static Fixture fixture() {
		VotingPluginMain plugin = mock(VotingPluginMain.class);
		BukkitScheduler scheduler = mock(BukkitScheduler.class);
		FoliaLib folia = mock(FoliaLib.class);
		ServerImplementation entityScheduler = mock(ServerImplementation.class);
		Player player = mock(Player.class);
		when(plugin.getBukkitScheduler()).thenReturn(scheduler);
		when(scheduler.getFoliaLib()).thenReturn(folia);
		when(folia.getImpl()).thenReturn(entityScheduler);
		doAnswer(invocation -> {
			invocation.getArgument(1, Runnable.class).run();
			return null;
		}).when(scheduler).runTask(eq(plugin), any(Runnable.class));
		return new Fixture(plugin, scheduler, entityScheduler, player);
	}

	private record Fixture(VotingPluginMain plugin, BukkitScheduler scheduler,
			ServerImplementation entityScheduler, Player player) {
	}
}
