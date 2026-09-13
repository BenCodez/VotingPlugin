package com.bencodez.votingplugin.util;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;

import org.bukkit.entity.Player;

import com.bencodez.simpleapi.folialib.enums.EntityTaskResult;
import com.bencodez.votingplugin.VotingPluginMain;

/** Schedules exactly-once completion work with an entity-retirement fallback. */
public final class BukkitCompletionScheduler {
	private BukkitCompletionScheduler() {
	}

	public static void run(VotingPluginMain plugin, Player player, Runnable task) {
		AtomicBoolean executed = new AtomicBoolean();
		Runnable once = () -> {
			if (executed.compareAndSet(false, true)) task.run();
		};
		if (player == null) {
			runGlobal(plugin, once);
			return;
		}
		AtomicBoolean fallbackSubmitted = new AtomicBoolean();
		Runnable fallback = () -> {
			if (fallbackSubmitted.compareAndSet(false, true)) runGlobal(plugin, once);
		};
		try {
			if (plugin.getBukkitScheduler().getFoliaLib() == null) {
				runLegacyEntity(plugin, player, once, fallback);
				return;
			}
			CompletableFuture<EntityTaskResult> result = plugin.getBukkitScheduler().getFoliaLib().getImpl()
					.runAtEntityWithFallback(player, ignored -> once.run(), fallback);
			result.whenComplete((status, failure) -> {
				// ENTITY_RETIRED invokes fallback itself. A scheduler that was already
				// retired returns SCHEDULER_RETIRED without invoking it.
				if (failure != null || status != EntityTaskResult.SUCCESS) {
					fallback.run();
				} else if (!executed.get()) {
					// Compatibility with scheduler adapters that report admission but do
					// not run the consumer inline with future completion.
					runLegacyEntity(plugin, player, once, fallback);
				}
			});
		} catch (RuntimeException schedulingFailure) {
			plugin.debug(schedulingFailure);
			fallback.run();
		}
	}

	private static void runLegacyEntity(VotingPluginMain plugin, Player player, Runnable task, Runnable fallback) {
		try {
			plugin.getBukkitScheduler().runTask(plugin, task, player);
		} catch (RuntimeException legacyFailure) {
			plugin.debug(legacyFailure);
			fallback.run();
		}
	}

	private static void runGlobal(VotingPluginMain plugin, Runnable task) {
		try {
			plugin.getBukkitScheduler().runTask(plugin, task);
		} catch (RuntimeException schedulingFailure) {
			plugin.debug(schedulingFailure);
		}
	}
}
