package com.bencodez.votingplugin.util;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;

/** Dispatches synchronous vote events from the voted player's Bukkit/Folia owner. */
public final class BukkitVoteEventDispatcher {
	private BukkitVoteEventDispatcher() {
	}

	public static void dispatch(VotingPluginMain plugin, PlayerVoteEvent event) {
		Player target = Bukkit.getPlayerExact(event.getPlayer());
		Runnable rejected = () -> fail(event);
		BukkitCompletionScheduler.run(plugin, target, () -> {
			try {
				plugin.getServer().getPluginManager().callEvent(event);
			} catch (RuntimeException | Error dispatchFailure) {
				plugin.debug(dispatchFailure);
				fail(event);
				throw dispatchFailure;
			}
		}, rejected);
	}

	private static void fail(PlayerVoteEvent event) {
		event.setProcessingFailed(true);
		event.completeProcessing();
	}
}
