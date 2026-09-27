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
		BukkitCompletionScheduler.run(plugin, target,
				() -> plugin.getServer().getPluginManager().callEvent(event));
	}
}
