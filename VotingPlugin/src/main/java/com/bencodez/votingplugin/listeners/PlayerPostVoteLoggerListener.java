package com.bencodez.votingplugin.listeners;

import java.util.UUID;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerPostVoteEvent;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogStatus;

public class PlayerPostVoteLoggerListener implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerPostVoteLoggerListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onplayerVote(PlayerPostVoteEvent event) {
		if (!plugin.isEnabled()) {
			plugin.getLogger().warning("Plugin disabled, ignoring vote");
			return;
		}
		VoteLogStatus status = VoteLogStatus.IMMEDIATE;
		if (event.isCached()) {
			status = VoteLogStatus.CACHED;
		}
		if (plugin.getConfigFile().isVoteLoggingEnabled()) {
			plugin.getVoteLogMysqlTable().logVote(UUID.randomUUID(), status, event.getService(),
					event.getUuid().toString(), event.getPlayerName(), event.getVoteTime(),
					event.getUser().getOfflineVotes().size());
		}
	}
}
