package com.bencodez.votingplugin.votelog.listeners;

import java.time.LocalDateTime;
import java.time.ZoneId;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;

public class PlayerSpecialRewardLoggerListener implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new votifer event.
	 *
	 * @param plugin the plugin
	 */
	public PlayerSpecialRewardLoggerListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * On votifer event.
	 *
	 * @param event the event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onplayerVote(PlayerSpecialRewardEvent event) {
		if (!plugin.isEnabled()) {
			return;
		}

		if (plugin.getConfigFile().isVoteLoggingEnabled()) {
			switch (event.getType()) {
			case ALLSITE:
				plugin.getVoteLogMysqlTable().logAllSitesReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			case ALMOSTALLSITES:
				plugin.getVoteLogMysqlTable().logAlmostAllSitesReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			case CUMMULATIVE:
				plugin.getVoteLogMysqlTable().logCumulativeReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			case FIRSTVOTE:
				break;
			case FIRSTVOTETODAY:
				break;
			case MILESTONE:
				plugin.getVoteLogMysqlTable().logMilestoneReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			case VOTESTREAK:
				plugin.getVoteLogMysqlTable().logVoteStreakReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			case TOPVOTER:
				plugin.getVoteLogMysqlTable().logTopVoterReward(event.getVoteUUID(), event.getUser().getUUID(),
						event.getUser().getPlayerName(),
						LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli(),
						event.getType().toString());
				break;
			default:
				break;

			}

		}
	}
}
