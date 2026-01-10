package com.bencodez.votingplugin.votelog.listeners;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VoteMilestoneRewardEvent;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;

/**
 * Logs VoteMilestone reward executions to the VoteLog table.
 */
public class VoteMilestoneVoteLogListener implements Listener {

	private final VotingPluginMain plugin;

	public VoteMilestoneVoteLogListener(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	@EventHandler
	public void onVoteMilestoneReward(VoteMilestoneRewardEvent e) {
		if (e == null || e.getUser() == null || e.getMilestone() == null) {
			return;
		}
		if (plugin == null || !plugin.isEnabled()) {
			return;
		}
		if (plugin.getVoteLogMysqlTable() == null) {
			return;
		}

		VoteMilestone m = e.getMilestone();

		String milestoneId = safe(e.getMilestoneId());
		String groupId = safe(e.getGroupId());
		String totalType = m.getTotal() == null ? "null" : m.getTotal().name();
		long value = e.getValue();

		long timeMillis = System.currentTimeMillis();

		try {
			plugin.getVoteLogMysqlTable().logVoteMilestoneReward(e.getVoteUUID(),
					e.getUuid() == null ? null : e.getUuid().toString(), e.getPlayerName(), timeMillis, milestoneId,
					groupId, totalType, value);
		} catch (Exception ex) {
			plugin.debug("[VoteLog] Failed to log VoteMilestone reward: " + ex.getClass().getSimpleName() + ":"
					+ (ex.getMessage() == null ? "" : ex.getMessage()));
		}
	}

	private static String safe(String s) {
		return s == null ? "" : s.replace('\n', ' ').replace('\r', ' ').trim();
	}
}
