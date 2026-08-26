package com.bencodez.votingplugin.specialrewards.votestreak;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined vote-streak reward paths. */
public final class VoteStreakRewardRegistrar {

	private VoteStreakRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		registerCurrent(plugin);
		registerLegacy(plugin);
	}

	private static void registerCurrent(VotingPluginMain plugin) {
		ConfigurationSection streaks = plugin.getSpecialRewardsConfig().getData().getConfigurationSection("VoteStreaks");
		if (streaks == null) {
			return;
		}

		for (String streakId : streaks.getKeys(false)) {
			if ("ProgressGroups".equalsIgnoreCase(streakId) || streaks.getConfigurationSection(streakId) == null) {
				continue;
			}
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					"VoteStreaks." + streakId + ".Rewards", plugin.getSpecialRewardsConfig()));
		}

		ConfigurationSection groups = streaks.getConfigurationSection("ProgressGroups");
		if (groups == null) {
			return;
		}
		for (String groupId : groups.getKeys(false)) {
			ConfigurationSection group = groups.getConfigurationSection(groupId);
			if (group == null) {
				continue;
			}
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					"VoteStreaks.ProgressGroups." + groupId + ".LostRewards", plugin.getSpecialRewardsConfig()));
			ConfigurationSection milestones = group.getConfigurationSection("Milestones");
			if (milestones == null) {
				continue;
			}
			for (String milestoneId : milestones.getKeys(false)) {
				if (milestones.getConfigurationSection(milestoneId) == null) {
					continue;
				}
				plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
						"VoteStreaks.ProgressGroups." + groupId + ".Milestones." + milestoneId + ".Rewards",
						plugin.getSpecialRewardsConfig()));
			}
		}
	}

	private static void registerLegacy(VotingPluginMain plugin) {
		for (String type : new String[] { "Day", "Week", "Month" }) {
			for (String value : plugin.getSpecialRewardsConfig().getVoteStreakVotes(type)) {
				plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
						"VoteStreak." + type + "." + value + ".Rewards", plugin.getSpecialRewardsConfig()));
			}
		}
	}
}
