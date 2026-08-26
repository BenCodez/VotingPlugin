package com.bencodez.votingplugin.specialrewards.votemilestones;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined VoteMilestones reward paths. */
public final class VoteMilestoneRewardRegistrar {

	private VoteMilestoneRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		ConfigurationSection section = plugin.getSpecialRewardsConfig().getData()
				.getConfigurationSection("VoteMilestones");
		if (section == null) {
			return;
		}
		for (String milestoneId : section.getKeys(false)) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(
					"VoteMilestones." + milestoneId + ".Rewards", plugin.getSpecialRewardsConfig()));
		}
	}
}
