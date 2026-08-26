package com.bencodez.votingplugin.specialrewards;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined general and legacy special reward paths. */
public final class SpecialRewardsRewardRegistrar {

	private SpecialRewardsRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		String[] simplePaths = {
				"VoteCoolDownEndedReward", "LoginRewards", "LogoutRewards",
				"AllSites", "AlmostAllSites", "FirstVote", "FirstVoteToday", "NameMCLikeReward.Rewards"
		};
		for (String path : simplePaths) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(path, plugin.getSpecialRewardsConfig()));
		}

		registerLegacyGroup(plugin, "Cumulative");
		registerLegacyGroup(plugin, "Milestones");
	}

	private static void registerLegacyGroup(VotingPluginMain plugin, String key) {
		ConfigurationSection section = plugin.getSpecialRewardsConfig().getData().getConfigurationSection(key);
		if (section == null) {
			return;
		}
		for (String entry : section.getKeys(false)) {
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(key + "." + entry + ".Rewards",
					plugin.getSpecialRewardsConfig()));
		}
	}
}
