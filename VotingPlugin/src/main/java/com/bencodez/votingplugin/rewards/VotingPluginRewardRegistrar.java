package com.bencodez.votingplugin.rewards;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.builtin.RewardPoints;
import com.bencodez.votingplugin.rewards.builtin.RewardVoteBossBar;
import com.bencodez.votingplugin.rewards.builtin.RewardWebhook;
import com.bencodez.votingplugin.rewards.builtin.placeholders.RewardTotalPlaceholder;
import com.bencodez.votingplugin.rewards.builtin.requirements.RequirementVoteTotal;
import com.bencodez.votingplugin.topvoter.TopVoter;

/** Registers VotingPlugin-specific AdvancedCore reward extensions. */
public final class VotingPluginRewardRegistrar {

	private VotingPluginRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		plugin.getRewardHandler().addInjectedReward(new RewardPoints(plugin));
		plugin.getRewardHandler().addInjectedReward(new RewardVoteBossBar(plugin));
		plugin.getRewardHandler().addInjectedReward(new RewardWebhook(plugin));
		plugin.getRewardHandler().addInjectedRequirements(new RequirementVoteTotal(plugin));

		for (TopVoter topVoter : TopVoter.values()) {
			plugin.getRewardHandler().addPlaceholder(new RewardTotalPlaceholder(plugin, topVoter));
		}
	}
}
