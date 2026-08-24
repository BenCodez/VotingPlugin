package com.bencodez.votingplugin.votesites;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined vote-site reward paths. */
public final class VoteSiteRewardRegistrar {

	private VoteSiteRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("AnySiteRewards", plugin.getSpecialRewardsConfig()));
		plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("EverySiteReward", plugin.getConfigVoteSites()));

		for (VoteSite site : plugin.getVoteSiteManager().getVoteSites()) {
			String base = "VoteSites." + site.getKey();
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(base + ".Rewards", plugin.getConfigVoteSites()));
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(base + ".WaitUntilVoteDelayRewards",
					plugin.getConfigVoteSites()));
			plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of(base + ".CoolDownEndRewards",
					plugin.getConfigVoteSites()));
		}
	}
}
