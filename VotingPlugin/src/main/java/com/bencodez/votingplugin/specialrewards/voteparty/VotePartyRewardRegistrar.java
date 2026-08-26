package com.bencodez.votingplugin.specialrewards.voteparty;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined vote-party reward paths. */
public final class VotePartyRewardRegistrar {

	private VotePartyRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("VoteParty.Rewards",
				plugin.getSpecialRewardsConfig()));
	}
}
