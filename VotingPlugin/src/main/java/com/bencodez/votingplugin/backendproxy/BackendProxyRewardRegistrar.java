package com.bencodez.votingplugin.backendproxy;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined backend/proxy reward paths. */
public final class BackendProxyRewardRegistrar {

	private BackendProxyRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("BungeeVotePartyRewards",
				plugin.getBungeeSettings()));
	}
}
