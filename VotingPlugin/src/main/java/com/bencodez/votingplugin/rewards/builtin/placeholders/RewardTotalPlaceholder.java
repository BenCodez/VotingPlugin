package com.bencodez.votingplugin.rewards.builtin.placeholders;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardPlaceholderHandle;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;

/** Reward placeholder exposing one VotingPlugin total. */
public class RewardTotalPlaceholder extends RewardPlaceholderHandle {

	private final VotingPluginMain plugin;
	private final TopVoter topVoter;

	public RewardTotalPlaceholder(VotingPluginMain plugin, TopVoter topVoter) {
		super("Total_" + topVoter.toString());
		this.plugin = plugin;
		this.topVoter = topVoter;
	}

	@Override
	public String getValue(Reward reward, AdvancedCoreUser user) {
		return "" + plugin.getVotingPluginUserManager().getVotingPluginUser(user).getTotal(topVoter);
	}
}
