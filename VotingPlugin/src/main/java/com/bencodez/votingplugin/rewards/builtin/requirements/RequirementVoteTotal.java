package com.bencodez.votingplugin.rewards.builtin.requirements;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.rewards.injectedrequirement.RequirementInjectConfigurationSection;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** VotingPlugin vote-total/points reward requirement. */
public class RequirementVoteTotal extends RequirementInjectConfigurationSection {

	private final VotingPluginMain plugin;

	public RequirementVoteTotal(VotingPluginMain plugin) {
		super("VoteTotal");
		this.plugin = plugin;
	}

	@Override
	public boolean onRequirementsRequested(Reward reward, AdvancedCoreUser acUser, ConfigurationSection section,
			RewardOptions rewardOptions) {
		boolean atleast = section.getBoolean("AtleastMode", false);
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(acUser);

		for (TopVoter top : TopVoter.values()) {
			int required = section.getInt(top.toString(), -1);
			if (required < 0) {
				continue;
			}
			int total = user.getTotal(top);
			if (atleast ? total < required : total != required) {
				plugin.debug("Failed requirement " + top + " " + total + (atleast ? "/" : "!=") + required);
				return false;
			}
		}

		int pointsRequired = section.getInt("Points", -1);
		if (pointsRequired >= 0) {
			int points = user.getPoints();
			if (atleast ? points < pointsRequired : points != pointsRequired) {
				plugin.debug("Failed requirement points " + points + (atleast ? "/" : "!=") + pointsRequired);
				return false;
			}
		}
		return true;
	}
}
