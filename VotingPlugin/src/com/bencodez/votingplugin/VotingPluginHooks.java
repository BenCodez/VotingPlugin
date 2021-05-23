package com.bencodez.votingplugin;

import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.injected.RewardInject;
import com.bencodez.advancedcore.api.rewards.injectedrequirement.RequirementInject;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VotingPluginHooks {
	private static VotingPluginHooks instance = new VotingPluginHooks();

	public static VotingPluginHooks getInstance() {
		return instance;
	}

	public void addCustomRequirement(RequirementInject inject) {
		RewardHandler.getInstance().addInjectedRequirements(inject);
	}

	public void addCustomReward(RewardInject inject) {
		RewardHandler.getInstance().addInjectedReward(inject);
	}

	public void backgroundUpdate(Player player) {
		VotingPluginUser user = getUserManager().getVotingPluginUser(player);
		user.offVote();
		user.checkOfflineRewards();
	}

	public VotingPluginMain getMainClass() {
		return VotingPluginMain.plugin;
	}

	public UserManager getUserManager() {
		return UserManager.getInstance();
	}

}
