package com.bencodez.votingplugin;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.injected.RewardInject;
import com.bencodez.advancedcore.api.rewards.injectedrequirement.RequirementInject;
import com.bencodez.votingplugin.user.UserManager;

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

	public VotingPluginMain getMainClass() {
		return VotingPluginMain.plugin;
	}

	public UserManager getUserManager() {
		return UserManager.getInstance();
	}

}
