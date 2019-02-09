package com.Ben12345rocks.VotingPlugin;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.Injected.RewardInject;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class VotingPluginHooks {
	private static VotingPluginHooks instance = new VotingPluginHooks();

	public static VotingPluginHooks getInstance() {
		return instance;
	}

	public Main getMainClass() {
		return Main.plugin;
	}

	public void addCustomReward(RewardInject inject) {
		RewardHandler.getInstance().getInjectedRewards().add(inject);
	}

	public UserManager getUserManager() {
		return UserManager.getInstance();
	}

}
