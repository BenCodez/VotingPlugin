package com.bencodez.votingplugin.votereminding;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.rewards.DirectlyDefinedRewardPath;

/** Registers directly-defined vote reminder reward paths. */
public final class VoteReminderRewardRegistrar {

	private VoteReminderRewardRegistrar() {
	}

	public static void register(VotingPluginMain plugin) {
		plugin.addDirectlyDefinedRewards(new DirectlyDefinedReward("VoteReminderOptions.Defaults.Rewards") {
			@Override public void createSection(String key) { plugin.getConfigFile().saveData(); }
			@Override public ConfigurationSection getFileData() { return plugin.getConfigFile().getData(); }
			@Override public void save() { plugin.getConfigFile().saveData(); }
			@Override public void setData(String path, Object value) { plugin.getConfigFile().setValue(path, value); }
		});

		ConfigurationSection reminders = plugin.getConfigFile().getData().getConfigurationSection("VoteReminders");
		if (reminders != null) {
			for (String key : reminders.getKeys(false)) {
				plugin.addDirectlyDefinedRewards(DirectlyDefinedRewardPath.of("VoteReminders." + key + ".Rewards",
						plugin.getConfigFile()));
			}
		}
	}
}
