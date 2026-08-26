package com.bencodez.votingplugin.rewards;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.rewards.DirectlyDefinedReward;
import com.bencodez.simpleapi.file.YMLFile;

/**
 * Creates directly-defined reward editors backed by a VotingPlugin YAML file.
 */
public final class DirectlyDefinedRewardPath {

	private DirectlyDefinedRewardPath() {
	}

	public static DirectlyDefinedReward of(String path, YMLFile file) {
		return new DirectlyDefinedReward(path) {
			@Override
			public void createSection(String key) {
				file.createSection(key);
			}

			@Override
			public ConfigurationSection getFileData() {
				return file.getData();
			}

			@Override
			public void save() {
				file.saveData();
			}

			@Override
			public void setData(String dataPath, Object value) {
				file.setValue(dataPath, value);
			}
		};
	}
}
