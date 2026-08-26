package com.bencodez.votingplugin.config;

import com.bencodez.votingplugin.VotingPluginMain;

/** Owns aggregate YAML load-health reporting for VotingPlugin configuration files. */
public final class VotingPluginConfigHealth {

	private final VotingPluginMain plugin;
	private boolean ymlError;

	public VotingPluginConfigHealth(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void check() {
		ymlError = plugin.getConfigFile().isFailedToRead() || plugin.getConfigVoteSites().isFailedToRead()
				|| plugin.getSpecialRewardsConfig().isFailedToRead() || plugin.getBungeeSettings().isFailedToRead()
				|| plugin.getGui().isFailedToRead();

		if (ymlError) {
			plugin.getBukkitScheduler().runTaskLaterAsynchronously(plugin,
					() -> plugin.getLogger().severe("Failed to load a file, check startup log"), 1);
		}
	}

	public boolean hasYmlError() {
		return ymlError;
	}
}
