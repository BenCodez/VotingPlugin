package com.bencodez.votingplugin.integration.votifier;

import com.bencodez.votingplugin.VotingPluginMain;

/** Owns Votifier/NuVotifier availability detection. */
public final class VotifierIntegration {

	private final VotingPluginMain plugin;
	private boolean loaded = true;

	public VotifierIntegration(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void detect() {
		loaded = true;
		try {
			Class.forName("com.vexsoftware.votifier.model.VotifierEvent");
		} catch (ClassNotFoundException e) {
			if (!plugin.getBungeeSettings().isUseBungeecoord()) {
				plugin.getLogger().warning("No VotifierEvent found, install Votifier, NuVotifier, or another Votifier plugin");
			} else {
				plugin.debug("No VotifierEvent found, but usebungeecoord enabled");
			}
			loaded = false;
		}
	}

	public boolean isLoaded() {
		return loaded;
	}
}
