package com.bencodez.votingplugin.votesites;

import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Handles vote-site key validation and normalization.
 */
public class VoteSiteValidator {

	private final VotingPluginMain plugin;

	public VoteSiteValidator(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void validateVoteSiteName(String siteName) {
		if (siteName == null) {
			return;
		}

		if (siteName.equalsIgnoreCase("null")) {
			plugin.getLogger().warning("Vote site name 'null' is not valid");
			return;
		}

		if (siteName.contains(" ")) {
			plugin.getLogger().warning("Vote site " + siteName + " contains spaces, this may cause issues");
		}
	}

	public String normalizeVoteSiteKey(String name) {
		if (name == null) {
			return null;
		}
		return name.replaceAll("[\\.\\s]+", "_");
	}
}
