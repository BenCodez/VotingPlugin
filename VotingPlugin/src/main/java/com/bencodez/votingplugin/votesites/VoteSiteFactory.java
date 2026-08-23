package com.bencodez.votingplugin.votesites;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

/**
 * Handles vote-site creation and config generation.
 */
public class VoteSiteFactory {

	private final VotingPluginMain plugin;
	private final VoteSiteResolver resolver;
	private final VoteSiteValidator validator;

	public VoteSiteFactory(VotingPluginMain plugin, VoteSiteResolver resolver, VoteSiteValidator validator) {
		this.plugin = plugin;
		this.resolver = resolver;
		this.validator = validator;
	}

	public VoteSite createIfAllowed(String siteName) {
		if (!plugin.getConfigFile().isAutoCreateVoteSites() || resolver.hasVoteSite(siteName)
				|| resolver.hasConfiguredVoteSite(siteName)) {
			return null;
		}

		if (!ServiceSiteValidator.isValid(siteName)) {
			plugin.getLogger().warning("Unable to auto-create vote site with unsupported name '"
					+ ServiceSiteValidator.sanitizeForLog(siteName) + "'");
			return null;
		}

		if (!plugin.getConfigVoteSites().tryGenerateVoteSite(siteName)) {
			return null;
		}

		return new VoteSite(plugin, validator.normalizeVoteSiteKey(siteName));
	}
}
