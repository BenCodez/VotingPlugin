package com.bencodez.votingplugin.votesites;

import java.util.ArrayList;

import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Resolves vote-site identifiers without mutating configuration.
 */
public class VoteSiteResolver {

	private final VotingPluginMain plugin;
	private final VoteSiteRegistry registry;
	private final VoteSiteValidator validator;

	public VoteSiteResolver(VotingPluginMain plugin, VoteSiteRegistry registry, VoteSiteValidator validator) {
		this.plugin = plugin;
		this.registry = registry;
		this.validator = validator;
	}

	public String getConfiguredVoteSiteName(String... identifiers) {
		if (identifiers == null) {
			return null;
		}

		ArrayList<String> configuredSites = plugin.getConfigVoteSites().getRawVoteSiteNames();
		if (configuredSites == null || configuredSites.isEmpty()) {
			return null;
		}

		for (String identifier : identifiers) {
			if (identifier == null || identifier.isEmpty()) {
				continue;
			}

			String normalizedIdentifier = validator.normalizeVoteSiteKey(identifier);
			for (String siteName : configuredSites) {
				if (siteName == null) {
					continue;
				}

				String serviceSite = plugin.getConfigVoteSites().getServiceSite(siteName);
				String displayName = plugin.getConfigVoteSites().getDisplayName(siteName);
				if (siteName.equalsIgnoreCase(identifier) || siteName.equalsIgnoreCase(normalizedIdentifier)
						|| (serviceSite != null && !serviceSite.isEmpty() && serviceSite.equalsIgnoreCase(identifier))
						|| (displayName != null && !displayName.isEmpty() && displayName.equalsIgnoreCase(identifier))) {
					return siteName;
				}
			}
		}

		return null;
	}

	public boolean hasConfiguredVoteSite(String... identifiers) {
		return getConfiguredVoteSiteName(identifiers) != null;
	}

	public String getVoteSiteName(boolean checkEnabled, String... identifiers) {
		if (identifiers == null) {
			return null;
		}

		for (String identifier : identifiers) {
			if (identifier == null) {
				return null;
			}

			if (!identifier.isEmpty()) {
				for (VoteSite site : registry.getVoteSites()) {
					if (checkEnabled && !site.isEnabled()) {
						continue;
					}

					String serviceSite = site.getServiceSite();
					if (serviceSite != null && serviceSite.equalsIgnoreCase(identifier)) {
						return site.getKey();
					}
					if (site.getKey().equalsIgnoreCase(identifier)) {
						return site.getKey();
					}
					String displayName = site.getDisplayName();
					if (displayName != null && displayName.equalsIgnoreCase(identifier)) {
						return site.getKey();
					}
				}
			}
		}

		if (!checkEnabled) {
			String configuredSiteName = getConfiguredVoteSiteName(identifiers);
			if (configuredSiteName != null) {
				return configuredSiteName;
			}
		}

		for (String identifier : identifiers) {
			return identifier;
		}

		return "";
	}

	public VoteSite resolveVoteSite(String identifier, boolean checkEnabled) {
		String siteName = getVoteSiteName(checkEnabled, identifier);
		if (siteName == null) {
			return null;
		}

		for (VoteSite voteSite : registry.getVoteSites()) {
			if (checkEnabled && !voteSite.isEnabled()) {
				continue;
			}

			if (voteSite.getKey().equalsIgnoreCase(siteName)) {
				return voteSite;
			}

			String displayName = voteSite.getDisplayName();
			if (displayName != null && displayName.equalsIgnoreCase(siteName)) {
				return voteSite;
			}
		}
		return null;
	}

	public String getVoteSiteServiceSite(String name) {
		if (name == null) {
			return null;
		}

		for (VoteSite site : registry.getVoteSites()) {
			if (!site.isEnabled()) {
				continue;
			}

			String serviceSite = site.getServiceSite();
			if (serviceSite != null
					&& (serviceSite.equalsIgnoreCase(name) || name.equalsIgnoreCase(site.getKey()))) {
				return serviceSite;
			}
		}
		return name;
	}

	public boolean hasVoteSite(String site) {
		return resolveVoteSite(site, false) != null;
	}

	public boolean isVoteSite(String voteSite) {
		if (voteSite == null) {
			return false;
		}
		for (VoteSite site : registry.getVoteSites()) {
			if (site.getKey().equalsIgnoreCase(voteSite)) {
				return true;
			}
		}
		return false;
	}
}
