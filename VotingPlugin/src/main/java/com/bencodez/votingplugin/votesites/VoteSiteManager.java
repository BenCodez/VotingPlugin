package com.bencodez.votingplugin.votesites;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

import lombok.Getter;
import lombok.Setter;

public class VoteSiteManager {

	@Getter
	@Setter
	private VotingPluginMain plugin;

	@Getter
	@Setter
	private List<VoteSite> voteSites = Collections.synchronizedList(new ArrayList<VoteSite>());

	public VoteSiteManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Reloads votesites from config and returns the backing list.
	 *
	 * @return the loaded vote sites
	 */
	public List<VoteSite> loadVoteSites() {
		plugin.getConfigVoteSites().setup();

		List<VoteSite> newSites = Collections.synchronizedList(new ArrayList<VoteSite>());
		newSites.addAll(plugin.getConfigVoteSites().getVoteSitesLoad());

		for (VoteSite site : newSites) {
			validateVoteSiteName(site.getKey());
		}

		voteSites = newSites;

		if (voteSites.isEmpty()) {
			plugin.getLogger().warning("Detected no voting sites, this may mean something isn't properly setup");
		}

		plugin.debug("Loaded VoteSites");
		return voteSites;
	}

	/**
	 * Validates a vote site name for common problems.
	 *
	 * @param siteName the site name
	 */
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

	/**
	 * Normalizes a vote site key for generated or matched site names.
	 *
	 * @param name the input name
	 * @return the normalized vote site key
	 */
	public String normalizeVoteSiteKey(String name) {
		if (name == null) {
			return null;
		}
		return name.replaceAll("[\\.\\s]+", "_");
	}

	/**
	 * Resolves identifiers against every configured vote-site section, including
	 * disabled sites.
	 *
	 * @param identifiers vote-site keys, service sites, or display names
	 * @return the configured vote-site key, or null when no configuration matches
	 */
	private String getConfiguredVoteSiteName(String... identifiers) {
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

			String normalizedIdentifier = normalizeVoteSiteKey(identifier);
			for (String siteName : configuredSites) {
				if (siteName == null) {
					continue;
				}

				String serviceSite = plugin.getConfigVoteSites().getServiceSite(siteName);
				String displayName = plugin.getConfigVoteSites().getDisplayName(siteName);
				if (siteName.equalsIgnoreCase(identifier) || siteName.equalsIgnoreCase(normalizedIdentifier)
						|| (serviceSite != null && !serviceSite.isEmpty()
								&& serviceSite.equalsIgnoreCase(identifier))
						|| (displayName != null && !displayName.isEmpty()
								&& displayName.equalsIgnoreCase(identifier))) {
					return siteName;
				}
			}
		}

		return null;
	}

	/**
	 * Checks whether an identifier belongs to any configured vote site, including
	 * a disabled site.
	 *
	 * @param identifiers vote-site identifiers
	 * @return true when a configured vote-site section matches
	 */
	public boolean hasConfiguredVoteSite(String... identifiers) {
		return getConfiguredVoteSiteName(identifiers) != null;
	}

	/**
	 * Attempts to map a URL, display name, or key to the configured vote site key.
	 *
	 * @param checkEnabled whether to only consider enabled vote sites
	 * @param urls one or more identifiers to resolve
	 * @return the resolved vote site key, or the first provided value if no match is
	 *         found
	 */
	public String getVoteSiteName(boolean checkEnabled, String... urls) {
		if (urls == null) {
			return null;
		}

		for (String url : urls) {
			if (url == null) {
				return null;
			}

			if (!url.isEmpty()) {
				for (VoteSite site : voteSites) {
					if (checkEnabled && !site.isEnabled()) {
						continue;
					}

					String serviceSite = site.getServiceSite();
					if (serviceSite != null && serviceSite.equalsIgnoreCase(url)) {
						return site.getKey();
					}

					if (site.getKey().equalsIgnoreCase(url)) {
						return site.getKey();
					}

					String displayName = site.getDisplayName();
					if (displayName != null && displayName.equalsIgnoreCase(url)) {
						return site.getKey();
					}
				}
			}
		}

		if (!checkEnabled) {
			String configuredSiteName = getConfiguredVoteSiteName(urls);
			if (configuredSiteName != null) {
				return configuredSiteName;
			}
		}

		for (String url : urls) {
			return url;
		}

		return "";
	}

	/**
	 * Resolves a VoteSite from an identifier.
	 *
	 * @param site the site identifier
	 * @param checkEnabled whether to only match enabled sites
	 * @return the vote site, or null if not found
	 */
	public VoteSite getVoteSite(String site, boolean checkEnabled) {
		String siteName = getVoteSiteName(checkEnabled, site);

		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName)) {
				return voteSite;
			}

			String displayName = voteSite.getDisplayName();
			if (displayName != null && displayName.equalsIgnoreCase(siteName)) {
				return voteSite;
			}
		}

		if (plugin.getConfigFile().isAutoCreateVoteSites() && !hasVoteSite(siteName)
				&& !hasConfiguredVoteSite(siteName)) {
			if (!ServiceSiteValidator.isValid(siteName)) {
				plugin.getLogger().warning("Unable to auto-create vote site with unsupported name '"
						+ ServiceSiteValidator.sanitizeForLog(siteName) + "'");
				return null;
			}
			if (!plugin.getConfigVoteSites().tryGenerateVoteSite(siteName)) {
				return null;
			}
			return new VoteSite(plugin, normalizeVoteSiteKey(siteName));
		}

		return null;
	}

	/**
	 * Gets all enabled vote sites.
	 *
	 * @return the enabled vote sites
	 */
	public ArrayList<VoteSite> getVoteSitesEnabled() {
		ArrayList<VoteSite> sites = new ArrayList<VoteSite>();

		for (VoteSite site : getVoteSites()) {
			if (site.isEnabled()) {
				sites.add(site);
			}
		}

		return sites;
	}

	/**
	 * Converts an input name or key into the configured service site if possible.
	 *
	 * @param name the input name
	 * @return the service site, or the original input if no mapping exists
	 */
	public String getVoteSiteServiceSite(String name) {
		if (name == null) {
			return null;
		}

		for (VoteSite site : voteSites) {
			if (!site.isEnabled()) {
				continue;
			}

			String url = site.getServiceSite();
			if (url != null) {
				if (url.equalsIgnoreCase(name) || name.equalsIgnoreCase(site.getKey())) {
					return url;
				}
			}
		}

		return name;
	}

	/**
	 * Checks whether a vote site exists.
	 *
	 * @param site the site identifier
	 * @return true if the vote site exists
	 */
	public boolean hasVoteSite(String site) {
		String siteName = getVoteSiteName(false, site);
		if (siteName == null) {
			return false;
		}

		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName)) {
				return true;
			}

			String displayName = voteSite.getDisplayName();
			if (displayName != null && displayName.equalsIgnoreCase(siteName)) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Checks whether a vote site key exists.
	 *
	 * @param voteSite the vote site key
	 * @return true if it exists
	 */
	public boolean isVoteSite(String voteSite) {
		for (VoteSite site : getVoteSites()) {
			if (site.getKey().equalsIgnoreCase(voteSite)) {
				return true;
			}
		}

		return false;
	}
}
