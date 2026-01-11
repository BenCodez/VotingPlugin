package com.bencodez.votingplugin.votesites;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class VoteSiteManager {

	private final VotingPluginMain plugin;

	@Getter
	private List<VoteSite> voteSites = Collections.synchronizedList(new ArrayList<>());

	public VoteSiteManager(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Reloads votesites from config and returns the backing list.
	 */
	public List<VoteSite> loadVoteSites() {
		plugin.getConfigVoteSites().setup();

		List<VoteSite> newSites = Collections.synchronizedList(new ArrayList<>());
		newSites.addAll(plugin.getConfigVoteSites().getVoteSitesLoad());
		voteSites = newSites;

		if (voteSites.isEmpty()) {
			plugin.getLogger().warning("Detected no voting sites, this may mean something isn't properly setup");
		}

		plugin.debug("Loaded VoteSites");
		return voteSites;
	}
	/**
	 * Attempts to map a URL / display name / key to the configured "site name".
	 * 
	 * @param checkEnabled Whether to only consider enabled vote sites.
	 * @param urls         One or more URLs / names to resolve.
	 * @return The resolved vote site name, or the first provided value if no match
	 */
	public String getVoteSiteName(boolean checkEnabled, String... urls) {
		ArrayList<String> sites = plugin.getConfigVoteSites().getVoteSitesNames(checkEnabled);

		for (String url : urls) {
			if (url == null) {
				return null;
			}
			if (!url.isEmpty() && sites != null) {
				for (String siteName : sites) {
					String configuredUrl = plugin.getConfigVoteSites().getServiceSite(siteName);
					if (configuredUrl != null && configuredUrl.equalsIgnoreCase(url)) {
						return siteName;
					}
					if (siteName.equalsIgnoreCase(url)) {
						return siteName;
					}
				}
			}
		}

		// If no match, return the first provided value (original behavior)
		for (String url : urls) {
			return url;
		}
		return "";
	}

	/**
	 * Resolves a VoteSite from an identifier.
	 * - Matches on key or display name
	 * - Can optionally auto-create vote sites (same behavior as VotingPluginMain)
	 */
	public VoteSite getVoteSite(String site, boolean checkEnabled) {
		String siteName = getVoteSiteName(checkEnabled, site);

		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return voteSite;
			}
		}

		if (plugin.getConfigFile().isAutoCreateVoteSites()
				&& !plugin.getConfigVoteSites().getVoteSitesNames(false).contains(siteName)) {

			plugin.getConfigVoteSites().generateVoteSite(siteName);

			// Original code used: new VoteSite(plugin, siteName.replace(".", "_"));
			// Keep identical behavior.
			return new VoteSite(plugin, siteName.replace(".", "_"));
		}

		return null;
	}

	public ArrayList<VoteSite> getVoteSitesEnabled() {
		ArrayList<VoteSite> sites = new ArrayList<>();
		for (VoteSite site : getVoteSites()) {
			if (site.isEnabled()) {
				sites.add(site);
			}
		}
		return sites;
	}

	/**
	 * Converts an input name/url into the configured service site URL if possible.
	 * If no mapping exists, returns the input (original behavior).
	 */
	public String getVoteSiteServiceSite(String name) {
		ArrayList<String> sites = plugin.getConfigVoteSites().getVoteSitesNames(true);
		if (name == null) {
			return null;
		}

		if (sites != null) {
			for (String siteName : sites) {
				String url = plugin.getConfigVoteSites().getServiceSite(siteName);
				if (url != null) {
					if (url.equalsIgnoreCase(name) || name.equalsIgnoreCase(siteName)) {
						return url;
					}
				}
			}
		}
		return name;
	}

	public boolean hasVoteSite(String site) {
		String siteName = getVoteSiteName(false, site);
		for (VoteSite voteSite : getVoteSites()) {
			if (voteSite.getKey().equalsIgnoreCase(siteName) || voteSite.getDisplayName().equals(siteName)) {
				return true;
			}
		}
		return false;
	}

	public boolean isVoteSite(String voteSite) {
		for (VoteSite site : getVoteSites()) {
			if (site.getKey().equalsIgnoreCase(voteSite)) {
				return true;
			}
		}
		return false;
	}
}
