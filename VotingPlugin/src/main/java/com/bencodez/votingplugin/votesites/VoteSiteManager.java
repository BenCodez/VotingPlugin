package com.bencodez.votingplugin.votesites;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

public class VoteSiteManager {

	@Getter
	@Setter
	private VotingPluginMain plugin;

	@Getter
	private final VoteSiteRegistry registry;

	@Getter
	private final VoteSiteValidator validator;

	@Getter
	private final VoteSiteResolver resolver;

	@Getter
	private final VoteSiteFactory factory;

	public VoteSiteManager(VotingPluginMain plugin) {
		this.plugin = plugin;
		registry = new VoteSiteRegistry();
		validator = new VoteSiteValidator(plugin);
		resolver = new VoteSiteResolver(plugin, registry, validator);
		factory = new VoteSiteFactory(plugin, resolver, validator);
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
			validator.validateVoteSiteName(site.getKey());
		}

		registry.setVoteSites(newSites);

		if (registry.getVoteSites().isEmpty()) {
			plugin.getLogger().warning("Detected no voting sites, this may mean something isn't properly setup");
		}

		plugin.debug("Loaded VoteSites");
		return registry.getVoteSites();
	}

	public List<VoteSite> getVoteSites() {
		return registry.getVoteSites();
	}

	/**
	 * Kept for API compatibility. New code should prefer the registry directly.
	 *
	 * @param voteSites the loaded vote sites
	 */
	public void setVoteSites(List<VoteSite> voteSites) {
		registry.setVoteSites(voteSites);
	}

	public void validateVoteSiteName(String siteName) {
		validator.validateVoteSiteName(siteName);
	}

	public String normalizeVoteSiteKey(String name) {
		return validator.normalizeVoteSiteKey(name);
	}

	public boolean hasConfiguredVoteSite(String... identifiers) {
		return resolver.hasConfiguredVoteSite(identifiers);
	}

	public String getVoteSiteName(boolean checkEnabled, String... urls) {
		return resolver.getVoteSiteName(checkEnabled, urls);
	}

	/**
	 * Resolves an already-loaded vote site without creating configuration.
	 *
	 * @param site the site identifier
	 * @param checkEnabled whether to only match enabled sites
	 * @return the loaded vote site, or null when none matches
	 */
	public VoteSite resolveVoteSite(String site, boolean checkEnabled) {
		return resolver.resolveVoteSite(site, checkEnabled);
	}

	/**
	 * Resolves a vote site and preserves the legacy auto-create behavior when no
	 * loaded site matches.
	 *
	 * @param site the site identifier
	 * @param checkEnabled whether to only match enabled sites
	 * @return the vote site, or null if not found or created
	 */
	public VoteSite getVoteSite(String site, boolean checkEnabled) {
		VoteSite voteSite = resolver.resolveVoteSite(site, checkEnabled);
		if (voteSite != null) {
			return voteSite;
		}

		String siteName = resolver.getVoteSiteName(checkEnabled, site);
		return factory.createIfAllowed(siteName);
	}

	public ArrayList<VoteSite> getVoteSitesEnabled() {
		return registry.getEnabledVoteSites();
	}

	public String getVoteSiteServiceSite(String name) {
		return resolver.getVoteSiteServiceSite(name);
	}

	public boolean hasVoteSite(String site) {
		return resolver.hasVoteSite(site);
	}

	public boolean isVoteSite(String voteSite) {
		return resolver.isVoteSite(voteSite);
	}
}
