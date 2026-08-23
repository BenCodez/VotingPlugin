package com.bencodez.votingplugin.votesites;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Owns the currently loaded vote sites.
 */
public class VoteSiteRegistry {

	private List<VoteSite> voteSites = Collections.synchronizedList(new ArrayList<VoteSite>());

	public List<VoteSite> getVoteSites() {
		return voteSites;
	}

	public void setVoteSites(List<VoteSite> voteSites) {
		if (voteSites == null) {
			this.voteSites = Collections.synchronizedList(new ArrayList<VoteSite>());
			return;
		}
		this.voteSites = voteSites;
	}

	public ArrayList<VoteSite> getEnabledVoteSites() {
		ArrayList<VoteSite> sites = new ArrayList<VoteSite>();
		for (VoteSite site : voteSites) {
			if (site.isEnabled()) {
				sites.add(site);
			}
		}
		return sites;
	}
}
