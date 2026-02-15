package com.bencodez.votingplugin.votesites;
/**
 * Represents the next available vote site.
 */
public final class NextSite {

    private final VoteSite site;
    private final long secondsUntilAvailable;

    /**
     * Constructor for NextSite.
     * @param site the vote site
     * @param secondsUntilAvailable seconds until site is available
     */
    public NextSite(VoteSite site, long secondsUntilAvailable) {
        this.site = site;
        this.secondsUntilAvailable = secondsUntilAvailable;
    }

    /**
     * Get the vote site.
     * @return the vote site
     */
    public VoteSite getSite() {
        return site;
    }

    /**
     * Get seconds until available.
     * @return seconds until available
     */
    public long getSecondsUntilAvailable() {
        return secondsUntilAvailable;
    }
}
