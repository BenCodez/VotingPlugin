package com.bencodez.votingplugin.votesites;
public final class NextSite {

    private final VoteSite site;
    private final long secondsUntilAvailable;

    public NextSite(VoteSite site, long secondsUntilAvailable) {
        this.site = site;
        this.secondsUntilAvailable = secondsUntilAvailable;
    }

    public VoteSite getSite() {
        return site;
    }

    public long getSecondsUntilAvailable() {
        return secondsUntilAvailable;
    }
}
