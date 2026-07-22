package com.bencodez.votingplugin.listeners;

/**
 * Handles the WaitUntilVoteDelay exception for votes queued by a proxy.
 */
public final class ProxyVoteDelayCheck {

	private ProxyVoteDelayCheck() {
	}

	/**
	 * A proxy stores the real vote timestamp before its queued message reaches the
	 * backend. The backend must accept that one delivery when the stored timestamp
	 * is the same timestamp carried by the message.
	 *
	 * @param proxyVote true when the vote was forwarded by VotingPlugin's proxy
	 * @param messageVoteTime real vote time included in the proxy message
	 * @param storedVoteTime current backend last-vote time for the site
	 * @return true when the delay check should allow the queued delivery
	 */
	public static boolean isQueuedVoteAlreadyRecorded(boolean proxyVote, long messageVoteTime, long storedVoteTime) {
		return proxyVote && messageVoteTime > 0L && messageVoteTime == storedVoteTime;
	}
}
