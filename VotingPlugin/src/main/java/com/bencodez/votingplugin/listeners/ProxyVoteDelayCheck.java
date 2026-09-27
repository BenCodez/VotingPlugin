package com.bencodez.votingplugin.listeners;

import com.bencodez.votingplugin.core.vote.SharedVoteProcessor;

/**
 * Legacy proxy vote-delay helper retained for integrations compiled against the
 * previous public entry point.
 *
 * @deprecated production processing uses stable vote IDs
 */
@Deprecated
public final class ProxyVoteDelayCheck {

	private ProxyVoteDelayCheck() {
	}

	/**
	 * @deprecated timestamps are not stable occurrence identities
	 */
	@Deprecated
	public static boolean isQueuedVoteAlreadyRecorded(boolean proxyVote, long messageVoteTime, long storedVoteTime) {
		return SharedVoteProcessor.isQueuedVoteAlreadyRecorded(proxyVote, messageVoteTime, storedVoteTime);
	}
}
