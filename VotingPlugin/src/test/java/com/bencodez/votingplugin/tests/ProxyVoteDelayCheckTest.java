package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.listeners.ProxyVoteDelayCheck;

class ProxyVoteDelayCheckTest {

	@Test
	void acceptsQueuedProxyVoteWhenTheProxyTimestampIsAlreadyStored() {
		assertTrue(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(true, 12345L, 12345L));
	}

	@Test
	void doesNotBypassTheDelayForDirectOrStaleProxyVotes() {
		assertFalse(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(false, 12345L, 12345L));
		assertFalse(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(true, 12344L, 12345L));
		assertFalse(ProxyVoteDelayCheck.isQueuedVoteAlreadyRecorded(true, 0L, 0L));
	}
}
