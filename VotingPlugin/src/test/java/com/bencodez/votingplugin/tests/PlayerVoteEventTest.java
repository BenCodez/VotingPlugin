package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.events.PlayerVoteEvent;

/**
 * Tests queue-delivery metadata carried by proxy vote events.
 */
public class PlayerVoteEventTest {

	@Test
	public void eventIsNotQueuedByDefault() {
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Player", "Service", true);

		assertFalse(event.isQueuedProxyVote());
	}

	@Test
	public void eventCanMarkAnIdentifiedQueuedProxyDelivery() {
		PlayerVoteEvent event = new PlayerVoteEvent(null, "Player", "Service", true);

		event.setQueuedProxyVote(true);

		assertTrue(event.isQueuedProxyVote());
	}
}
