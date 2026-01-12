package com.bencodez.votingplugin.tests.broadcast;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.broadcast.VoteBroadcastType;

public class VoteBroadcastTypeTest {

	@Test
	public void parse_null_returns_default() {
		assertEquals(VoteBroadcastType.EVERY_VOTE, VoteBroadcastType.parse(null, VoteBroadcastType.EVERY_VOTE));
	}

	@Test
	public void parse_empty_returns_default() {
		assertEquals(VoteBroadcastType.COOLDOWN_PER_PLAYER, VoteBroadcastType.parse("   ", VoteBroadcastType.COOLDOWN_PER_PLAYER));
	}

	@Test
	public void parse_invalid_returns_default() {
		assertEquals(VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL, VoteBroadcastType.parse("nope", VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL));
	}

	@Test
	public void parse_case_insensitive() {
		assertEquals(VoteBroadcastType.FIRST_VOTE_OF_DAY, VoteBroadcastType.parse("first_vote_of_day", VoteBroadcastType.EVERY_VOTE));
		assertEquals(VoteBroadcastType.BATCH_WINDOW_PER_PLAYER, VoteBroadcastType.parse("BaTcH_WiNdOw_PeR_PlAyEr", VoteBroadcastType.EVERY_VOTE));
	}
}
