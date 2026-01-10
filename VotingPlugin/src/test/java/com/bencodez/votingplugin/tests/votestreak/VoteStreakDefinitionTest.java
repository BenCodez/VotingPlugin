package com.bencodez.votingplugin.tests.votestreak;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;

class VoteStreakDefinitionTest {

	@Test
	void constructor_clampsNegativeAllowMissedAndVotesRequired() {
		VoteStreakDefinition def = new VoteStreakDefinition(
				"daily",
				VoteStreakType.DAILY,
				true,
				2,        // requiredAmount
				0,        // votesRequired -> should clamp to 1
				-5,       // allowMissedAmount -> clamp to 0
				-99       // allowMissedPeriod -> clamp to 0
		);

		assertEquals("daily", def.getId());
		assertEquals(VoteStreakType.DAILY, def.getType());
		assertTrue(def.isEnabled());

		assertEquals(2, def.getRequiredAmount());
		assertEquals(1, def.getVotesRequired());         // clamped
		assertEquals(0, def.getAllowMissedAmount());     // clamped
		assertEquals(0, def.getAllowMissedPeriod());     // clamped
	}
}
