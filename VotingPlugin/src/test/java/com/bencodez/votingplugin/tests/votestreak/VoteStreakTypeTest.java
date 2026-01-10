package com.bencodez.votingplugin.tests.votestreak;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;

class VoteStreakTypeTest {

	@Test
	void from_isCaseInsensitiveAndTrims() {
		assertEquals(VoteStreakType.DAILY, VoteStreakType.from("daily"));
		assertEquals(VoteStreakType.WEEKLY, VoteStreakType.from("  weekly  "));
		assertEquals(VoteStreakType.MONTHLY, VoteStreakType.from("MoNtHlY"));
	}

	@Test
	void from_throwsForInvalid() {
		IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> VoteStreakType.from("hourly"));
		assertTrue(ex.getMessage().contains("Invalid VoteStreak Type"));
	}
}
