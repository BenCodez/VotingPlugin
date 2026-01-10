package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;

import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.AtMatcher;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneGroupSelect;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneLimit;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneTotal;

class VoteMilestoneMatchTest {

	@Test
	void matchesEvery() {
		VoteMilestone m = new VoteMilestone("Every20", true, VoteMilestoneTotal.ALLTIME_VOTES, null, 20, "Rewards",
				VoteMilestoneGroupSelect.ALL, "Default", VoteMilestoneLimit.none());

		assertFalse(m.matches(19));
		assertTrue(m.matches(20));
		assertFalse(m.matches(21));
		assertTrue(m.matches(40));
	}

	@Test
	void matchesAtOnly() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", 50);

		AtMatcher at = AtMatcher.fromConfig(c, "At");
		VoteMilestone m = new VoteMilestone("At50", true, VoteMilestoneTotal.ALLTIME_VOTES, at, null, "Rewards",
				VoteMilestoneGroupSelect.ALL, "Default", VoteMilestoneLimit.none());

		assertTrue(m.matches(50));
		assertFalse(m.matches(49));
	}

	@Test
	void matchesAtOrEvery() {
		MemoryConfiguration c = new MemoryConfiguration();
		c.set("At", Arrays.asList(50, 100));
		AtMatcher at = AtMatcher.fromConfig(c, "At");

		VoteMilestone m = new VoteMilestone("Combo", true, VoteMilestoneTotal.ALLTIME_VOTES, at, 25, "Rewards",
				VoteMilestoneGroupSelect.ALL, "Default", VoteMilestoneLimit.none());

		// Every
		assertTrue(m.matches(25));
		assertTrue(m.matches(75));

		// At
		assertTrue(m.matches(50));
		assertTrue(m.matches(100));

		// Neither
		assertFalse(m.matches(26));
	}
}
