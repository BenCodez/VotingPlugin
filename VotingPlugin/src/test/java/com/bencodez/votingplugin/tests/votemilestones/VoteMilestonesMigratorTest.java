package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneLimit;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneTotal;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestonesMigrator;

class VoteMilestonesMigratorTest {

	@Test
	void migratesFirstVoteUsesAbsoluteRewardPath() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection fv = root.createSection("FirstVote");
		fv.set("Messages.Player", "hi");

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(root, 0, false, false);

		assertTrue(legacy.containsKey("Legacy_FirstVote"));
		VoteMilestone m = legacy.get("Legacy_FirstVote");
		assertNotNull(m);

		assertEquals(VoteMilestoneTotal.ALLTIME_VOTES, m.getTotal());
		assertEquals("FirstVote", m.getRewardPath());

		assertTrue(m.matches(1));
		assertFalse(m.matches(2));

		assertTrue(root.isString("FirstVote.Messages.Player"));
	}

	@Test
	void migratesCumulativeUsesAbsoluteRewardPath() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection cum = root.createSection("Cumulative");
		ConfigurationSection c20 = cum.createSection("20");
		c20.set("Enabled", true);
		c20.set("TotalToUse", "Daily");
		ConfigurationSection rewards = c20.createSection("Rewards");
		rewards.set("Messages.Player", "yo");

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(root, 0, false, false);

		VoteMilestone m = legacy.get("Legacy_Cumulative_20_DAILY_VOTES");
		assertNotNull(m);

		assertEquals(VoteMilestoneTotal.DAILY_VOTES, m.getTotal());
		assertEquals("Cumulative.20.Rewards", m.getRewardPath());

		assertTrue(m.matches(20));
		assertFalse(m.matches(21));

		assertTrue(root.isString("Cumulative.20.Rewards.Messages.Player"));
	}

	@Test
	void migratesMilestonesUsesAbsoluteRewardPath() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection ms = root.createSection("MileStones");
		ConfigurationSection m50 = ms.createSection("50");
		m50.set("Enabled", true);
		ConfigurationSection rewards = m50.createSection("Rewards");
		rewards.set("Commands", java.util.Arrays.asList("say test"));

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(root, 0, false, false);

		VoteMilestone m = legacy.get("Legacy_Milestone_50_ALLTIME");
		assertNotNull(m);

		assertEquals(VoteMilestoneTotal.ALLTIME_VOTES, m.getTotal());
		assertEquals("MileStones.50.Rewards", m.getRewardPath());

		assertTrue(m.matches(50));
		assertFalse(m.matches(49));

		assertTrue(root.isList("MileStones.50.Rewards.Commands"));
	}

	@Test
	void migratesAllSitesUsesRewardsPathAndAtTotalSites_andHasDailyLimit() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection as = root.createSection("AllSites");
		as.set("Messages.Player", "all sites!");

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(root, 5, false, false);

		VoteMilestone m = legacy.get("Legacy_AllSites");
		assertNotNull(m);
		assertEquals(VoteMilestoneTotal.ALLSITES_TODAY, m.getTotal());
		assertEquals("AllSites", m.getRewardPath());

		assertTrue(m.matches(5));
		assertFalse(m.matches(4));

		assertNotNull(m.getLimit());
		assertEquals(VoteMilestoneLimit.Type.WINDOW_DAY, m.getLimit().getType());
		assertTrue(m.getLimit().isEnabled());
	}

	@Test
	void migratesAlmostAllSitesHasDailyLimit() {
		MemoryConfiguration root = new MemoryConfiguration();
		root.createSection("AlmostAllSites").set("Messages.Player", "almost!");

		Map<String, VoteMilestone> legacy = VoteMilestonesMigrator.compileLegacy(root, 5, false, false);

		VoteMilestone m = legacy.get("Legacy_AlmostAllSites");
		assertNotNull(m);

		assertNotNull(m.getLimit());
		assertEquals(VoteMilestoneLimit.Type.WINDOW_DAY, m.getLimit().getType());
		assertTrue(m.getLimit().isEnabled());
	}
}
