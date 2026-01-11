package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.*;

import java.time.ZoneId;
import java.time.ZonedDateTime;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneLimit;

class VoteMilestoneLimitTest {

	private static ConfigurationSection milestoneWithLimit(String type, String duration) {
		MemoryConfiguration ms = new MemoryConfiguration();
		ConfigurationSection lim = ms.createSection("Limit");
		if (type != null) {
			lim.set("Type", type);
		}
		if (duration != null) {
			lim.set("Duration", duration);
		}
		return ms;
	}

	@Test
	void defaultIsNoneWhenMissing() {
		MemoryConfiguration ms = new MemoryConfiguration();
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(ms);
		assertNotNull(limit);
		assertEquals(VoteMilestoneLimit.Type.NONE, limit.getType());
		assertFalse(limit.isEnabled());
	}

	@Test
	void parsesWindowDay() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("WINDOW_DAY", null));
		assertEquals(VoteMilestoneLimit.Type.WINDOW_DAY, limit.getType());
		assertTrue(limit.isEnabled());
	}

	@Test
	void parsesCooldownNumericAsMinutes() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "30"));
		assertEquals(VoteMilestoneLimit.Type.COOLDOWN, limit.getType());
		assertTrue(limit.isEnabled());
		assertEquals(30L * 60_000L, limit.getCooldownMillis());
	}

	@Test
	void parsesCooldownWithUnitHours() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "12h"));
		assertEquals(VoteMilestoneLimit.Type.COOLDOWN, limit.getType());
		assertEquals(12L * 3_600_000L, limit.getCooldownMillis());
	}

	@Test
	void parsesCooldownMonthAsFixedMillis() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "1mo"));
		assertEquals(VoteMilestoneLimit.Type.COOLDOWN, limit.getType());

		// 30 days fixed
		long expected = 30L * 86_400_000L;
		assertEquals(expected, limit.getCooldownMillis());
	}

	@Test
	void windowDayBlocksSameDayAllowsNextDay() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("WINDOW_DAY", null));
		ZoneId zone = ZoneId.of("UTC");

		long last = ZonedDateTime.of(2026, 1, 9, 10, 0, 0, 0, zone).toInstant().toEpochMilli();
		long sameDay = ZonedDateTime.of(2026, 1, 9, 23, 0, 0, 0, zone).toInstant().toEpochMilli();
		long nextDay = ZonedDateTime.of(2026, 1, 10, 0, 1, 0, 0, zone).toInstant().toEpochMilli();

		assertFalse(limit.allows(last, sameDay, zone));
		assertTrue(limit.allows(last, nextDay, zone));
	}

	@Test
	void cooldownMillisBlocksUntilElapsed() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "10m"));
		ZoneId zone = ZoneId.of("UTC");

		long last = 1_000_000L;
		long tooSoon = last + (9 * 60_000L);
		long ok = last + (10 * 60_000L);

		assertFalse(limit.allows(last, tooSoon, zone));
		assertTrue(limit.allows(last, ok, zone));
	}
}
