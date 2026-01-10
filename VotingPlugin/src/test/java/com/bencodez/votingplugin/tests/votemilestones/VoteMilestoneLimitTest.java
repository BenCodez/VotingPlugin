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
		assertEquals(0, limit.getCooldownMonths());
	}

	@Test
	void parsesCooldownWithUnitHours() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "12h"));
		assertEquals(VoteMilestoneLimit.Type.COOLDOWN, limit.getType());
		assertEquals(12L * 3_600_000L, limit.getCooldownMillis());
		assertEquals(0, limit.getCooldownMonths());
	}

	@Test
	void parsesCooldownMonths() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "1mo"));
		assertEquals(VoteMilestoneLimit.Type.COOLDOWN, limit.getType());
		assertEquals(0L, limit.getCooldownMillis());
		assertEquals(1, limit.getCooldownMonths());
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

		long last = ZonedDateTime.of(2026, 1, 9, 10, 0, 0, 0, zone).toInstant().toEpochMilli();
		long before = ZonedDateTime.of(2026, 1, 9, 10, 9, 59, 0, zone).toInstant().toEpochMilli();
		long after = ZonedDateTime.of(2026, 1, 9, 10, 10, 0, 0, zone).toInstant().toEpochMilli();

		assertFalse(limit.allows(last, before, zone));
		assertTrue(limit.allows(last, after, zone));
		assertEquals(last + 10L * 60_000L, limit.nextAllowedMs(last, zone));
	}

	@Test
	void cooldownMonthIsCalendarMonth() {
		VoteMilestoneLimit limit = VoteMilestoneLimit.fromConfig(milestoneWithLimit("COOLDOWN", "1mo"));
		ZoneId zone = ZoneId.of("UTC");

		long last = ZonedDateTime.of(2026, 1, 31, 10, 0, 0, 0, zone).toInstant().toEpochMilli();
		long nextAllowed = limit.nextAllowedMs(last, zone);

		// Calendar month add: Jan 31 + 1 month -> Feb 28 (or 29 in leap years) at same local time
		ZonedDateTime z = ZonedDateTime.ofInstant(java.time.Instant.ofEpochMilli(nextAllowed), zone);
		assertEquals(2026, z.getYear());
		assertEquals(2, z.getMonthValue());
		assertTrue(z.getDayOfMonth() == 28 || z.getDayOfMonth() == 29);
	}
}
