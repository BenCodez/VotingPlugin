package com.bencodez.votingplugin.tests.votestreak;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.time.TimeChecker;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakDefinition;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakHandler;
import com.bencodez.votingplugin.specialrewards.votestreak.VoteStreakType;
import com.bencodez.votingplugin.user.VotingPluginUser;

class VoteStreakHandlerTest {
	private VotingPluginMain plugin;
	private TimeChecker timeChecker;
	private VoteStreakHandler handler;

	@BeforeEach
	void setup() {
		plugin = mock(VotingPluginMain.class, RETURNS_DEEP_STUBS);
		timeChecker = mock(TimeChecker.class);
		when(plugin.getTimeChecker()).thenReturn(timeChecker);
		when(timeChecker.getTime()).thenReturn(LocalDateTime.of(2026, 1, 10, 12, 0));
		when(plugin.getLogger()).thenReturn(silentLogger());
		handler = new VoteStreakHandler(plugin);
	}

	private static VotingPluginUser mapBackedUser(UUID uuid, String name, Map<String, String> backing) {
		VotingPluginUser user = mock(VotingPluginUser.class);
		when(user.getJavaUUID()).thenReturn(uuid);
		when(user.getPlayerName()).thenReturn(name);
		when(user.getVoteStreakState(anyString())).thenAnswer(inv -> backing.get(inv.getArgument(0, String.class)));
		doAnswer(inv -> {
			backing.put(inv.getArgument(0, String.class), inv.getArgument(1, String.class));
			return null;
		}).when(user).setVoteStreakState(anyString(), anyString());
		return user;
	}

	private static Logger silentLogger() {
		Logger l = Logger.getLogger("silent-test-logger");
		l.setUseParentHandlers(false);
		l.setLevel(Level.OFF);
		for (java.util.logging.Handler h : l.getHandlers()) {
			l.removeHandler(h);
		}
		return l;
	}

	private static MemoryConfiguration rootWithOneStreak(String id, String type, boolean enabled, int intervalAmount,
			int votesRequired, int allowMissedAmount, int allowMissedPeriod, boolean recurring) {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection def = root.createSection("VoteStreaks").createSection(id);
		def.set("Type", type);
		def.set("Enabled", enabled);
		ConfigurationSection req = def.createSection("Requirements");
		req.set("Amount", intervalAmount);
		req.set("VotesRequired", votesRequired);
		def.set("AllowMissedAmount", allowMissedAmount);
		def.set("AllowMissedPeriod", allowMissedPeriod);
		def.set("Recurring", recurring);
		return root;
	}

	private static String[] parseState(String raw) {
		assertNotNull(raw);
		assertFalse(raw.isEmpty());
		String[] p = raw.split("\\|", -1);
		assertTrue(p.length >= 6, "expected >= 6 fields, got " + p.length + " raw=" + raw);
		return p;
	}

	private void loadFromRoot(ConfigurationSection root) {
		handler.new VoteStreakConfigLoader().load(root);
	}

	@Test
	void configLoader_loadsDefinition_andGetDefinitionWorks() {
		MemoryConfiguration root = rootWithOneStreak("DailyStreak", "DAILY", true, 5, 2, 0, 0, true);
		loadFromRoot(root);
		VoteStreakDefinition def = handler.getById().get("DailyStreak");
		assertNotNull(def);
		assertEquals("DailyStreak", def.getId());
		assertEquals(VoteStreakType.DAILY, def.getType());
		assertTrue(def.isEnabled());
		assertEquals(5, def.getRequiredAmount());
		assertEquals(2, def.getVotesRequired());
	}

	@Test
	void processVote_whenAlreadyCountedThisPeriod_doesNotIncrementStreakAgain() {
		MemoryConfiguration root = rootWithOneStreak("test", "DAILY", true, 9999, 2, 0, 0, true);
		loadFromRoot(root);
		VoteStreakDefinition def = handler.getById().get("test");
		String col = handler.getColumnName(def);
		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		backing.put(col, "2026-01-10|5|1|true||0");
		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());
		String[] p = parseState(handler.readStateString(user, col));
		assertEquals("5", p[1]);
		assertEquals("1", p[2]);
		assertEquals("true", p[3]);
	}

	@Test
	void processVote_acceptsOldFiveFieldFormat_andUpgradesState() {
		MemoryConfiguration root = rootWithOneStreak("test", "DAILY", true, 9999, 2, 0, 0, true);
		loadFromRoot(root);
		VoteStreakDefinition def = handler.getById().get("test");
		String col = handler.getColumnName(def);
		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		backing.put(col, "2026-01-10|5|true||2");
		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());
		String[] p = parseState(handler.readStateString(user, col));
		assertEquals("2026-01-10", p[0]);
		assertEquals("5", p[1]);
		assertEquals("1", p[2]);
		assertEquals("true", p[3]);
		assertEquals("2", p[5]);
	}

	@Test
	void processVote_migratesLegacyProgressIntoNewState() {
		MemoryConfiguration root = rootWithOneStreak("daily", "DAILY", true, 99, 1, 0, 0, true);
		loadFromRoot(root);
		VoteStreakDefinition def = handler.getById().get("daily");
		String col = handler.getColumnName(def);
		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		when(user.getDayVoteStreak()).thenReturn(7);
		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());
		String[] p = parseState(handler.readStateString(user, col));
		assertEquals("8", p[1]);
	}

	@Test
	void configLoader_migratesLegacyVoteStreakSection() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection day = root.createSection("VoteStreak").createSection("Day");
		day.createSection("2").set("Enabled", true);
		day.createSection("-3").set("Enabled", true);

		when(plugin.getSpecialRewardsConfig().getData()).thenReturn(root);
		handler.migrateLegacyConfigManually();
		loadFromRoot(root);

		assertEquals(2, handler.getDefinitions().size());
		assertNotNull(root.getConfigurationSection("VoteStreaks.LegacyDAILY2OneTime"));
		assertNotNull(root.getConfigurationSection("VoteStreaks.LegacyDAILY3Recurring"));
		assertFalse(root.getBoolean("VoteStreak.Day.2.Enabled"));
		assertFalse(root.getBoolean("VoteStreak.Day.-3.Enabled"));
	}
}
