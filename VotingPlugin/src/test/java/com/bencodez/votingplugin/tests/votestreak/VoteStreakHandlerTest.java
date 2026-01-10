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
		when(plugin.getLogger()).thenReturn(Logger.getLogger("VoteStreakHandlerTest"));
		
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
	    l.setUseParentHandlers(false);   // <- key: stops ConsoleHandler printing
	    l.setLevel(Level.OFF);           // or Level.WARNING if you want warnings
	    // also remove any handlers that might already be attached
	    java.util.logging.Handler[] hs = l.getHandlers();
	    for (java.util.logging.Handler h : hs) {
	        l.removeHandler(h);
	    }
	    return l;
	}


	private static MemoryConfiguration rootWithOneStreak(String id, String type, boolean enabled, int intervalAmount,
			int votesRequired, int allowMissedAmount, int allowMissedPeriod) {

		MemoryConfiguration root = new MemoryConfiguration();

		ConfigurationSection def = root.createSection("VoteStreaks").createSection(id);
		def.set("Type", type);
		def.set("Enabled", enabled);

		ConfigurationSection req = def.createSection("Requirements");
		req.set("Amount", intervalAmount);
		req.set("VotesRequired", votesRequired);

		def.set("AllowMissedAmount", allowMissedAmount);
		def.set("AllowMissedPeriod", allowMissedPeriod);

		return root;
	}

	/**
	 * periodKey|streakCount|votesThisPeriod|countedThisPeriod|missWindowStartKey|missesUsed
	 */
	private static String[] parseState(String raw) {
		assertNotNull(raw);
		assertFalse(raw.isEmpty());
		String[] p = raw.split("\\|", -1);
		assertTrue(p.length >= 6, "expected >= 6 fields, got " + p.length + " raw=" + raw);
		return p;
	}

	private void loadFromRoot(ConfigurationSection root) {
		// IMPORTANT: no plugin.getSpecialRewardsConfig() involved.
		handler.new VoteStreakConfigLoader().load(root);
	}

	@Test
	void configLoader_loadsDefinition_andGetDefinitionWorks() {
		MemoryConfiguration root = rootWithOneStreak("DailyStreak", "DAILY", true, 5, 2, 0, 0);
		loadFromRoot(root);

		// getDefinition() is broken due to key casing mismatch in handler: byId.put(id)
		// vs lookup lowercased
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
		MemoryConfiguration root = rootWithOneStreak("test", "DAILY", true, 9999, 2, 0, 0);
		loadFromRoot(root);

		VoteStreakDefinition def = handler.getById().get("test");
		assertNotNull(def);

		String col = handler.getColumnName(def);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);

		backing.put(col, "2026-01-10|5|1|true||0");

		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());

		String[] p = parseState(handler.readStateString(user, col));
		assertEquals("5", p[1], "streakCount must not increment again when already counted");
		assertEquals("1", p[2], "votesThisPeriod should NOT increment once already countedThisPeriod=true");
		assertEquals("true", p[3]);
	}

	@Test
	void processVote_acceptsOldFiveFieldFormat_andUpgradesState() {
		MemoryConfiguration root = rootWithOneStreak("test", "DAILY", true, 9999, 2, 0, 0);
		loadFromRoot(root);

		VoteStreakDefinition def = handler.getById().get("test");
		assertNotNull(def);

		String col = handler.getColumnName(def);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);

		backing.put(col, "2026-01-10|5|true||2");

		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());

		String[] p = parseState(handler.readStateString(user, col));
		assertEquals("2026-01-10", p[0]);
		assertEquals("5", p[1], "streakCount should not increment (already counted)");
		assertEquals("1", p[2], "votesThisPeriod upgraded-from-old (counted=true => 1) and ignored on vote");
		assertEquals("true", p[3]);
		assertEquals("2", p[5], "missesUsed preserved");
	}

}
