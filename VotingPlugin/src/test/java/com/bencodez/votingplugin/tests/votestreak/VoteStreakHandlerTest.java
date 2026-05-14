package com.bencodez.votingplugin.tests.votestreak;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.RETURNS_DEEP_STUBS;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
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

	private static YamlConfiguration rootWithLegacyVoteStreaks() {
		YamlConfiguration root = new YamlConfiguration();

		ConfigurationSection legacy = root.createSection("VoteStreak");

		ConfigurationSection week = legacy.createSection("Week");
		week.set("Enabled", true);

		ConfigurationSection enabledOneTime = week.createSection("2");
		enabledOneTime.set("Enabled", true);
		enabledOneTime.createSection("Rewards").createSection("Messages").set("Player",
				"&aYou voted for %Streak% %Type%'s in a row!");

		ConfigurationSection disabledOneTime = week.createSection("3");
		disabledOneTime.set("Enabled", false);
		disabledOneTime.createSection("Rewards").createSection("Messages").set("Player", "&cShould not migrate");

		ConfigurationSection enabledRecurring = week.createSection("-4");
		enabledRecurring.set("Enabled", true);
		enabledRecurring.createSection("Rewards").createSection("Messages").set("Player", "&aRecurring migrated");

		return root;
	}

	private static YamlConfiguration rootWithDisabledLegacyWeek() {
		YamlConfiguration root = new YamlConfiguration();

		ConfigurationSection legacy = root.createSection("VoteStreak");
		ConfigurationSection week = legacy.createSection("Week");
		week.set("Enabled", false);

		ConfigurationSection enabledEntry = week.createSection("2");
		enabledEntry.set("Enabled", true);
		enabledEntry.createSection("Rewards").createSection("Messages").set("Player",
				"&cShould not migrate because Week.Enabled is false");

		return root;
	}

	private static YamlConfiguration rootWithLegacyListRewards() {
		YamlConfiguration root = new YamlConfiguration();

		ConfigurationSection legacy = root.createSection("VoteStreak");
		ConfigurationSection day = legacy.createSection("Day");
		day.set("Enabled", true);

		ConfigurationSection enabledOneTime = day.createSection("3");
		enabledOneTime.set("Enabled", true);
		enabledOneTime.set("Rewards", Arrays.asList("VoteSteak_3_Rewards_Online", "VoteSteak_3_Rewards_Offline"));

		return root;
	}

	/**
	 * periodKey|streakCount|votesThisPeriod|countedThisPeriod|missWindowStartKey|missesUsed
	 *
	 * @param raw raw streak state
	 * @return parsed state fields
	 */
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

	private void setSpecialRewardsRoot(FileConfiguration root) {
		when(plugin.getSpecialRewardsConfig().getData()).thenReturn(root);
	}

	private boolean shouldReward(VoteStreakDefinition def, int streakCount) throws Exception {
		Method m = VoteStreakHandler.class.getDeclaredMethod("shouldReward", VoteStreakDefinition.class, int.class);
		m.setAccessible(true);
		return (boolean) m.invoke(handler, def, streakCount);
	}

	@Test
	void configLoader_loadsDefinition_andGetDefinitionWorks() {
		MemoryConfiguration root = rootWithOneStreak("DailyStreak", "DAILY", true, 5, 2, 0, 0);
		loadFromRoot(root);

		VoteStreakDefinition def = handler.getById().get("dailystreak");
		assertNotNull(def);

		assertEquals("DailyStreak", def.getId());
		assertEquals(VoteStreakType.DAILY, def.getType());
		assertTrue(def.isEnabled());
		assertEquals(5, def.getRequiredAmount());
		assertEquals(2, def.getVotesRequired());

		assertEquals(def, handler.getDefinition("DailyStreak"));
		assertEquals(def, handler.getDefinition("dailystreak"));
		assertEquals(def, handler.getDefinition("DAILYSTREAK"));
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
		assertEquals("1", p[2], "votesThisPeriod should not increment once already countedThisPeriod=true");
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
		assertEquals("5", p[1], "streakCount should not increment because already counted");
		assertEquals("1", p[2], "votesThisPeriod upgraded from old counted=true state and ignored on vote");
		assertEquals("true", p[3]);
		assertEquals("2", p[5], "missesUsed preserved");
	}

	@Test
	void shouldReward_respectsRecurringFlag() throws Exception {
		VoteStreakDefinition oneTime = new VoteStreakDefinition("oneTime", VoteStreakType.DAILY, true, 3, 1, 0, 0,
				false);
		VoteStreakDefinition recurring = new VoteStreakDefinition("recurring", VoteStreakType.DAILY, true, 3, 1, 0, 0,
				true);

		assertFalse(shouldReward(oneTime, 2));
		assertTrue(shouldReward(oneTime, 3));
		assertFalse(shouldReward(oneTime, 6));

		assertFalse(shouldReward(recurring, 2));
		assertTrue(shouldReward(recurring, 3));
		assertTrue(shouldReward(recurring, 6));
	}

	@Test
	void migrateLegacyConfigManually_migratesOnlyEnabledLegacyEntries() {
		YamlConfiguration root = rootWithLegacyVoteStreaks();
		setSpecialRewardsRoot(root);

		int migrated = handler.migrateLegacyConfigManually();

		assertEquals(2, migrated);

		ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
		assertNotNull(voteStreaks);

		ConfigurationSection oneTime = voteStreaks.getConfigurationSection("LegacyWEEKLY2OneTime");
		assertNotNull(oneTime);
		assertEquals("WEEKLY", oneTime.getString("Type"));
		assertTrue(oneTime.getBoolean("Enabled"));
		assertFalse(oneTime.getBoolean("Recurring"));
		assertEquals(2, oneTime.getInt("Requirements.Amount"));
		assertEquals(1, oneTime.getInt("Requirements.VotesRequired"));
		assertEquals(0, oneTime.getInt("AllowMissedAmount"));
		assertEquals(0, oneTime.getInt("AllowMissedPeriod"));

		ConfigurationSection recurring = voteStreaks.getConfigurationSection("LegacyWEEKLY4Recurring");
		assertNotNull(recurring);
		assertEquals("WEEKLY", recurring.getString("Type"));
		assertTrue(recurring.getBoolean("Enabled"));
		assertTrue(recurring.getBoolean("Recurring"));
		assertEquals(4, recurring.getInt("Requirements.Amount"));
		assertEquals(1, recurring.getInt("Requirements.VotesRequired"));

		assertNull(voteStreaks.getConfigurationSection("LegacyWEEKLY3OneTime"),
				"disabled legacy streak entries should not migrate");

		assertFalse(root.getBoolean("VoteStreak.Week.2.Enabled"),
				"migrated legacy entries should be disabled after migration");
		assertFalse(root.getBoolean("VoteStreak.Week.-4.Enabled"),
				"migrated recurring legacy entries should be disabled after migration");
		assertFalse(root.getBoolean("VoteStreak.Week.3.Enabled"), "disabled legacy entries should remain disabled");
	}

	@Test
	void migrateLegacyConfigManually_doesNotMigrateWhenLegacyTypeIsDisabled() {
		YamlConfiguration root = rootWithDisabledLegacyWeek();
		setSpecialRewardsRoot(root);

		int migrated = handler.migrateLegacyConfigManually();

		assertEquals(0, migrated);

		ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
		assertNotNull(voteStreaks);
		assertTrue(voteStreaks.getKeys(false).isEmpty(),
				"disabled legacy type sections should not migrate any child entries");

		assertTrue(root.getBoolean("VoteStreak.Week.2.Enabled"),
				"entry should not be changed when parent legacy type is disabled");
	}

	@Test
	void migrateLegacyConfigManually_copiesRewardsWithoutFlattenedDuplicatePaths() {
		YamlConfiguration root = rootWithLegacyVoteStreaks();
		setSpecialRewardsRoot(root);

		handler.migrateLegacyConfigManually();

		ConfigurationSection migrated = root.getConfigurationSection("VoteStreaks.LegacyWEEKLY2OneTime");
		assertNotNull(migrated);

		assertEquals("&aYou voted for %Streak% %Type%'s in a row!", migrated.getString("Rewards.Messages.Player"));

		ConfigurationSection rewards = migrated.getConfigurationSection("Rewards");
		assertNotNull(rewards);
		assertTrue(rewards.getKeys(false).contains("Messages"));
		assertFalse(rewards.getKeys(false).contains("Messages.Player"),
				"Rewards should not contain flattened Messages.Player key");
	}

	@Test
	void migrateLegacyConfigManually_copiesListBasedRewards() {
		YamlConfiguration root = rootWithLegacyListRewards();
		setSpecialRewardsRoot(root);

		handler.migrateLegacyConfigManually();

		ConfigurationSection migrated = root.getConfigurationSection("VoteStreaks.LegacyDAILY3OneTime");
		assertNotNull(migrated);

		List<String> rewards = migrated.getStringList("Rewards");
		assertEquals(Arrays.asList("VoteSteak_3_Rewards_Online", "VoteSteak_3_Rewards_Offline"), rewards);
		assertFalse(migrated.isConfigurationSection("Rewards"),
				"list-based reward references should remain list-based");
	}

}
