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

	private static MemoryConfiguration rootWithThreeStreaks() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");

		addStreak(voteStreaks, "Daily3", "DAILY", true, 3, 1, 1, 7);
		addStreak(voteStreaks, "Daily7", "DAILY", true, 7, 1, 1, 7);
		addStreak(voteStreaks, "Weekly2", "WEEKLY", true, 2, 1, 0, 0);

		return root;
	}

	private static void addStreak(ConfigurationSection voteStreaks, String id, String type, boolean enabled,
			int intervalAmount, int votesRequired, int allowMissedAmount, int allowMissedPeriod) {
		ConfigurationSection def = voteStreaks.createSection(id);
		def.set("Type", type);
		def.set("Enabled", enabled);

		ConfigurationSection req = def.createSection("Requirements");
		req.set("Amount", intervalAmount);
		req.set("VotesRequired", votesRequired);

		def.set("AllowMissedAmount", allowMissedAmount);
		def.set("AllowMissedPeriod", allowMissedPeriod);
	}

	private static ConfigurationSection addProgressGroup(ConfigurationSection voteStreaks, String groupId, String type,
			int votesRequired, int allowMissedAmount, int allowMissedPeriod) {
		ConfigurationSection group = voteStreaks.createSection("ProgressGroups").createSection(groupId);
		group.set("Type", type);
		group.set("VotesRequired", votesRequired);
		group.set("AllowMissedAmount", allowMissedAmount);
		group.set("AllowMissedPeriod", allowMissedPeriod);
		group.createSection("Milestones");
		return group;
	}

	private static void addProgressGroupMilestone(ConfigurationSection group, String id, int amount, boolean enabled,
			boolean recurring) {
		ConfigurationSection milestone = group.getConfigurationSection("Milestones").createSection(id);
		milestone.set("Enabled", enabled);
		milestone.set("Amount", amount);
		milestone.set("Recurring", recurring);
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

	private boolean shouldGiveProgressGroupLostRewards(List<VoteStreakDefinition> definitions,
			VoteStreakDefinition progressDefinition, int lostStreakCount) throws Exception {
		Method m = VoteStreakHandler.class.getDeclaredMethod("shouldGiveProgressGroupLostRewards", List.class,
				VoteStreakDefinition.class, int.class);
		m.setAccessible(true);
		return (boolean) m.invoke(handler, definitions, progressDefinition, lostStreakCount);
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
		assertEquals("", def.getProgressGroup());

		assertEquals(def, handler.getDefinition("DailyStreak"));
		assertEquals(def, handler.getDefinition("dailystreak"));
		assertEquals(def, handler.getDefinition("DAILYSTREAK"));
	}

	@Test
	void configLoader_loadsProgressGroup_andUsesSharedColumnName() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 2, 1, 7);
		addProgressGroupMilestone(group, "Daily3", 3, true, false);

		loadFromRoot(root);

		VoteStreakDefinition def = handler.getDefinition("Daily3");
		assertNotNull(def);
		assertEquals("continuousstreak", def.getProgressGroup());
		assertEquals(2, def.getVotesRequired());
		assertEquals(1, def.getAllowMissedAmount());
		assertEquals(7, def.getAllowMissedPeriod());
		assertFalse(def.isRecurring());
		assertEquals("VoteStreakGroup_DAILY_continuousstreak", handler.getColumnName(def));
		assertEquals("VoteStreaks.ProgressGroups.continuousstreak.Milestones.Daily3.Rewards", def.getRewardPath());
	}

	@Test
	void configLoader_skipsProgressGroupThatDuplicatesFlatStreakId() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		addStreak(voteStreaks, "ContinuousStreak", "DAILY", true, 3, 1, 0, 0);
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		addProgressGroupMilestone(group, "SharedGroupTestA", 1, true, true);

		loadFromRoot(root);

		assertNotNull(handler.getDefinition("ContinuousStreak"));
		assertNull(handler.getDefinition("SharedGroupTestA"));
		assertTrue(handler.getProgressGroups().isEmpty());
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
	void processVote_sharedProgressGroupCountsOneVoteOnceForMultipleDefinitions() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 1, 7);
		addProgressGroupMilestone(group, "Daily99", 99, true, false);
		addProgressGroupMilestone(group, "Daily100", 100, true, false);
		loadFromRoot(root);

		VoteStreakDefinition daily99 = handler.getDefinition("Daily99");
		VoteStreakDefinition daily100 = handler.getDefinition("Daily100");
		assertNotNull(daily99);
		assertNotNull(daily100);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);

		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());

		String groupCol = "VoteStreakGroup_DAILY_continuousstreak";
		String[] p = parseState(backing.get(groupCol));
		assertEquals("1", p[1], "shared progress should increment once for one vote");
		assertFalse(backing.containsKey("VoteStreak_Daily99"));
		assertFalse(backing.containsKey("VoteStreak_Daily100"));
		assertEquals(groupCol, handler.getColumnName(daily99));
		assertEquals(groupCol, handler.getColumnName(daily100));
	}

	@Test
	void processVote_sharedProgressGroupDoesNotRefireRewardedOneTimeMilestone() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 1, 7);
		addProgressGroupMilestone(group, "Daily3", 3, true, false);
		loadFromRoot(root);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		backing.put("VoteStreakGroup_DAILY_continuousstreak", "2026-01-10|2|0|false||0|daily3");

		handler.processVote(user, System.currentTimeMillis(), UUID.randomUUID());

		String[] p = parseState(backing.get("VoteStreakGroup_DAILY_continuousstreak"));
		assertEquals("3", p[1], "shared progress should still advance");
		assertEquals("daily3", p[6], "reward history should be preserved");
	}

	@Test
	void lostRewards_progressGroupRunsAfterOneSuccessfulPeriodByDefault() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		group.createSection("LostRewards").createSection("Messages").set("Player", "&cStreak lost");
		addProgressGroupMilestone(group, "Daily3", 3, true, false);

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition progressDefinition = handler.getDefinition("Daily3");
		assertNotNull(progressDefinition);

		assertTrue(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 1),
				"LostRewards should run after one successful period when milestone requirement is disabled");
	}

	@Test
	void lostRewards_progressGroupRequiresReachedMilestoneWhenConfigured() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		group.set("RequireMilestoneForLostRewards", true);
		group.createSection("LostRewards").createSection("Messages").set("Player", "&cStreak lost");
		addProgressGroupMilestone(group, "Daily3", 3, true, false);
		addProgressGroupMilestone(group, "Daily7", 7, true, false);

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition progressDefinition = handler.getDefinition("Daily3");
		assertNotNull(progressDefinition);

		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 2),
				"LostRewards should not run before an enabled milestone is reached");
		assertTrue(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 3),
				"LostRewards should run once the first enabled milestone is reached");
		assertTrue(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 8),
				"LostRewards should run when any enabled milestone has been reached");
	}

	@Test
	void lostRewards_progressGroupIgnoresDisabledMilestones() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		group.set("RequireMilestoneForLostRewards", true);
		group.createSection("LostRewards").createSection("Messages").set("Player", "&cStreak lost");
		addProgressGroupMilestone(group, "Daily3", 3, false, false);
		addProgressGroupMilestone(group, "Daily7", 7, true, false);

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition progressDefinition = handler.getDefinition("Daily3");
		assertNotNull(progressDefinition);

		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 3),
				"Disabled milestones must not satisfy RequireMilestoneForLostRewards");
		assertTrue(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 7),
				"An enabled milestone should satisfy RequireMilestoneForLostRewards");
	}

	@Test
	void lostRewards_progressGroupDoesNotRunWithoutConfiguredRewards() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		addProgressGroupMilestone(group, "Daily3", 3, true, false);

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition progressDefinition = handler.getDefinition("Daily3");
		assertNotNull(progressDefinition);

		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 3),
				"LostRewards should not run when the group has no LostRewards section");
	}

	@Test
	void lostRewards_regularVoteStreakIsNeverEligible() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		addStreak(voteStreaks, "Daily3", "DAILY", true, 3, 1, 0, 0);
		voteStreaks.getConfigurationSection("Daily3").createSection("LostRewards").createSection("Messages")
				.set("Player", "&cShould never run");

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition definition = handler.getDefinition("Daily3");
		assertNotNull(definition);

		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), definition, 3),
				"Regular VoteStreak definitions must never use LostRewards");
	}

	@Test
	void lostRewards_progressGroupRequiresPositiveStreakCount() throws Exception {
		YamlConfiguration root = new YamlConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		group.createSection("LostRewards").createSection("Messages").set("Player", "&cStreak lost");
		addProgressGroupMilestone(group, "Daily3", 3, true, false);

		setSpecialRewardsRoot(root);
		loadFromRoot(root);

		VoteStreakDefinition progressDefinition = handler.getDefinition("Daily3");
		assertNotNull(progressDefinition);

		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, 0));
		assertFalse(shouldGiveProgressGroupLostRewards(handler.getDefinitions(), progressDefinition, -1));
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
	void resetVoteStreaks_resetsAllConfiguredDefinitions() {
		loadFromRoot(rootWithThreeStreaks());

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		for (VoteStreakDefinition def : handler.getDefinitions()) {
			backing.put(handler.getColumnName(def), "2026-01-10|5|1|true||0");
		}

		int reset = handler.resetVoteStreaks(user);

		assertEquals(3, reset);
		for (VoteStreakDefinition def : handler.getDefinitions()) {
			assertEquals("", backing.get(handler.getColumnName(def)));
		}
	}

	@Test
	void resetVoteStreaks_byTypeResetsOnlyMatchingDefinitions() {
		loadFromRoot(rootWithThreeStreaks());

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		VoteStreakDefinition daily3 = handler.getDefinition("Daily3");
		VoteStreakDefinition daily7 = handler.getDefinition("Daily7");
		VoteStreakDefinition weekly2 = handler.getDefinition("Weekly2");

		backing.put(handler.getColumnName(daily3), "2026-01-10|3|1|true||0");
		backing.put(handler.getColumnName(daily7), "2026-01-10|7|1|true||0");
		backing.put(handler.getColumnName(weekly2), "2026-W02|2|1|true||0");

		int reset = handler.resetVoteStreaks(user, VoteStreakType.DAILY);

		assertEquals(2, reset);
		assertEquals("", backing.get(handler.getColumnName(daily3)));
		assertEquals("", backing.get(handler.getColumnName(daily7)));
		assertEquals("2026-W02|2|1|true||0", backing.get(handler.getColumnName(weekly2)));
	}

	@Test
	void resetVoteStreak_byIdResetsOnlyMatchingDefinition() {
		loadFromRoot(rootWithThreeStreaks());

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		VoteStreakDefinition daily3 = handler.getDefinition("Daily3");
		VoteStreakDefinition weekly2 = handler.getDefinition("Weekly2");

		backing.put(handler.getColumnName(daily3), "2026-01-10|3|1|true||0");
		backing.put(handler.getColumnName(weekly2), "2026-W02|2|1|true||0");

		assertTrue(handler.resetVoteStreak(user, "daily3"));
		assertEquals("", backing.get(handler.getColumnName(daily3)));
		assertEquals("2026-W02|2|1|true||0", backing.get(handler.getColumnName(weekly2)));
	}

	@Test
	void resetVoteStreak_byProgressGroupResetsSharedState() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		addProgressGroupMilestone(group, "SharedGroupTestA", 1, true, true);
		addProgressGroupMilestone(group, "SharedGroupTestB", 1, true, true);
		addStreak(voteStreaks, "UngroupedControl", "DAILY", true, 1, 1, 0, 0);
		loadFromRoot(root);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		String groupCol = "VoteStreakGroup_DAILY_continuousstreak";
		String controlCol = "VoteStreak_UngroupedControl";
		backing.put(groupCol, "2026-01-10|1|1|true||0");
		backing.put(controlCol, "2026-01-10|1|1|true||0");

		assertTrue(handler.resetVoteStreak(user, "continuousstreak"));

		assertEquals("", backing.get(groupCol));
		assertEquals("2026-01-10|1|1|true||0", backing.get(controlCol));
	}

	@Test
	void resetVoteStreak_byProgressGroupMilestoneIdDoesNotResetSharedState() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		addProgressGroupMilestone(group, "SharedGroupTestA", 1, true, true);
		loadFromRoot(root);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		String groupCol = "VoteStreakGroup_DAILY_continuousstreak";
		backing.put(groupCol, "2026-01-10|1|1|true||0");

		assertFalse(handler.resetVoteStreak(user, "SharedGroupTestA"));

		assertEquals("2026-01-10|1|1|true||0", backing.get(groupCol));
	}

	@Test
	void resetVoteStreaks_countsSharedProgressGroupOnce() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection voteStreaks = root.createSection("VoteStreaks");
		ConfigurationSection group = addProgressGroup(voteStreaks, "continuousstreak", "DAILY", 1, 0, 0);
		addProgressGroupMilestone(group, "SharedGroupTestA", 1, true, true);
		addProgressGroupMilestone(group, "SharedGroupTestB", 1, true, true);
		addStreak(voteStreaks, "UngroupedControl", "DAILY", true, 1, 1, 0, 0);
		loadFromRoot(root);

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);
		backing.put("VoteStreakGroup_DAILY_continuousstreak", "2026-01-10|1|1|true||0");
		backing.put("VoteStreak_UngroupedControl", "2026-01-10|1|1|true||0");

		assertEquals(2, handler.resetVoteStreaks(user));
	}

	@Test
	void resetVoteStreak_unknownIdReturnsFalse() {
		loadFromRoot(rootWithThreeStreaks());

		Map<String, String> backing = new HashMap<>();
		VotingPluginUser user = mapBackedUser(UUID.randomUUID(), "Ben", backing);

		assertFalse(handler.resetVoteStreak(user, "missing"));
		assertTrue(backing.isEmpty());
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