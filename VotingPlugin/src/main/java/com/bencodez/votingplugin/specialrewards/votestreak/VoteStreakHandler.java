package com.bencodez.votingplugin.specialrewards.votestreak;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.temporal.ChronoUnit;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.TimeChecker;
import com.bencodez.advancedcore.api.user.usercache.keys.UserDataKeyString;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;
import com.bencodez.votingplugin.events.SpecialRewardType;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

public class VoteStreakHandler {

	private final VotingPluginMain plugin;

	@Getter
	private final Map<String, VoteStreakDefinition> byId = new LinkedHashMap<>();
	private final Map<String, VoteStreakDefinition> byProgressGroup = new LinkedHashMap<>();
	private final List<VoteStreakDefinition> ordered = new ArrayList<>();

	public VoteStreakHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Reload streak definitions from config.
	 */
	public void reload() {
		byId.clear();
		byProgressGroup.clear();
		ordered.clear();

		// VoteStreak definitions live in specialrewards.yml now (Option B)
		ConfigurationSection root = plugin.getSpecialRewardsConfig().getData();
		new VoteStreakConfigLoader().load(root);
	}

	public List<VoteStreakDefinition> getDefinitions() {
		return Collections.unmodifiableList(ordered);
	}

	public VoteStreakDefinition getDefinition(String id) {
		if (id == null) {
			return null;
		}
		return byId.get(id.toLowerCase(Locale.ROOT));
	}

	public Set<String> getProgressGroups() {
		Set<String> groups = new LinkedHashSet<>();
		for (VoteStreakDefinition def : byProgressGroup.values()) {
			groups.add(def.getProgressGroup());
		}
		return Collections.unmodifiableSet(groups);
	}

	/**
	 * Call this whenever a vote happens
	 *
	 * @param user           voting plugin user
	 * @param voteTimeMillis vote time
	 * @param voteUUID       vote unique id
	 */
	public void processVote(VotingPluginUser user, long voteTimeMillis, UUID voteUUID) {
		if (user == null) {
			plugin.extraDebug("[VoteStreak] processVote: user is null, returning");
			return;
		}
		if (ordered.isEmpty()) {
			plugin.extraDebug("[VoteStreak] processVote: ordered is empty (no definitions loaded), returning");
			return;
		}

		plugin.extraDebug("[VoteStreak] processVote: user=" + safeUser(user) + " timeMillis=" + voteTimeMillis
				+ " defs=" + ordered.size());

		Map<String, List<VoteStreakDefinition>> grouped = new LinkedHashMap<>();

		for (VoteStreakDefinition def : ordered) {
			if (!def.isEnabled()) {
				plugin.extraDebug(
						"[VoteStreak] processVote: skip disabled def id=" + def.getId() + " idKey=" + def.getId());
				continue;
			}

			grouped.computeIfAbsent(getColumnName(def), key -> new ArrayList<>()).add(def);
		}

		for (List<VoteStreakDefinition> defs : grouped.values()) {
			try {
				processVoteForDefinitions(user, defs, voteTimeMillis, voteUUID);
			} catch (Exception e) {
				plugin.getLogger().warning(
						"VoteStreak processing failed for '" + defs.get(0).getId() + "': " + e.getMessage());
				plugin.debug(e);
			}
		}
	}

	private int getLegacyStreakProgress(VotingPluginUser user, VoteStreakType type) {
		switch (type) {
		case DAILY:
			return Math.max(0, user.getDayVoteStreak());
		case WEEKLY:
			return Math.max(0, user.getWeekVoteStreak());
		case MONTHLY:
			return Math.max(0, user.getMonthVoteStreak());
		default:
			return 0;
		}
	}

	private void migrateLegacyProgressIfNeeded(VotingPluginUser user, VoteStreakDefinition def, StreakState state,
			String currentPeriodKey) {
		if (state.periodKey != null && !state.periodKey.isEmpty()) {
			return;
		}
		int legacyProgress = getLegacyStreakProgress(user, def.getType());
		if (legacyProgress <= 0) {
			return;
		}
		state.periodKey = currentPeriodKey;
		state.streakCount = legacyProgress;
		state.votesThisPeriod = 0;
		state.countedThisPeriod = false;
		state.missWindowStartKey = "";
		state.missesUsed = 0;
		plugin.extraDebug("[VoteStreak] migrated legacy progress for " + def.getId() + ": " + legacyProgress);
	}

	public int migrateLegacyConfigManually() {
		ConfigurationSection root = plugin.getSpecialRewardsConfig().getData();
		if (root == null) {
			return 0;
		}
		return new VoteStreakConfigLoader().loadLegacy(root) ? ordered.size() : 0;
	}

	private void processVoteForDefinitions(VotingPluginUser user, List<VoteStreakDefinition> defs, long voteTimeMillis,
			UUID voteUUID) {
		if (defs == null || defs.isEmpty()) {
			return;
		}

		VoteStreakDefinition progressDef = defs.get(0);
		final boolean sharedProgress = progressDef.getProgressGroup() != null && !progressDef.getProgressGroup().isEmpty();
		final String col = getColumnName(progressDef);
		final String rawBefore = readStateString(user, col);
		StreakState state = StreakState.deserialize(rawBefore);

		final String currentPeriodKey = periodKey(progressDef.getType(), voteTimeMillis);
		migrateLegacyProgressIfNeeded(user, progressDef, state, currentPeriodKey);

		plugin.extraDebug("[VoteStreak] def=" + progressDef.getId() + " idKey=" + progressDef.getId() + " type="
				+ progressDef.getType() + " col=" + col + " progressGroup=" + progressDef.getProgressGroup()
				+ " period=" + currentPeriodKey + " votesReq=" + progressDef.getVotesRequired() + " interval="
				+ progressDef.getRequiredAmount() + " rawBefore='" + rawBefore + "' stateBefore={periodKey="
				+ state.periodKey + ",streakCount=" + state.streakCount + ",votesThisPeriod=" + state.votesThisPeriod
				+ ",countedThisPeriod=" + state.countedThisPeriod + ",missesUsed=" + state.missesUsed
				+ ",missWindowStartKey=" + state.missWindowStartKey + "}");

		// First time init
		if (state.periodKey == null || state.periodKey.isEmpty()) {
			plugin.extraDebug("[VoteStreak] init state (no prior periodKey)");
			state.periodKey = currentPeriodKey;
			state.countedThisPeriod = false;
			state.votesThisPeriod = 0;
			state.missWindowStartKey = "";
			state.missesUsed = 0;
		} else if (!state.periodKey.equals(currentPeriodKey)) {
			plugin.extraDebug("[VoteStreak] period advanced from " + state.periodKey + " -> " + currentPeriodKey);
			advancePeriodsAndApplyMisses(user, defs, state, voteTimeMillis, currentPeriodKey, voteUUID);
			plugin.extraDebug("[VoteStreak] after advance state={periodKey=" + state.periodKey + ",streakCount="
					+ state.streakCount + ",votesThisPeriod=" + state.votesThisPeriod + ",countedThisPeriod="
					+ state.countedThisPeriod + ",missesUsed=" + state.missesUsed + ",missWindowStartKey="
					+ state.missWindowStartKey + "}");
		}

		// Count votes within the period until threshold reached, then mark period
		// satisfied once.
		if (!state.countedThisPeriod) {
			state.votesThisPeriod++;

			int votesRequired = Math.max(1, progressDef.getVotesRequired());
			plugin.extraDebug("[VoteStreak] vote counted in period: votesThisPeriod=" + state.votesThisPeriod + "/"
					+ votesRequired);

			if (state.votesThisPeriod >= votesRequired) {
				state.countedThisPeriod = true;
				state.streakCount++;

				for (VoteStreakDefinition def : defs) {
					int interval = Math.max(1, def.getRequiredAmount());
					boolean shouldReward = shouldReward(def, state, sharedProgress);

					plugin.extraDebug("[VoteStreak] period satisfied: def=" + def.getId() + " streakCount="
							+ state.streakCount + " interval=" + interval + " shouldReward=" + shouldReward);

					if (shouldReward) {
						plugin.extraDebug("[VoteStreak] giving rewards for idKey=" + def.getId());
						giveRewards(user, def, voteUUID, state.streakCount);
						if (sharedProgress && !def.isRecurring()) {
							state.markRewarded(def);
						}
					}
				}
			}
		} else {
			plugin.extraDebug("[VoteStreak] already satisfied this period; ignoring vote. votesThisPeriod="
					+ state.votesThisPeriod);
		}

		// Persist + verify
		final String rawAfter = state.serialize();
		plugin.extraDebug("[VoteStreak] writing col=" + col + " rawAfter='" + rawAfter + "'");
		writeStateString(user, col, rawAfter);

		final String readBack = readStateString(user, col);
		plugin.extraDebug("[VoteStreak] read-back col=" + col + " raw='" + readBack + "' (writtenLen="
				+ rawAfter.length() + " readLen=" + readBack.length() + ")");
	}

	private boolean shouldReward(VoteStreakDefinition def, StreakState state, boolean sharedProgress) {
		if (!shouldReward(def, state.streakCount)) {
			return false;
		}
		return !sharedProgress || def.isRecurring() || !state.hasRewarded(def);
	}

	private boolean shouldReward(VoteStreakDefinition def, int streakCount) {
		if (streakCount <= 0) {
			return false;
		}

		int interval = Math.max(1, def.getRequiredAmount());
		if (def.isRecurring()) {
			return (streakCount % interval) == 0;
		}
		return streakCount == interval;
	}

	/**
	 * Advances the streak to the current period and applies missed periods.
	 *
	 * @param user voting plugin user
	 * @param definitions definitions sharing the same streak state
	 * @param state current streak state
	 * @param nowMillis current time in milliseconds
	 * @param currentPeriodKey current period key
	 * @param voteUUID unique ID associated with the vote
	 */
	private void advancePeriodsAndApplyMisses(VotingPluginUser user, List<VoteStreakDefinition> definitions,
			StreakState state, long nowMillis, String currentPeriodKey, UUID voteUUID) {
		VoteStreakDefinition progressDefinition = definitions.get(0);

		if (!state.countedThisPeriod) {
			plugin.extraDebug("[VoteStreak] advance: previous period " + state.periodKey
					+ " NOT satisfied -> miss");
			recordMiss(progressDefinition, state, nowMillis);
		}

		int skipped = periodsBetween(progressDefinition.getType(), state.periodKey, currentPeriodKey);
		plugin.extraDebug("[VoteStreak] advance: skipped=" + skipped + " between " + state.periodKey
				+ " -> " + currentPeriodKey);

		if (skipped > 1) {
			for (int i = 0; i < skipped - 1; i++) {
				plugin.extraDebug("[VoteStreak] advance: recording intermediate missed period "
						+ (i + 1) + "/" + (skipped - 1));
				recordMiss(progressDefinition, state, nowMillis);
			}
		}

		state.periodKey = currentPeriodKey;
		state.countedThisPeriod = false;
		state.votesThisPeriod = 0;

		boolean streakLost = false;

		if (progressDefinition.getAllowMissedAmount() <= 0) {
			if (state.missesUsed > 0) {
				plugin.extraDebug("[VoteStreak] advance: no misses allowed and missesUsed="
						+ state.missesUsed + " -> reset");
				streakLost = true;
			}
		} else if (state.missesUsed > progressDefinition.getAllowMissedAmount()) {
			plugin.extraDebug("[VoteStreak] advance: missesUsed=" + state.missesUsed
					+ " > allowMissedAmount=" + progressDefinition.getAllowMissedAmount() + " -> reset");
			streakLost = true;
		}

		if (streakLost) {
			int lostStreakCount = state.streakCount;
			if (shouldGiveProgressGroupLostRewards(definitions, progressDefinition, lostStreakCount)) {
				giveProgressGroupLostRewards(user, progressDefinition, lostStreakCount);
			}
			resetStreakAndMissTracking(state);
		}
	}

	/**
	 * Checks whether lost rewards should run for a progress group.
	 *
	 * @param definitions progress group milestone definitions
	 * @param progressDefinition definition owning the shared progress
	 * @param lostStreakCount streak count before reset
	 * @return true if the progress group lost rewards should run
	 */
	private boolean shouldGiveProgressGroupLostRewards(List<VoteStreakDefinition> definitions,
			VoteStreakDefinition progressDefinition, int lostStreakCount) {
		String progressGroup = progressDefinition.getProgressGroup();
		if (lostStreakCount <= 0 || progressGroup == null || progressGroup.isEmpty()) {
			return false;
		}

		String groupPath = "VoteStreaks.ProgressGroups." + progressGroup;
		ConfigurationSection root = plugin.getSpecialRewardsConfig().getData();
		if (!root.isConfigurationSection(groupPath + ".LostRewards")) {
			return false;
		}

		boolean requireMilestone = root.getBoolean(groupPath + ".RequireMilestoneForLostRewards", false);
		return !requireMilestone || hasReachedEnabledMilestone(definitions, lostStreakCount);
	}

	/**
	 * Checks whether the streak reached at least one enabled milestone.
	 *
	 * @param definitions progress group milestone definitions
	 * @param streakCount streak count before reset
	 * @return true if an enabled milestone was reached
	 */
	private boolean hasReachedEnabledMilestone(List<VoteStreakDefinition> definitions, int streakCount) {
		for (VoteStreakDefinition definition : definitions) {
			if (definition.isEnabled() && streakCount >= definition.getRequiredAmount()) {
				return true;
			}
		}
		return false;
	}

	private void recordMiss(VoteStreakDefinition def, StreakState state, long nowMillis) {
		if (def.getAllowMissedPeriod() <= 0) {
			if (state.missWindowStartKey == null || state.missWindowStartKey.isEmpty()) {
				state.missWindowStartKey = periodKey(def.getType(), nowMillis);
				state.missesUsed = 1;
				plugin.extraDebug("[VoteStreak] recordMiss: start window=" + state.missWindowStartKey
						+ " missesUsed=1 (no window)");
			} else {
				state.missesUsed++;
				plugin.extraDebug("[VoteStreak] recordMiss: missesUsed=" + state.missesUsed + " (no window)");
			}
			return;
		}

		String nowKey = periodKey(def.getType(), nowMillis);

		if (state.missWindowStartKey == null || state.missWindowStartKey.isEmpty()) {
			state.missWindowStartKey = nowKey;
			state.missesUsed = 1;
			plugin.extraDebug("[VoteStreak] recordMiss: start window=" + nowKey + " missesUsed=1");
			return;
		}

		int windowSpan = periodsBetween(def.getType(), state.missWindowStartKey, nowKey);

		if (windowSpan >= def.getAllowMissedPeriod()) {
			plugin.extraDebug("[VoteStreak] recordMiss: window expired span=" + windowSpan + " >= allowMissedPeriod="
					+ def.getAllowMissedPeriod() + " -> reset window to " + nowKey);
			state.missWindowStartKey = nowKey;
			state.missesUsed = 1;
		} else {
			state.missesUsed++;
			plugin.extraDebug(
					"[VoteStreak] recordMiss: within window span=" + windowSpan + " missesUsed=" + state.missesUsed);
		}
	}

	private void resetStreakAndMissTracking(StreakState state) {
		plugin.extraDebug("[VoteStreak] resetStreakAndMissTracking: reset streakCount/misses/votesThisPeriod");
		state.streakCount = 0;
		state.missesUsed = 0;
		state.missWindowStartKey = "";
		state.votesThisPeriod = 0;
	}

	private LocalDate getPluginDateFromMillis(long millis) {
		TimeChecker tc = plugin.getTimeChecker();

		LocalDateTime ldt = tryInvokeLocalDateTime(tc, "getTime", millis);
		if (ldt != null) {
			return ldt.toLocalDate();
		}
		ldt = tryInvokeLocalDateTime(tc, "getTimeFromMillis", millis);
		if (ldt != null) {
			return ldt.toLocalDate();
		}
		ldt = tryInvokeLocalDateTime(tc, "getTimeAt", millis);
		if (ldt != null) {
			return ldt.toLocalDate();
		}

		LocalDateTime pluginNow = tc.getTime();
		long nowMillis = System.currentTimeMillis();
		long deltaMillis = millis - nowMillis;
		return pluginNow.plusNanos(deltaMillis * 1_000_000L).toLocalDate();
	}

	private LocalDateTime tryInvokeLocalDateTime(Object target, String method, long millis) {
		try {
			Method m = target.getClass().getMethod(method, long.class);
			Object out = m.invoke(target, millis);
			if (out instanceof LocalDateTime) {
				return (LocalDateTime) out;
			}
		} catch (Exception ignored) {
		}
		return null;
	}

	private String periodKey(VoteStreakType type, long millis) {
		switch (type) {
		case DAILY: {
			LocalDate d = getPluginDateFromMillis(millis);
			return d.toString();
		}
		case WEEKLY: {
			LocalDate d = getPluginDateFromMillis(millis);
			WeekFields wf = WeekFields.ISO;
			int week = d.get(wf.weekOfWeekBasedYear());
			int year = d.get(wf.weekBasedYear());
			return year + "-W" + String.format(Locale.ROOT, "%02d", week);
		}
		case MONTHLY: {
			YearMonth ym = YearMonth.from(getPluginDateFromMillis(millis));
			return ym.toString();
		}
		default:
			return String.valueOf(millis);
		}
	}

	private int periodsBetween(VoteStreakType type, String startKey, String endKey) {
		if (startKey == null || endKey == null) {
			return 0;
		}
		if (startKey.equals(endKey)) {
			return 0;
		}

		try {
			switch (type) {
			case DAILY: {
				LocalDate a = LocalDate.parse(startKey);
				LocalDate b = LocalDate.parse(endKey);
				long days = ChronoUnit.DAYS.between(a, b);
				return (int) Math.max(0, days);
			}
			case MONTHLY: {
				YearMonth a = YearMonth.parse(startKey);
				YearMonth b = YearMonth.parse(endKey);
				long months = ChronoUnit.MONTHS.between(a, b);
				return (int) Math.max(0, months);
			}
			case WEEKLY: {
				WeekKey a = WeekKey.parse(startKey);
				WeekKey b = WeekKey.parse(endKey);
				return Math.max(0, b.toIndex() - a.toIndex());
			}
			default:
				return 0;
			}
		} catch (Exception e) {
			return 0;
		}
	}

	private static final class WeekKey {
		final int year;
		final int week;

		private WeekKey(int year, int week) {
			this.year = year;
			this.week = week;
		}

		static WeekKey parse(String s) {
			String[] parts = s.split("-W");
			int y = Integer.parseInt(parts[0]);
			int w = Integer.parseInt(parts[1]);
			return new WeekKey(y, w);
		}

		int toIndex() {
			return (year * 53) + week;
		}
	}

	/**
	 * Forces rewards for a configured vote streak definition.
	 *
	 * This does not update streak progress, vote counts, period state, or config
	 * files.
	 *
	 * @param user        voting plugin user
	 * @param id          vote streak id
	 * @param streakCount streak amount to use for placeholders
	 * @param voteUUID    vote uuid for the reward event
	 * @return true if the reward was forced
	 */
	public boolean forceVoteStreakReward(VotingPluginUser user, String id, int streakCount, UUID voteUUID) {
		if (user == null || id == null || id.trim().isEmpty()) {
			return false;
		}

		VoteStreakDefinition def = getDefinition(id);
		if (def == null) {
			return false;
		}

		giveRewards(user, def, voteUUID == null ? UUID.randomUUID() : voteUUID, Math.max(1, streakCount));
		return true;
	}

	public int resetVoteStreaks(VotingPluginUser user) {
		if (user == null) {
			return 0;
		}

		int reset = 0;
		Set<String> resetColumns = new LinkedHashSet<>();
		for (VoteStreakDefinition def : ordered) {
			if (resetColumns.add(getColumnName(def))) {
				writeStateString(user, getColumnName(def), "");
				reset++;
			}
		}
		return reset;
	}

	public int resetVoteStreaks(VotingPluginUser user, VoteStreakType type) {
		if (user == null || type == null) {
			return 0;
		}

		int reset = 0;
		Set<String> resetColumns = new LinkedHashSet<>();
		for (VoteStreakDefinition def : ordered) {
			if (def.getType() == type && resetColumns.add(getColumnName(def))) {
				writeStateString(user, getColumnName(def), "");
				reset++;
			}
		}
		return reset;
	}

	public boolean resetVoteStreak(VotingPluginUser user, String id) {
		if (user == null || id == null || id.trim().isEmpty()) {
			return false;
		}

		String target = id.trim().toLowerCase(Locale.ROOT);
		VoteStreakDefinition progressGroup = byProgressGroup.get(target);
		if (progressGroup != null) {
			writeStateString(user, getColumnName(progressGroup), "");
			return true;
		}

		VoteStreakDefinition def = getDefinition(target);
		if (def == null) {
			return false;
		}
		if (def.getProgressGroup() != null && !def.getProgressGroup().isEmpty()) {
			return false;
		}

		writeStateString(user, getColumnName(def), "");
		return true;
	}

	/**
	 * Gives rewards configured for losing a progress group streak.
	 *
	 * @param user voting plugin user
	 * @param definition definition belonging to the progress group
	 * @param lostStreakCount streak count before reset
	 */
	private void giveProgressGroupLostRewards(VotingPluginUser user, VoteStreakDefinition definition,
			int lostStreakCount) {
		String rewardPath = "VoteStreaks.ProgressGroups." + definition.getProgressGroup() + ".LostRewards";
		ConfigurationSection root = plugin.getSpecialRewardsConfig().getData();

		plugin.extraDebug("[VoteStreak] giving progress group lost rewards for group="
				+ definition.getProgressGroup() + " path=" + rewardPath + " streakCount=" + lostStreakCount);

		new RewardBuilder(root, rewardPath)
				.withPlaceHolder("id", definition.getProgressGroup())
				.withPlaceHolder("type", definition.getType().toString())
				.withPlaceHolder("amount", String.valueOf(lostStreakCount))
				.withPlaceHolder("streak", String.valueOf(lostStreakCount))
				.send(user);
	}

	private void giveRewards(VotingPluginUser user, VoteStreakDefinition def, UUID voteUUID, int streakCount) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.VOTESTREAKS.setType(def.getType().toString()).setAmount(def.getVotesRequired()),
				voteUUID);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), def.getRewardPath())
				.withPlaceHolder("id", def.getId()).withPlaceHolder("type", def.getType().toString())
				.withPlaceHolder("amount", "" + streakCount).withPlaceHolder("streak", "" + streakCount).send(user);
	}

	public String getColumnName(VoteStreakDefinition def) {
		if (def.getProgressGroup() != null && !def.getProgressGroup().isEmpty()) {
			return "VoteStreakGroup_" + def.getType().name() + "_" + def.getProgressGroup();
		}
		return "VoteStreak_" + def.getId();
	}

	public StreakState readState(VotingPluginUser user, VoteStreakDefinition def) {
		String raw = readStateString(user, getColumnName(def));
		return StreakState.deserialize(raw);
	}

	public void writeState(VotingPluginUser user, VoteStreakDefinition def, StreakState state) {
		writeStateString(user, getColumnName(def), state.serialize());
	}

	public String readStateString(VotingPluginUser user, String columnName) {
		String val = user.getVoteStreakState(columnName);
		return val == null ? "" : val;
	}

	private void writeStateString(VotingPluginUser user, String columnName, String value) {
		user.setVoteStreakState(columnName, value);
	}

	private static String safeUser(VotingPluginUser user) {
		try {
			String n = user.getPlayerName();
			if (n != null && !n.isEmpty()) {
				return n;
			}
		} catch (Exception ignored) {
		}
		try {
			return String.valueOf(user.getUUID());
		} catch (Exception ignored) {
		}
		return "unknown";
	}

	private static final class StreakState {
		String periodKey = "";
		int streakCount = 0;
		int votesThisPeriod = 0;
		boolean countedThisPeriod = false;
		String missWindowStartKey = "";
		int missesUsed = 0;
		Set<String> rewardedDefinitions = new LinkedHashSet<>();

		String serialize() {
			String base = safe(periodKey) + "|" + streakCount + "|" + votesThisPeriod + "|" + countedThisPeriod + "|"
					+ safe(missWindowStartKey) + "|" + missesUsed;
			String rewarded = serializeRewardedDefinitions();
			return rewarded.isEmpty() ? base : base + "|" + rewarded;
		}

		static StreakState deserialize(String raw) {
			StreakState s = new StreakState();
			if (raw == null || raw.isEmpty()) {
				return s;
			}

			String[] p = raw.split("\\|", -1);

			if (p.length >= 6 && MessageAPI.isInt(p[2])) {
				if (p.length > 0)
					s.periodKey = p[0];
				if (p.length > 1)
					s.streakCount = parseInt(p[1], 0);
				if (p.length > 2)
					s.votesThisPeriod = parseInt(p[2], 0);
				if (p.length > 3)
					s.countedThisPeriod = parseBool(p[3], false);
				if (p.length > 4)
					s.missWindowStartKey = p[4];
				if (p.length > 5)
					s.missesUsed = parseInt(p[5], 0);
			} else {
				if (p.length > 0)
					s.periodKey = p[0];
				if (p.length > 1)
					s.streakCount = parseInt(p[1], 0);
				if (p.length > 2)
					s.countedThisPeriod = parseBool(p[2], false);
				if (p.length > 3)
					s.missWindowStartKey = p[3];
				if (p.length > 4)
					s.missesUsed = parseInt(p[4], 0);

				s.votesThisPeriod = s.countedThisPeriod ? 1 : 0;
			}

			if (p.length > 6) {
				for (String rewarded : p[6].split(",")) {
					if (rewarded != null && !rewarded.trim().isEmpty()) {
						s.rewardedDefinitions.add(rewarded.trim().toLowerCase(Locale.ROOT));
					}
				}
			}

			if (s.periodKey == null)
				s.periodKey = "";
			if (s.missWindowStartKey == null)
				s.missWindowStartKey = "";
			if (s.streakCount < 0)
				s.streakCount = 0;
			if (s.votesThisPeriod < 0)
				s.votesThisPeriod = 0;
			if (s.missesUsed < 0)
				s.missesUsed = 0;

			return s;
		}

		boolean hasRewarded(VoteStreakDefinition def) {
			return rewardedDefinitions.contains(def.getId().toLowerCase(Locale.ROOT));
		}

		void markRewarded(VoteStreakDefinition def) {
			rewardedDefinitions.add(def.getId().toLowerCase(Locale.ROOT));
		}

		private String serializeRewardedDefinitions() {
			if (rewardedDefinitions.isEmpty()) {
				return "";
			}
			return String.join(",", rewardedDefinitions);
		}

		private static String safe(String s) {
			return s == null ? "" : s;
		}

		private static int parseInt(String s, int def) {
			if (s == null)
				return def;
			if (MessageAPI.isInt(s))
				return Integer.parseInt(s);
			return def;
		}

		private static boolean parseBool(String s, boolean def) {
			if (s == null)
				return def;
			return Boolean.parseBoolean(s);
		}
	}

	public final class VoteStreakConfigLoader {

		private final Pattern idPattern = Pattern.compile("^[A-Za-z0-9_\\-]+$"); // no spaces
		private final Pattern progressGroupPattern = Pattern.compile("^[A-Za-z0-9_\\-]+$");

		private boolean loadLegacy(ConfigurationSection root) {
			ConfigurationSection legacy = root.getConfigurationSection("VoteStreak");
			if (legacy == null) {
				return false;
			}
			ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
			if (voteStreaks == null) {
				voteStreaks = root.createSection("VoteStreaks");
			}
			boolean any = false;
			any |= loadLegacyType(voteStreaks, legacy, "Day", VoteStreakType.DAILY);
			any |= loadLegacyType(voteStreaks, legacy, "Week", VoteStreakType.WEEKLY);
			any |= loadLegacyType(voteStreaks, legacy, "Month", VoteStreakType.MONTHLY);
			if (any) {
				plugin.getSpecialRewardsConfig().saveData();
			}
			return any;
		}

		private boolean loadLegacyType(ConfigurationSection voteStreaks, ConfigurationSection legacy, String key,
				VoteStreakType type) {
			ConfigurationSection sec = legacy.getConfigurationSection(key);
			if (sec == null) {
				return false;
			}

			if (!sec.getBoolean("Enabled", true)) {
				return false;
			}

			boolean any = false;
			boolean migratedAny = false;

			for (String streakKey : sec.getKeys(false)) {
				ConfigurationSection defSec = sec.getConfigurationSection(streakKey);
				if (defSec == null) {
					continue;
				}

				String normalized = streakKey.replace("-", "");
				if (!MessageAPI.isInt(normalized)) {
					continue;
				}

				int amount = Integer.parseInt(normalized);
				if (amount <= 0) {
					continue;
				}

				boolean enabled = defSec.getBoolean("Enabled", true);
				if (!enabled) {
					continue;
				}

				boolean recurring = streakKey.contains("-");
				defSec.set("Enabled", false);

				String id = "Legacy" + type.name() + amount + (recurring ? "Recurring" : "OneTime");

				if (voteStreaks.getConfigurationSection(id) == null) {
					ConfigurationSection migrated = voteStreaks.createSection(id);
					migrated.set("Type", type.name());
					migrated.set("Enabled", true);
					migrated.set("Recurring", recurring);

					ConfigurationSection req = migrated.createSection("Requirements");
					req.set("Amount", amount);
					req.set("VotesRequired", 1);

					migrated.set("AllowMissedAmount", 0);
					migrated.set("AllowMissedPeriod", 0);

					ConfigurationSection rewards = defSec.getConfigurationSection("Rewards");
					if (rewards != null) {
						migrated.createSection("Rewards", rewards.getValues(false));
					} else if (defSec.isList("Rewards")) {
						migrated.set("Rewards", defSec.getList("Rewards"));
					}

					migratedAny = true;
				}

				VoteStreakDefinition def = new VoteStreakDefinition(id, type, true, amount, 1, 0, 0, recurring);
				plugin.getUserManager().getDataManager()
						.addKey(new UserDataKeyString(getColumnName(def)).setColumnType("MEDIUMTEXT"));

				byId.put(id.toLowerCase(Locale.ROOT), def);
				ordered.add(def);
				any = true;
			}

			if (migratedAny) {
				plugin.getSpecialRewardsConfig().saveData();
			}

			return any;

		}

		public void load(ConfigurationSection root) {
			if (root == null) {
				plugin.getLogger().warning("VoteStreaks config root is null; no streaks loaded.");
				return;
			}

			ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
			if (voteStreaks == null || voteStreaks.getKeys(false).isEmpty()) {
				plugin.getLogger().warning(
						"VoteStreaks is missing or empty; run /av migratevotestreaks to migrate legacy VoteStreak config.");
				return;
			}

			int count = 0;

			for (String id : voteStreaks.getKeys(false)) {
				count++;

				if ("ProgressGroups".equalsIgnoreCase(id)) {
					continue;
				}

				if (id == null || id.trim().isEmpty()) {
					plugin.getLogger().warning("VoteStreaks entry #" + count + " has empty key; skipping.");
					continue;
				}

				id = id.trim();
				if (!idPattern.matcher(id).matches()) {
					plugin.getLogger()
							.warning("VoteStreaks ID '" + id + "' is invalid (only A-Z, 0-9, _, -). Skipping.");
					continue;
				}

				ConfigurationSection defSec = voteStreaks.getConfigurationSection(id);
				if (defSec == null) {
					plugin.getLogger().warning("VoteStreaks '" + id + "' is not a section; skipping.");
					continue;
				}

				String typeStr = defSec.getString("Type");
				VoteStreakType type = null;
				try {
					type = VoteStreakType.from(typeStr);
				} catch (Exception e) {
					plugin.getLogger()
							.warning("VoteStreaks '" + id + "' has invalid Type '" + typeStr + "'; skipping.");
				}
				if (type == null) {
					plugin.getLogger()
							.warning("VoteStreaks '" + id + "' has invalid Type '" + typeStr + "'; skipping.");
					continue;
				}

				boolean enabled = defSec.getBoolean("Enabled", true);

				int amountInterval = 0;
				int votesRequired = 1;

				ConfigurationSection req = defSec.getConfigurationSection("Requirements");
				if (req != null) {
					amountInterval = req.getInt("Amount", 0);
					votesRequired = req.getInt("VotesRequired", 1);
				}

				if (amountInterval <= 0) {
					plugin.getLogger().warning("VoteStreaks '" + id + "' Requirements.Amount must be > 0; skipping.");
					continue;
				}
				if (votesRequired <= 0) {
					votesRequired = 1;
				}

				int allowMissedAmount = Math.max(0, defSec.getInt("AllowMissedAmount", 0));
				int allowMissedPeriod = Math.max(0, defSec.getInt("AllowMissedPeriod", 0));

				boolean recurring = defSec.getBoolean("Recurring", true);

				// ConfigurationSection editableTarget = getOrCreateVoteStreakSection(id);

				VoteStreakDefinition def = new VoteStreakDefinition(id, type, enabled, amountInterval, votesRequired,
						allowMissedAmount, allowMissedPeriod, recurring);

				plugin.getUserManager().getDataManager()
						.addKey(new UserDataKeyString(getColumnName(def)).setColumnType("MEDIUMTEXT"));

				byId.put(id.toLowerCase(Locale.ROOT), def);
				ordered.add(def);
			}

			loadProgressGroups(voteStreaks);

			plugin.getLogger().info("Loaded " + ordered.size() + " VoteStreak definitions.");
		}

		private void loadProgressGroups(ConfigurationSection voteStreaks) {
			ConfigurationSection progressGroups = voteStreaks.getConfigurationSection("ProgressGroups");
			if (progressGroups == null) {
				return;
			}

			for (String groupId : progressGroups.getKeys(false)) {
				if (groupId == null || groupId.trim().isEmpty()) {
					plugin.getLogger().warning("VoteStreaks.ProgressGroups has an empty group id; skipping.");
					continue;
				}

				groupId = groupId.trim();
				if (!progressGroupPattern.matcher(groupId).matches()) {
					plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId
							+ "' is invalid (only A-Z, 0-9, _, -). Skipping.");
					continue;
				}
				if (byId.containsKey(groupId.toLowerCase(Locale.ROOT))) {
					plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId
							+ "' duplicates an existing VoteStreak id; skipping.");
					continue;
				}

				ConfigurationSection groupSec = progressGroups.getConfigurationSection(groupId);
				if (groupSec == null) {
					plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' is not a section; skipping.");
					continue;
				}

				VoteStreakType type = readType("VoteStreaks.ProgressGroups '" + groupId + "'", groupSec.getString("Type"));
				if (type == null) {
					continue;
				}

				boolean groupEnabled = groupSec.getBoolean("Enabled", true);
				int votesRequired = Math.max(1, groupSec.getInt("VotesRequired", 1));
				int allowMissedAmount = Math.max(0, groupSec.getInt("AllowMissedAmount", 0));
				int allowMissedPeriod = Math.max(0, groupSec.getInt("AllowMissedPeriod", 0));

				ConfigurationSection milestones = groupSec.getConfigurationSection("Milestones");
				if (milestones == null || milestones.getKeys(false).isEmpty()) {
					plugin.getLogger()
							.warning("VoteStreaks.ProgressGroups '" + groupId + "' has no Milestones; skipping.");
					continue;
				}

				for (String milestoneId : milestones.getKeys(false)) {
					loadProgressGroupMilestone(groupId, groupSec, milestones, milestoneId, type, groupEnabled,
							votesRequired, allowMissedAmount, allowMissedPeriod);
				}
			}
		}

		private void loadProgressGroupMilestone(String groupId, ConfigurationSection groupSec,
				ConfigurationSection milestones, String milestoneId, VoteStreakType type, boolean groupEnabled,
				int votesRequired, int allowMissedAmount, int allowMissedPeriod) {
			if (milestoneId == null || milestoneId.trim().isEmpty()) {
				plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' has an empty milestone id; skipping.");
				return;
			}

			milestoneId = milestoneId.trim();
			if (!idPattern.matcher(milestoneId).matches()) {
				plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' milestone '" + milestoneId
						+ "' is invalid (only A-Z, 0-9, _, -). Skipping.");
				return;
			}

			if (byId.containsKey(milestoneId.toLowerCase(Locale.ROOT))) {
				plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' milestone '" + milestoneId
						+ "' duplicates an existing VoteStreak id; skipping.");
				return;
			}

			ConfigurationSection milestoneSec = milestones.getConfigurationSection(milestoneId);
			if (milestoneSec == null) {
				plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' milestone '" + milestoneId
						+ "' is not a section; skipping.");
				return;
			}

			int amount = readRequiredAmount(milestoneSec);
			if (amount <= 0) {
				plugin.getLogger().warning("VoteStreaks.ProgressGroups '" + groupId + "' milestone '" + milestoneId
						+ "' Amount must be > 0; skipping.");
				return;
			}

			boolean enabled = groupEnabled && milestoneSec.getBoolean("Enabled", true);
			boolean recurring = milestoneSec.getBoolean("Recurring", groupSec.getBoolean("Recurring", true));
			String rewardPath = "VoteStreaks.ProgressGroups." + groupId + ".Milestones." + milestoneId + ".Rewards";

			VoteStreakDefinition def = new VoteStreakDefinition(milestoneId, type, enabled, amount, votesRequired,
					allowMissedAmount, allowMissedPeriod, recurring, groupId, rewardPath);

			plugin.getUserManager().getDataManager()
					.addKey(new UserDataKeyString(getColumnName(def)).setColumnType("MEDIUMTEXT"));

			byId.put(milestoneId.toLowerCase(Locale.ROOT), def);
			byProgressGroup.putIfAbsent(groupId.toLowerCase(Locale.ROOT), def);
			ordered.add(def);
		}

		private VoteStreakType readType(String logPrefix, String typeStr) {
			VoteStreakType type = null;
			try {
				type = VoteStreakType.from(typeStr);
			} catch (Exception e) {
				plugin.getLogger().warning(logPrefix + " has invalid Type '" + typeStr + "'; skipping.");
			}
			if (type == null) {
				plugin.getLogger().warning(logPrefix + " has invalid Type '" + typeStr + "'; skipping.");
			}
			return type;
		}

		private int readRequiredAmount(ConfigurationSection sec) {
			int amount = sec.getInt("Amount", 0);
			ConfigurationSection req = sec.getConfigurationSection("Requirements");
			if (amount <= 0 && req != null) {
				amount = req.getInt("Amount", 0);
			}
			return amount;
		}
	}
}