package com.bencodez.votingplugin.votestreak;

import java.lang.reflect.Method;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.temporal.ChronoUnit;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.time.TimeChecker;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;
import com.bencodez.votingplugin.events.SpecialRewardType;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Vote streak handler: loads definitions + processes votes.
 *
 * Config format (Option B):
 *
 * VoteStreaks: Daily3Votes: Type: DAILY Enabled: true Requirements: Amount: 3
 * VotesRequired: 3 AllowMissedAmount: 1 AllowMissedPeriod: 7 Rewards: Commands:
 * - say test
 *
 * RewardBuilder expects: <VoteStreaksSection>.<idKey>.<reward keys> We pass the
 * VoteStreaks section and def.getId() (lowercase).
 */
public class VoteStreakHandler {

	private final VotingPluginMain plugin;

	private final Map<String, VoteStreakDefinition> byId = new LinkedHashMap<>();
	private final List<VoteStreakDefinition> ordered = new ArrayList<>();

	public VoteStreakHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Reload streak definitions from config.
	 */
	public void reload() {
		byId.clear();
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

	/**
	 * Call this whenever a vote happens
	 *
	 * @param user           voting plugin user
	 * @param voteTimeMillis vote time
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

		for (VoteStreakDefinition def : ordered) {
			if (!def.isEnabled()) {
				plugin.extraDebug(
						"[VoteStreak] processVote: skip disabled def id=" + def.getId() + " idKey=" + def.getId());
				continue;
			}

			try {
				processVoteForDefinition(user, def, voteTimeMillis, voteUUID);
			} catch (Exception e) {
				plugin.getLogger().warning("VoteStreak processing failed for '" + def.getId() + "': " + e.getMessage());
				plugin.debug(e);
			}
		}
	}

	private void processVoteForDefinition(VotingPluginUser user, VoteStreakDefinition def, long voteTimeMillis,
			UUID voteUUID) {
		final String col = getColumnName(def);
		final String rawBefore = readStateString(user, col);
		StreakState state = StreakState.deserialize(rawBefore);

		final String currentPeriodKey = periodKey(def.getType(), voteTimeMillis);

		plugin.extraDebug("[VoteStreak] def=" + def.getId() + " idKey=" + def.getId() + " type=" + def.getType()
				+ " col=" + col + " period=" + currentPeriodKey + " votesReq=" + def.getVotesRequired() + " interval="
				+ def.getRequiredAmount() + " rawBefore='" + rawBefore + "' stateBefore={periodKey=" + state.periodKey
				+ ",streakCount=" + state.streakCount + ",votesThisPeriod=" + state.votesThisPeriod
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
			advancePeriodsAndApplyMisses(def, state, voteTimeMillis, currentPeriodKey);
			plugin.extraDebug("[VoteStreak] after advance state={periodKey=" + state.periodKey + ",streakCount="
					+ state.streakCount + ",votesThisPeriod=" + state.votesThisPeriod + ",countedThisPeriod="
					+ state.countedThisPeriod + ",missesUsed=" + state.missesUsed + ",missWindowStartKey="
					+ state.missWindowStartKey + "}");
		}

		// Count votes within the period until threshold reached, then mark period
		// satisfied once.
		if (!state.countedThisPeriod) {
			state.votesThisPeriod++;

			int votesRequired = Math.max(1, def.getVotesRequired());
			plugin.extraDebug("[VoteStreak] vote counted in period: votesThisPeriod=" + state.votesThisPeriod + "/"
					+ votesRequired);

			if (state.votesThisPeriod >= votesRequired) {
				state.countedThisPeriod = true;
				state.streakCount++;

				int interval = Math.max(1, def.getRequiredAmount());
				boolean shouldReward = state.streakCount > 0 && (state.streakCount % interval) == 0;

				plugin.extraDebug("[VoteStreak] period satisfied: streakCount=" + state.streakCount + " interval="
						+ interval + " shouldReward=" + shouldReward);

				if (shouldReward) {
					plugin.extraDebug("[VoteStreak] giving rewards for idKey=" + def.getId());
					giveRewards(user, def, voteUUID);
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

	/**
	 * When periods change, we determine if previous periods were misses.
	 * 
	 * @param def              vote streak definition
	 * @param state            current streak state
	 * @param nowMillis        current time millis
	 * @param currentPeriodKey current period key
	 */
	private void advancePeriodsAndApplyMisses(VoteStreakDefinition def, StreakState state, long nowMillis,
			String currentPeriodKey) {

		if (!state.countedThisPeriod) {
			plugin.extraDebug("[VoteStreak] advance: previous period " + state.periodKey + " NOT satisfied -> miss");
			recordMiss(def, state, nowMillis);
		}

		int skipped = periodsBetween(def.getType(), state.periodKey, currentPeriodKey);
		plugin.extraDebug(
				"[VoteStreak] advance: skipped=" + skipped + " between " + state.periodKey + " -> " + currentPeriodKey);

		if (skipped > 1) {
			for (int i = 0; i < skipped - 1; i++) {
				plugin.extraDebug(
						"[VoteStreak] advance: recording intermediate missed period " + (i + 1) + "/" + (skipped - 1));
				recordMiss(def, state, nowMillis);
			}
		}

		state.periodKey = currentPeriodKey;
		state.countedThisPeriod = false;
		state.votesThisPeriod = 0;

		if (def.getAllowMissedAmount() <= 0) {
			if (state.missesUsed > 0) {
				plugin.extraDebug(
						"[VoteStreak] advance: no misses allowed and missesUsed=" + state.missesUsed + " -> reset");
				resetStreakAndMissTracking(state);
			}
		} else if (state.missesUsed > def.getAllowMissedAmount()) {
			plugin.extraDebug("[VoteStreak] advance: missesUsed=" + state.missesUsed + " > allowMissedAmount="
					+ def.getAllowMissedAmount() + " -> reset");
			resetStreakAndMissTracking(state);
		}
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

	private void giveRewards(VotingPluginUser user, VoteStreakDefinition def, UUID voteUUID) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.VOTESTREAK.setType(def.getType().toString()).setAmount(def.getVotesRequired()),
				voteUUID);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), "VoteStreaks." + def.getId() + ".Rewards")
				.withPlaceHolder("id", def.getId()).send(user);
	}

	private String getColumnName(VoteStreakDefinition def) {
		return "VoteStreak_" + def.getId();
	}

	private StreakState readState(VotingPluginUser user, VoteStreakDefinition def) {
		String raw = readStateString(user, getColumnName(def));
		return StreakState.deserialize(raw);
	}

	private void writeState(VotingPluginUser user, VoteStreakDefinition def, StreakState state) {
		writeStateString(user, getColumnName(def), state.serialize());
	}

	private String readStateString(VotingPluginUser user, String columnName) {
		String val = user.getData().getString(columnName);
		return val == null ? "" : val;
	}

	private void writeStateString(VotingPluginUser user, String columnName, String value) {
		user.getData().setString(columnName, value);
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

		String serialize() {
			return safe(periodKey) + "|" + streakCount + "|" + votesThisPeriod + "|" + countedThisPeriod + "|"
					+ safe(missWindowStartKey) + "|" + missesUsed;
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

	private ConfigurationSection getOrCreateVoteStreakSection(String idKey) {
		ConfigurationSection root = plugin.getSpecialRewardsConfig().getData();

		ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
		if (voteStreaks == null) {
			voteStreaks = root.createSection("VoteStreaks");
		}

		ConfigurationSection sec = voteStreaks.getConfigurationSection(idKey);
		if (sec == null) {
			sec = voteStreaks.createSection(idKey);
		}
		return sec;
	}

	public final class VoteStreakConfigLoader {

		private final Pattern idPattern = Pattern.compile("^[A-Za-z0-9_\\-]+$"); // no spaces

		public void load(ConfigurationSection root) {
			if (root == null) {
				plugin.getLogger().warning("VoteStreaks config root is null; no streaks loaded.");
				return;
			}

			ConfigurationSection voteStreaks = root.getConfigurationSection("VoteStreaks");
			if (voteStreaks == null) {
				plugin.getLogger().warning("VoteStreaks is missing or not a section; no streaks loaded.");
				return;
			}

			int count = 0;

			for (String id : voteStreaks.getKeys(false)) {
				count++;

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
				VoteStreakType type = VoteStreakType.from(typeStr);
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

				ConfigurationSection editableTarget = getOrCreateVoteStreakSection(id);

				VoteStreakDefinition def = new VoteStreakDefinition(id, type, enabled, amountInterval, votesRequired,
						allowMissedAmount, allowMissedPeriod);

				byId.put(id, def);
				ordered.add(def);
			}

			plugin.getLogger().info("Loaded " + ordered.size() + " VoteStreak definitions.");
		}
	}
}
