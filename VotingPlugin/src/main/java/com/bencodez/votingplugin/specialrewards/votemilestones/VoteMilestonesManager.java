package com.bencodez.votingplugin.specialrewards.votemilestones;

import java.time.ZoneId;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;
import java.util.UUID;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.VoteMilestoneRewardEvent;
import com.bencodez.votingplugin.proxy.VoteTotalsSnapshot;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

/**
 * Evaluates VoteMilestones on each vote.
 *
 * <p>
 * RewardPath is treated as an absolute path from SpecialRewards.yml root.
 * </p>
 *
 * <p>
 * Supports optional per-milestone limits by persisting a compact
 * id-to-timestamp map in user storage.
 * </p>
 */
public class VoteMilestonesManager {

	@Getter
	private static final String LIMITS_STORAGE_KEY = "VoteMilestoneLimits";

	private final VotingPluginMain plugin;

	@Getter
	private VoteMilestonesConfig config;

	@Getter
	private Map<String, VoteMilestoneGroupSelect> groupModes;

	public VoteMilestonesManager(VotingPluginMain plugin) {
		this.plugin = plugin;
		loadConfig();
	}

	public void reload() {
		loadConfig();
	}

	/**
	 * Stores milestone last-trigger timestamps (epoch millis) in a single compact
	 * string:
	 *
	 * <pre>
	 * id|1700000000000;OtherId|1700001111111
	 * </pre>
	 *
	 * with backslash escaping for '\', '|', ';'.
	 */
	private static final class MilestoneLimitStore {
		private final Map<String, Long> lastTriggered = new LinkedHashMap<>();
		private boolean dirty;

		static MilestoneLimitStore loadFromUser(VotingPluginUser user) {
			MilestoneLimitStore s = new MilestoneLimitStore();
			s.parse(getUserString(user, LIMITS_STORAGE_KEY));
			return s;
		}

		boolean remove(String id) {
			if (id == null) {
				return false;
			}
			Long prev = lastTriggered.remove(id);
			if (prev != null) {
				dirty = true;
				return true;
			}
			return false;
		}

		long getLastTriggeredMs(String id) {
			if (id == null) {
				return -1L;
			}
			Long v = lastTriggered.get(id);
			return v == null ? -1L : v.longValue();
		}

		void markTriggered(String id, long nowMs) {
			if (id == null || id.isEmpty()) {
				return;
			}
			lastTriggered.put(id, nowMs);
			dirty = true;
		}

		boolean isDirty() {
			return dirty;
		}

		void saveToUser(VotingPluginUser user) {
			if (!dirty) {
				return;
			}
			setUserString(user, LIMITS_STORAGE_KEY, serialize());
			dirty = false;
		}

		String serialize() {
			StringBuilder sb = new StringBuilder();
			boolean first = true;

			for (Map.Entry<String, Long> e : lastTriggered.entrySet()) {
				if (e == null || e.getKey() == null || e.getValue() == null) {
					continue;
				}

				long ms = e.getValue().longValue();
				if (ms <= 0) {
					continue;
				}

				if (!first) {
					sb.append(';');
				}
				first = false;

				sb.append(escape(e.getKey())).append('|').append(ms);
			}
			return sb.toString();
		}

		private void parse(String raw) {
			lastTriggered.clear();
			if (raw == null || raw.trim().isEmpty()) {
				return;
			}

			StringBuilder cur = new StringBuilder();
			ArrayList<String> entries = new ArrayList<>();
			boolean esc = false;

			for (int i = 0; i < raw.length(); i++) {
				char c = raw.charAt(i);
				if (esc) {
					cur.append(c);
					esc = false;
					continue;
				}
				if (c == '\\') {
					esc = true;
					continue;
				}
				if (c == ';') {
					entries.add(cur.toString());
					cur.setLength(0);
					continue;
				}
				cur.append(c);
			}
			if (cur.length() > 0) {
				entries.add(cur.toString());
			}

			for (String entry : entries) {
				if (entry == null || entry.isEmpty()) {
					continue;
				}

				int idx = entry.indexOf('|');
				if (idx <= 0) {
					continue;
				}

				String id = unescape(entry.substring(0, idx));
				String msStr = entry.substring(idx + 1);

				long ms = safeParseLong(msStr, -1L);
				if (ms > 0) {
					lastTriggered.put(id, ms);
				}
			}
		}

		private static String escape(String s) {
			StringBuilder sb = new StringBuilder();
			for (int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				if (c == '\\' || c == '|' || c == ';') {
					sb.append('\\');
				}
				sb.append(c);
			}
			return sb.toString();
		}

		private static String unescape(String s) {
			if (s == null || s.indexOf('\\') < 0) {
				return s;
			}
			StringBuilder sb = new StringBuilder();
			boolean esc = false;
			for (int i = 0; i < s.length(); i++) {
				char c = s.charAt(i);
				if (esc) {
					sb.append(c);
					esc = false;
					continue;
				}
				if (c == '\\') {
					esc = true;
					continue;
				}
				sb.append(c);
			}
			return sb.toString();
		}

		private static long safeParseLong(String s, long def) {
			try {
				return Long.parseLong(s.trim());
			} catch (Exception ex) {
				return def;
			}
		}
	}

	private static String getUserString(VotingPluginUser user, String key) {
		return user.getUserData().getString(key);
	}

	private static void setUserString(VotingPluginUser user, String key, String value) {
		user.getUserData().setString(key, value);
	}

	private ZoneId getLimitZone() {
		return ZoneId.systemDefault();
	}

	private long nowMs() {
		return System.currentTimeMillis();
	}

	public void loadConfig() {
		groupModes = VoteMilestoneGroupParser.parseGroups(plugin.getSpecialRewardsConfig().getData());
		this.config = VoteMilestonesConfig.load(plugin, this, plugin.getSpecialRewardsConfig().getData());

		if (config == null || config.getMilestones() == null) {
			plugin.debug("[VoteMilestones] Loaded 0 milestones (config null)");
			return;
		}

		int total = config.getMilestones().size();
		int enabled = 0;
		int legacy = 0;

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null) {
				continue;
			}
			if (m.isEnabled()) {
				enabled++;
			}
			if (m.getId() != null && m.getId().startsWith("Legacy_")) {
				legacy++;
			}
		}

		plugin.debug(
				"[VoteMilestones] Loaded " + total + " milestones (" + enabled + " enabled, " + legacy + " legacy)");

		VoteMilestoneGroupSelect defaultMode = resolveDefaultGroupMode();
		int groupCount = (groupModes == null ? 0 : groupModes.size());

		plugin.debug("[VoteMilestones] GroupModes count=" + groupCount + " default=" + defaultMode.name()
				+ (groupCount > 0 ? " modes=" + formatGroupModes(groupModes, 10) : ""));

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null) {
				continue;
			}

			String groupId = safeGroupId(m);
			VoteMilestoneGroupSelect mode = resolveGroupSelect(m);

			StringBuilder line = new StringBuilder();
			line.append("[VoteMilestones] Milestone").append(" id=").append(m.getId()).append(" enabled=")
					.append(m.isEnabled()).append(" group=").append(groupId).append(" mode=").append(mode.name())
					.append(" total=").append(m.getTotal() != null ? m.getTotal().name() : "null");

			if (m.getAtMatcher() != null) {
				line.append(" at=").append(m.getAtMatcher().toDebugString(8, 3));
			}
			if (m.getEvery() != null && m.getEvery() > 0) {
				line.append(" every=").append(m.getEvery());
			}

			if (m.getLimit() != null && m.getLimit().isEnabled()) {
				line.append(" limit=").append(m.getLimit().getType().name());
			}

			line.append(" path=").append(safe(m.getRewardPath())).append(" legacy=")
					.append(m.getId() != null && m.getId().startsWith("Legacy_"));

			plugin.debug(line.toString());
		}
	}

	private VoteMilestoneGroupSelect resolveGroupSelect(VoteMilestone milestone) {
		return resolveGroupSelectWithSource(milestone).select;
	}

	private VoteMilestoneGroupSelect resolveDefaultGroupMode() {
		VoteMilestoneGroupSelect def = getGroupModeCaseInsensitive("default");
		return def == null ? VoteMilestoneGroupSelect.ALL : def;
	}

	private ResolvedGroupSelect resolveGroupSelectWithSource(VoteMilestone milestone) {
		String groupId = safeGroupId(milestone);

		VoteMilestoneGroupSelect explicit = getGroupModeCaseInsensitive(groupId);
		if (explicit != null) {
			return new ResolvedGroupSelect(explicit, GroupSelectSource.CONFIG_GROUP, groupId);
		}

		VoteMilestoneGroupSelect fromMilestone = (milestone == null ? null : milestone.getGroupSelect());
		if (fromMilestone != null) {
			return new ResolvedGroupSelect(fromMilestone, GroupSelectSource.MILESTONE, groupId);
		}

		VoteMilestoneGroupSelect def = getGroupModeCaseInsensitive("default");
		if (def != null) {
			return new ResolvedGroupSelect(def, GroupSelectSource.CONFIG_DEFAULT, "default");
		}

		return new ResolvedGroupSelect(VoteMilestoneGroupSelect.ALL, GroupSelectSource.FALLBACK_ALL, "");
	}

	private VoteMilestoneGroupSelect getGroupModeCaseInsensitive(String key) {
		if (groupModes == null || groupModes.isEmpty() || key == null) {
			return null;
		}

		String k = key.trim();
		if (k.isEmpty()) {
			return null;
		}

		VoteMilestoneGroupSelect direct = groupModes.get(k);
		if (direct != null) {
			return direct;
		}

		for (Map.Entry<String, VoteMilestoneGroupSelect> e : groupModes.entrySet()) {
			if (e == null || e.getKey() == null) {
				continue;
			}
			if (e.getKey().trim().equalsIgnoreCase(k)) {
				return e.getValue();
			}
		}
		return null;
	}

	private String formatGroupModes(Map<String, VoteMilestoneGroupSelect> map, int max) {
		if (map == null || map.isEmpty()) {
			return "";
		}

		StringBuilder sb = new StringBuilder();
		int count = 0;

		VoteMilestoneGroupSelect def = getGroupModeCaseInsensitive("default");
		if (def != null) {
			sb.append("default=").append(def.name());
			count++;
		}

		for (Map.Entry<String, VoteMilestoneGroupSelect> e : map.entrySet()) {
			if (e == null) {
				continue;
			}
			String k = e.getKey();
			if (k == null) {
				continue;
			}
			if ("default".equalsIgnoreCase(k)) {
				continue;
			}

			if (count > 0) {
				sb.append(", ");
			}
			sb.append(k).append("=").append(e.getValue() == null ? "null" : e.getValue().name());
			count++;

			if (count >= max) {
				sb.append(", ...");
				break;
			}
		}
		return sb.toString();
	}

	public void handleVote(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData,
			Map<String, String> contextPlaceholders) {
		handleVote(user, bungeeMessageData, false, null, contextPlaceholders);
	}

	public void handleVote(VotingPluginUser user, VoteTotalsSnapshot bungeeMessageData, boolean forceBungee,
			UUID voteUUID, Map<String, String> contextPlaceholders) {
		if (config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return;
		}

		UUID uuid = user.getJavaUUID();
		String playerName = user.getPlayerName();

		long nowMs = nowMs();
		ZoneId zone = getLimitZone();

		MilestoneLimitStore limitStore = MilestoneLimitStore.loadFromUser(user);

		Map<String, List<MatchResult>> matchedByGroup = new LinkedHashMap<>();
		int order = 0;

		for (VoteMilestone m : config.getMilestones().values()) {
			order++;
			if (m == null) {
				continue;
			}

			long value = -1L;
			boolean valueOk = false;
			Exception valueErr = null;

			if (m.isEnabled()) {
				try {
					value = m.getTotal().getValue(user, bungeeMessageData);
					valueOk = true;
				} catch (Exception e) {
					valueErr = e;
				}
			}

			boolean matched = false;
			TriggerType matchedBy = TriggerType.NONE;

			if (m.isEnabled() && valueOk) {
				try {
					matchedBy = getMatchedTriggerType(m, value);
					matched = (matchedBy != TriggerType.NONE);
				} catch (Exception e) {
					valueErr = e;
					valueOk = false;
				}
			}

			String trigger = buildTriggerDebug(m);
			String groupId = safeGroupId(m);

			if (!m.isEnabled()) {
				plugin.debug("[VoteMilestones] VoteCheck player=" + playerName + "/" + uuid + " id=" + m.getId()
						+ " group=" + groupId + " enabled=false total="
						+ (m.getTotal() == null ? "null" : m.getTotal().name()) + " trigger=" + trigger + " path="
						+ safe(m.getRewardPath()) + " value=SKIPPED match=false");
				continue;
			}

			if (!valueOk) {
				plugin.debug("[VoteMilestones] VoteCheck player=" + playerName + "/" + uuid + " id=" + m.getId()
						+ " group=" + groupId + " enabled=true total="
						+ (m.getTotal() == null ? "null" : m.getTotal().name()) + " trigger=" + trigger + " path="
						+ safe(m.getRewardPath()) + " value=ERROR match=false error=" + (valueErr == null ? "unknown"
								: valueErr.getClass().getSimpleName() + ":" + safe(valueErr.getMessage())));
				continue;
			}

			plugin.debug("[VoteMilestones] VoteCheck player=" + playerName + "/" + uuid + " id=" + m.getId() + " group="
					+ groupId + " enabled=true total=" + (m.getTotal() == null ? "null" : m.getTotal().name())
					+ " trigger=" + trigger + " path=" + safe(m.getRewardPath()) + " value=" + value + " match="
					+ matched);

			if (!matched) {
				continue;
			}

			MatchResult mr = new MatchResult(m, value, order, matchedBy);
			matchedByGroup.computeIfAbsent(groupId, k -> new ArrayList<>()).add(mr);
		}

		for (Map.Entry<String, List<MatchResult>> e : matchedByGroup.entrySet()) {
			String groupId = e.getKey();
			List<MatchResult> matches = e.getValue();
			if (matches == null || matches.isEmpty()) {
				continue;
			}

			ResolvedGroupSelect resolved = resolveGroupSelectWithSource(matches.get(0).m);
			VoteMilestoneGroupSelect select = resolved.select;
			List<MatchResult> toExecute = selectMatches(select, matches);

			plugin.debug("[VoteMilestones] GroupResolve player=" + playerName + "/" + uuid + " group=" + groupId
					+ " select=" + (select == null ? "null" : select.name()) + " source="
					+ (resolved.source == null ? "null" : resolved.source.name())
					+ (resolved.sourceKey != null && !resolved.sourceKey.isEmpty() ? " sourceKey=" + resolved.sourceKey
							: "")
					+ " matched=" + matches.size() + " execute=" + toExecute.size());

			for (MatchResult mr : toExecute) {
				VoteMilestone m = mr.m;
				long value = mr.value;

				VoteMilestoneLimit limit = (m == null ? null : m.getLimit());
				if (limit != null && limit.isEnabled()) {
					long lastMs = limitStore.getLastTriggeredMs(m.getId());
					boolean allowed = limit.allows(lastMs, nowMs, zone);
					if (!allowed) {
						plugin.debug(
								"[VoteMilestones] LimitBlock player=" + playerName + "/" + uuid + " group=" + groupId
										+ " id=" + m.getId() + " limit=" + limit.getType().name() + " lastMs=" + lastMs
										+ (limit.getType() == VoteMilestoneLimit.Type.COOLDOWN
												? " nextAllowedMs=" + limit.nextAllowedMs(lastMs)
												: "")
										+ " nowMs=" + nowMs);
						continue;
					}
				}

				HashMap<String, String> placeholders = new HashMap<>();
				if (contextPlaceholders != null) {
					placeholders.putAll(contextPlaceholders);
				}
				placeholders.putAll(buildPlaceholders(m, value));

				try {
					executeRewards(user, m, placeholders, forceBungee);

					if (limit != null && limit.isEnabled()) {
						limitStore.markTriggered(m.getId(), nowMs);
					}

					Bukkit.getPluginManager().callEvent(new VoteMilestoneRewardEvent(user, m, value, groupId,
							safe(m.getRewardPath()), placeholders, voteUUID));

					plugin.debug("[VoteMilestones] Execute ok player=" + playerName + "/" + uuid + " group=" + groupId
							+ " id=" + m.getId() + " value=" + value + " matchedBy=" + mr.matchedBy.name() + " path="
							+ safe(m.getRewardPath()));
				} catch (Exception ex) {
					plugin.debug("[VoteMilestones] Execute fail player=" + playerName + "/" + uuid + " group=" + groupId
							+ " id=" + m.getId() + " value=" + value + " matchedBy=" + mr.matchedBy.name() + " path="
							+ safe(m.getRewardPath()) + " error=" + ex.getClass().getSimpleName() + ":"
							+ safe(ex.getMessage()));
				}
			}
		}

		if (limitStore.isDirty()) {
			try {
				limitStore.saveToUser(user);
			} catch (Exception ex) {
				plugin.debug("[VoteMilestones] LimitSave fail player=" + playerName + "/" + uuid + " error="
						+ ex.getClass().getSimpleName() + ":" + safe(ex.getMessage()));
			}
		}
	}

	/*
	 * ======================================================================= FORCE
	 * EXECUTION
	 * =======================================================================
	 */

	public int forceGroup(VotingPluginUser user, String groupId, boolean bypassLimits, VoteTotalsSnapshot bungeeData) {
		if (user == null || groupId == null || groupId.trim().isEmpty()) {
			return 0;
		}
		if (config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return 0;
		}

		final String wantedGroup = groupId.trim();
		final UUID uuid = user.getJavaUUID();
		final String playerName = user.getPlayerName();
		final long nowMs = nowMs();
		final ZoneId zone = getLimitZone();

		MilestoneLimitStore limitStore = MilestoneLimitStore.loadFromUser(user);

		int executed = 0;

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null || !m.isEnabled()) {
				continue;
			}

			String mid = safeGroupId(m);
			if (!equalsIgnoreCase(mid, wantedGroup)) {
				continue;
			}

			VoteMilestoneLimit limit = m.getLimit();
			if (!bypassLimits && limit != null && limit.isEnabled()) {
				long lastMs = limitStore.getLastTriggeredMs(m.getId());
				if (!limit.allows(lastMs, nowMs, zone)) {
					continue;
				}
			}

			long value = safeTotalValue(user, m, bungeeData);
			HashMap<String, String> placeholders = buildPlaceholders(m, value);

			try {
				executeRewards(user, m, placeholders, false);

				if (!bypassLimits && limit != null && limit.isEnabled()) {
					limitStore.markTriggered(m.getId(), nowMs);
				}

				Bukkit.getPluginManager().callEvent(
						new VoteMilestoneRewardEvent(user, m, value, mid, safe(m.getRewardPath()), placeholders, null));

				executed++;
				plugin.debug("[VoteMilestones] Force Execute ok player=" + playerName + "/" + uuid + " group=" + mid
						+ " id=" + m.getId() + " value=" + value + " bypassLimits=" + bypassLimits + " path="
						+ safe(m.getRewardPath()));
			} catch (Exception ex) {
				plugin.debug("[VoteMilestones] Force Execute fail player=" + playerName + "/" + uuid + " group=" + mid
						+ " id=" + m.getId() + " bypassLimits=" + bypassLimits + " error="
						+ ex.getClass().getSimpleName() + ":" + safe(ex.getMessage()));
			}
		}

		if (limitStore.isDirty()) {
			try {
				limitStore.saveToUser(user);
			} catch (Exception ex) {
				plugin.debug("[VoteMilestones] Force LimitSave fail player=" + playerName + "/" + uuid + " error="
						+ ex.getClass().getSimpleName() + ":" + safe(ex.getMessage()));
			}
		}

		return executed;
	}

	public boolean forceMilestone(VotingPluginUser user, String milestoneId, boolean bypassLimits,
			VoteTotalsSnapshot bungeeData) {
		if (user == null || milestoneId == null || milestoneId.trim().isEmpty()) {
			return false;
		}
		if (config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return false;
		}

		VoteMilestone m = getMilestoneByIdCaseInsensitive(milestoneId.trim());
		if (m == null || !m.isEnabled()) {
			return false;
		}

		final String groupId = safeGroupId(m);
		final UUID uuid = user.getJavaUUID();
		final String playerName = user.getPlayerName();
		final long nowMs = nowMs();
		final ZoneId zone = getLimitZone();

		MilestoneLimitStore limitStore = MilestoneLimitStore.loadFromUser(user);

		VoteMilestoneLimit limit = m.getLimit();
		if (!bypassLimits && limit != null && limit.isEnabled()) {
			long lastMs = limitStore.getLastTriggeredMs(m.getId());
			if (!limit.allows(lastMs, nowMs, zone)) {
				return false;
			}
		}

		long value = safeTotalValue(user, m, bungeeData);
		HashMap<String, String> placeholders = buildPlaceholders(m, value);

		try {
			executeRewards(user, m, placeholders, false);

			if (!bypassLimits && limit != null && limit.isEnabled()) {
				limitStore.markTriggered(m.getId(), nowMs);
				limitStore.saveToUser(user);
			}

			Bukkit.getPluginManager().callEvent(
					new VoteMilestoneRewardEvent(user, m, value, groupId, safe(m.getRewardPath()), placeholders, null));

			plugin.debug("[VoteMilestones] ForceSingle Execute ok player=" + playerName + "/" + uuid + " group="
					+ groupId + " id=" + m.getId() + " value=" + value + " bypassLimits=" + bypassLimits + " path="
					+ safe(m.getRewardPath()));
			return true;
		} catch (Exception ex) {
			plugin.debug("[VoteMilestones] ForceSingle Execute fail player=" + playerName + "/" + uuid + " group="
					+ groupId + " id=" + m.getId() + " bypassLimits=" + bypassLimits + " error="
					+ ex.getClass().getSimpleName() + ":" + safe(ex.getMessage()));
			return false;
		}
	}

	/**
	 * Resolve a milestone's configured group id by milestone id (case-insensitive).
	 *
	 * @return group id, or null if milestone not found
	 */
	public String getGroupForMilestoneId(String milestoneId) {
		VoteMilestone m = getMilestoneByIdCaseInsensitive(milestoneId);
		return m == null ? null : safeGroupId(m);
	}

	/**
	 * Returns true if a milestone id exists (case-insensitive) and is enabled.
	 */
	public boolean isMilestoneEnabled(String milestoneId) {
		VoteMilestone m = getMilestoneByIdCaseInsensitive(milestoneId);
		return m != null && m.isEnabled();
	}

	/*
	 * =======================================================================
	 * PREVIEW / DEBUG / STATUS
	 * =======================================================================
	 */

	public List<String> previewGroup(VotingPluginUser user, String groupId, VoteTotalsSnapshot bungeeData) {
		List<String> out = new ArrayList<>();
		if (user == null) {
			return out;
		}
		if (config == null || config.getMilestones() == null) {
			return out;
		}

		// load limit info (if you have limits enabled, this tells you what is currently
		// blocked)
		MilestoneLimitStore store = MilestoneLimitStore.loadFromUser(user);
		long nowMs = nowMs();
		ZoneId zone = getLimitZone();

		// gather matches (what would fire "right now")
		List<MatchResult> matches = new ArrayList<>();
		int order = 0;

		// find current total for this group (all milestones in a group should use the
		// same total type, but we don't assume)
		Long firstValue = null;

		for (VoteMilestone m : config.getMilestones().values()) {
			order++;
			if (m == null || !m.isEnabled()) {
				continue;
			}
			if (!safeGroupId(m).equalsIgnoreCase(groupId)) {
				continue;
			}

			long value = safeTotalValue(user, m, bungeeData);
			if (firstValue == null) {
				firstValue = value;
			}

			TriggerType matchedBy = getMatchedTriggerType(m, value);
			boolean matched = matchedBy != TriggerType.NONE;
			if (matched) {
				matches.add(new MatchResult(m, value, order, matchedBy));
			}
		}

		ResolvedGroupSelect resolved = (matches.isEmpty() ? null : resolveGroupSelectWithSource(matches.get(0).m));
		VoteMilestoneGroupSelect select = resolved == null ? VoteMilestoneGroupSelect.ALL : resolved.select;
		List<MatchResult> toExecute = matches.isEmpty() ? java.util.Collections.emptyList()
				: selectMatches(select, matches);

		out.add("group=" + groupId + " total=" + (firstValue == null ? 0 : firstValue.longValue()) + " matched="
				+ matches.size() + " execute=" + toExecute.size() + " select="
				+ (select == null ? "null" : select.name())
				+ (resolved == null ? ""
						: " source=" + resolved.source.name()
								+ (resolved.sourceKey != null && !resolved.sourceKey.isEmpty()
										? " sourceKey=" + resolved.sourceKey
										: "")));

		// show per-milestone details
		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null || !safeGroupId(m).equalsIgnoreCase(groupId)) {
				continue;
			}
			long value = safeTotalValue(user, m, bungeeData);

			TriggerType matchedBy = (m.isEnabled() ? getMatchedTriggerType(m, value) : TriggerType.NONE);
			boolean matched = matchedBy != TriggerType.NONE;

			Long lastPoint = lastTriggerForMilestone(m, value);
			Long nextPoint = nextTriggerForMilestone(m, value);

			// limit info (if enabled)
			VoteMilestoneLimit limit = m.getLimit();
			long lastMs = store.getLastTriggeredMs(m.getId());
			boolean limitEnabled = limit != null && limit.isEnabled();
			boolean allowedNow = !limitEnabled || limit.allows(lastMs, nowMs, zone);
			Long nextAllowed = (limitEnabled && !allowedNow) ? limit.nextAllowedMs(lastMs) : null;

			boolean executedNow = false;
			for (MatchResult mr : toExecute) {
				if (mr != null && mr.m != null
						&& (mr.m.getId() != null && m.getId() != null && mr.m.getId().equalsIgnoreCase(m.getId()))) {
					executedNow = true;
					break;
				}
			}

			StringBuilder sb = new StringBuilder();
			sb.append(m.getId()).append(" enabled=").append(m.isEnabled()).append(" totalType=")
					.append(m.getTotal() == null ? "null" : m.getTotal().name()).append(" value=").append(value)
					.append(" match=").append(matched ? matchedBy.name() : "false").append(" lastPoint=")
					.append(lastPoint == null ? "?" : lastPoint.longValue()).append(" nextPoint=")
					.append(nextPoint == null ? "?" : nextPoint.longValue());

			if (limitEnabled) {
				sb.append(" limit=").append(limit.getType().name());
				sb.append(" lastTriggered=").append(lastMs <= 0 ? "never" : formatEpochMs(lastMs, zone));
				if (!allowedNow) {
					sb.append(" BLOCKED");
					if (nextAllowed != null) {
						sb.append(" nextAllowed=").append(formatEpochMs(nextAllowed.longValue(), zone)).append(" (")
								.append(formatDeltaMs(nextAllowed.longValue() - nowMs)).append(")");
					}
				} else {
					sb.append(" allowed=true");
				}
			}

			if (executedNow) {
				sb.append(" [EXECUTE]");
			} else if (matched) {
				sb.append(" [MATCH]");
			}

			out.add(sb.toString());
		}

		return out;
	}

	/**
	 * Detailed status for a group: shows what has already been given (based on
	 * limiter storage) and what should have been given given the current total.
	 */
	public List<String> statusGroup(VotingPluginUser user, String groupId, VoteTotalsSnapshot bungeeData) {
		List<String> out = new ArrayList<>();
		if (user == null) {
			out.add("No user");
			return out;
		}
		if (config == null || config.getMilestones() == null) {
			out.add("No milestones configured");
			return out;
		}

		MilestoneLimitStore store = MilestoneLimitStore.loadFromUser(user);
		long nowMs = nowMs();
		ZoneId zone = getLimitZone();

		out.add("group=" + groupId + " uuid=" + user.getUUID());

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null) {
				continue;
			}
			if (!safeGroupId(m).equalsIgnoreCase(groupId)) {
				continue;
			}

			long value = safeTotalValue(user, m, bungeeData);
			Long lastPoint = lastTriggerForMilestone(m, value);
			Long nextPoint = nextTriggerForMilestone(m, value);
			TriggerType matchedBy = (m.isEnabled() ? getMatchedTriggerType(m, value) : TriggerType.NONE);

			VoteMilestoneLimit limit = m.getLimit();
			boolean limitEnabled = limit != null && limit.isEnabled();
			long lastMs = store.getLastTriggeredMs(m.getId());

			boolean alreadyGiven = lastMs > 0;
			// If totals are monotonic, reaching current value implies we've passed every
			// trigger point <= value.
			boolean shouldHaveBeenGiven = !alreadyGiven && m.isEnabled() && lastPoint != null
					&& lastPoint.longValue() > 0 && value >= lastPoint.longValue()
					&& (matchedBy != TriggerType.NONE || value > lastPoint.longValue());

			boolean allowedNow = !limitEnabled || limit.allows(lastMs, nowMs, zone);
			Long nextAllowed = (limitEnabled && !allowedNow) ? limit.nextAllowedMs(lastMs) : null;

			String state;
			if (!m.isEnabled()) {
				state = "DISABLED";
			} else if (alreadyGiven) {
				state = "GIVEN";
			} else if (shouldHaveBeenGiven) {
				state = "MISSING";
			} else if (matchedBy != TriggerType.NONE) {
				state = "DUE_NOW";
			} else {
				state = "PENDING";
			}

			StringBuilder sb = new StringBuilder();
			sb.append(m.getId()).append(" state=").append(state).append(" value=").append(value).append(" lastPoint=")
					.append(lastPoint == null ? "?" : lastPoint.longValue()).append(" nextPoint=")
					.append(nextPoint == null ? "?" : nextPoint.longValue());

			if (matchedBy != TriggerType.NONE) {
				sb.append(" matchBy=").append(matchedBy.name());
			}

			if (limitEnabled) {
				sb.append(" limit=").append(limit.getType().name());
				sb.append(" lastTriggered=").append(lastMs <= 0 ? "never" : formatEpochMs(lastMs, zone));
				if (!allowedNow) {
					sb.append(" BLOCKED");
					if (nextAllowed != null) {
						sb.append(" nextAllowed=").append(formatEpochMs(nextAllowed.longValue(), zone)).append(" (")
								.append(formatDeltaMs(nextAllowed.longValue() - nowMs)).append(")");
					}
				}
			}

			out.add(sb.toString());
		}

		return out;
	}

	/**
	 * Backwards-compatible one-line status (keeps existing callers).
	 */
	public String status(VotingPluginUser user, String groupId, VoteTotalsSnapshot bungeeData) {
		List<String> lines = statusGroup(user, groupId, bungeeData);
		if (lines.isEmpty()) {
			return "No data";
		}
		// keep it short: group=... total=...
		String first = lines.get(0);
		long total = 0;
		for (VoteMilestone m : (config == null || config.getMilestones() == null)
				? java.util.Collections.<VoteMilestone>emptyList()
				: config.getMilestones().values()) {
			if (m != null && safeGroupId(m).equalsIgnoreCase(groupId)) {
				total = safeTotalValue(user, m, bungeeData);
				break;
			}
		}
		return "group=" + groupId + " total=" + total + " (" + first + ")";
	}

	private String formatEpochMs(long ms, ZoneId zone) {
		try {
			java.time.ZonedDateTime zdt = java.time.ZonedDateTime.ofInstant(java.time.Instant.ofEpochMilli(ms), zone);
			return zdt.toLocalDateTime().toString().replace('T', ' ');
		} catch (Exception e) {
			return String.valueOf(ms);
		}
	}

	private String formatDeltaMs(long deltaMs) {
		long s = Math.max(0, deltaMs) / 1000L;
		long days = s / 86400L;
		s %= 86400L;
		long hours = s / 3600L;
		s %= 3600L;
		long mins = s / 60L;
		long secs = s % 60L;
		StringBuilder sb = new StringBuilder();
		if (days > 0)
			sb.append(days).append("d ");
		if (hours > 0 || days > 0)
			sb.append(hours).append("h ");
		if (mins > 0 || hours > 0 || days > 0)
			sb.append(mins).append("m ");
		sb.append(secs).append("s");
		return sb.toString().trim();
	}

	public List<String> list(String groupId) {
		List<String> out = new ArrayList<>();
		if (config == null || config.getMilestones() == null) {
			return out;
		}

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null) {
				continue;
			}
			if (!safeGroupId(m).equalsIgnoreCase(groupId)) {
				continue;
			}
			out.add(m.getId() + " enabled=" + m.isEnabled());
		}
		return out;
	}

	/*
	 * ======================================================================= LIMIT
	 * RESET =======================================================================
	 */

	public int resetLimits(VotingPluginUser user, String groupId) {
		if (user == null) {
			return 0;
		}

		MilestoneLimitStore store = MilestoneLimitStore.loadFromUser(user);
		int removed = 0;

		if (config == null || config.getMilestones() == null) {
			return 0;
		}

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null) {
				continue;
			}
			if (groupId != null && !groupId.trim().isEmpty() && !safeGroupId(m).equalsIgnoreCase(groupId)) {
				continue;
			}
			if (store.remove(m.getId())) {
				removed++;
			}
		}

		if (store.isDirty()) {
			store.saveToUser(user);
		}
		return removed;
	}

	/*
	 * =======================================================================
	 * PLACEHOLDER HELPERS (NEXT/LAST/UNTIL)
	 * =======================================================================
	 */

	/**
	 * Returns the next VoteMilestone trigger value for the given group (or
	 * default), based on enabled milestones in that group.
	 *
	 * @return next trigger value, or null if none can be determined
	 */
	public Long getNextMilestoneValue(String groupId, VotingPluginUser user) {
		if (user == null || config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return null;
		}
		String g = (groupId == null || groupId.trim().isEmpty()) ? "default" : groupId.trim();

		Long best = null;
		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null || !m.isEnabled()) {
				continue;
			}
			String mg = safeGroupId(m);
			if (!mg.equalsIgnoreCase(g)) {
				continue;
			}
			long current = safeTotalValue(user, m, null);
			Long next = nextTriggerForMilestone(m, current);
			if (next == null) {
				continue;
			}
			if (best == null || next.longValue() < best.longValue()) {
				best = next;
			}
		}
		return best;
	}

	/**
	 * Returns the last (most recent) VoteMilestone trigger value for the given
	 * group (or default), based on enabled milestones in that group.
	 *
	 * @return last trigger value, or null if none can be determined
	 */
	public Long getLastMilestoneValue(String groupId, VotingPluginUser user) {
		if (user == null || config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return null;
		}
		String g = (groupId == null || groupId.trim().isEmpty()) ? "default" : groupId.trim();

		Long best = null;
		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null || !m.isEnabled()) {
				continue;
			}
			String mg = safeGroupId(m);
			if (!mg.equalsIgnoreCase(g)) {
				continue;
			}
			long current = safeTotalValue(user, m, null);
			Long last = lastTriggerForMilestone(m, current);
			if (last == null) {
				continue;
			}
			if (best == null || last.longValue() > best.longValue()) {
				best = last;
			}
		}
		return best;
	}

	/**
	 * Returns how many votes (in the same counter used by the next milestone) until
	 * the next milestone. If no next milestone exists (or can't be determined),
	 * returns 0.
	 */
	public long getVotesUntilNextMilestone(String groupId, VotingPluginUser user) {
		if (user == null || config == null || config.getMilestones() == null || config.getMilestones().isEmpty()) {
			return 0;
		}
		String g = (groupId == null || groupId.trim().isEmpty()) ? "default" : groupId.trim();

		Long bestNext = null;
		Long bestCurrent = null;

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m == null || !m.isEnabled()) {
				continue;
			}
			String mg = safeGroupId(m);
			if (!mg.equalsIgnoreCase(g)) {
				continue;
			}
			long current = safeTotalValue(user, m, null);
			Long next = nextTriggerForMilestone(m, current);
			if (next == null) {
				continue;
			}
			if (bestNext == null || next.longValue() < bestNext.longValue()) {
				bestNext = next;
				bestCurrent = current;
			}
		}

		if (bestNext == null || bestCurrent == null) {
			return 0;
		}
		long diff = bestNext.longValue() - bestCurrent.longValue();
		return Math.max(0, diff);
	}

	private Long nextTriggerForMilestone(VoteMilestone m, long currentValue) {
		if (m == null) {
			return null;
		}

		Long best = null;

		// every=N => next multiple
		Integer every = m.getEvery();
		if (every != null && every > 0) {
			long nextEvery = ((currentValue / every) + 1L) * every;
			if (nextEvery > currentValue) {
				best = nextEvery;
			}
		}

		// atMatcher exact totals => next > current
		AtMatcher at = m.getAtMatcher();
		Long nextAt = nextFromAtMatcher(at, currentValue);
		if (nextAt != null && (best == null || nextAt.longValue() < best.longValue())) {
			best = nextAt;
		}

		return best;
	}

	private Long lastTriggerForMilestone(VoteMilestone m, long currentValue) {
		if (m == null) {
			return null;
		}

		Long best = null;

		Integer every = m.getEvery();
		if (every != null && every > 0) {
			long lastEvery = (currentValue / every) * every;
			if (lastEvery > 0 && lastEvery <= currentValue) {
				best = lastEvery;
			}
		}

		AtMatcher at = m.getAtMatcher();
		Long lastAt = lastFromAtMatcher(at, currentValue);
		if (lastAt != null && (best == null || lastAt.longValue() > best.longValue())) {
			best = lastAt;
		}

		return best;
	}

	private List<Long> extractAtCandidates(AtMatcher atMatcher) {
		if (atMatcher == null) {
			return java.util.Collections.emptyList();
		}

		TreeSet<Long> sorted = new TreeSet<>();

		// At: 0 means "first vote", which corresponds to newTotal == 1
		if (atMatcher.hasZero()) {
			sorted.add(1L);
		}

		for (Long v : atMatcher.getExactTotals()) {
			if (v != null && v.longValue() > 0) {
				sorted.add(v);
			}
		}

		if (sorted.isEmpty()) {
			return java.util.Collections.emptyList();
		}
		return new ArrayList<>(sorted);
	}

	private Long nextFromAtMatcher(AtMatcher atMatcher, long currentValue) {
		List<Long> vals = extractAtCandidates(atMatcher);
		if (vals.isEmpty()) {
			return null;
		}
		for (Long v : vals) {
			if (v != null && v.longValue() > currentValue) {
				return v;
			}
		}
		return null;
	}

	private Long lastFromAtMatcher(AtMatcher atMatcher, long currentValue) {
		List<Long> vals = extractAtCandidates(atMatcher);
		if (vals.isEmpty()) {
			return null;
		}
		Long best = null;
		for (Long v : vals) {
			if (v == null) {
				continue;
			}
			long lv = v.longValue();
			if (lv <= currentValue && (best == null || lv > best.longValue())) {
				best = v;
			}
		}
		return best;
	}

	/*
	 * =======================================================================
	 * INTERNAL HELPERS
	 * =======================================================================
	 */

	private long safeTotalValue(VotingPluginUser user, VoteMilestone m, VoteTotalsSnapshot data) {
		try {
			return m.getTotal() == null ? 0 : m.getTotal().getValue(user, data);
		} catch (Exception e) {
			return 0;
		}
	}

	private HashMap<String, String> buildPlaceholders(VoteMilestone m, long value) {
		HashMap<String, String> map = new HashMap<>();
		String amount = String.valueOf(value);

		map.put("total", amount);
		map.put("milestone", amount);
		map.put("id", m.getId());

		// keep older style keys your rewards may already be using
		map.put("amount", amount);
		map.put("cumulative", amount);

		// common percent variants seen in configs
		map.put("%total%", amount);
		map.put("%votemilestone%", m.getId());

		map.put("total_type", m.getTotal() == null ? "null" : m.getTotal().name());
		map.put("%total_type%", m.getTotal() == null ? "null" : m.getTotal().name());

		return map;
	}

	private VoteMilestone getMilestoneByIdCaseInsensitive(String id) {
		if (id == null || config == null || config.getMilestones() == null) {
			return null;
		}

		VoteMilestone direct = config.getMilestones().get(id);
		if (direct != null) {
			return direct;
		}

		for (VoteMilestone m : config.getMilestones().values()) {
			if (m != null && m.getId() != null && m.getId().equalsIgnoreCase(id)) {
				return m;
			}
		}
		return null;
	}

	private static boolean equalsIgnoreCase(String a, String b) {
		if (a == null && b == null) {
			return true;
		}
		if (a == null || b == null) {
			return false;
		}
		return a.equalsIgnoreCase(b);
	}

	private enum TriggerType {
		NONE, AT, EVERY
	}

	private enum GroupSelectSource {
		CONFIG_GROUP, CONFIG_DEFAULT, MILESTONE, FALLBACK_ALL
	}

	private static final class ResolvedGroupSelect {
		final VoteMilestoneGroupSelect select;
		final GroupSelectSource source;
		final String sourceKey;

		ResolvedGroupSelect(VoteMilestoneGroupSelect select, GroupSelectSource source, String sourceKey) {
			this.select = select;
			this.source = source;
			this.sourceKey = sourceKey;
		}
	}

	private static final class MatchResult {
		final VoteMilestone m;
		final long value;
		final int order;
		final TriggerType matchedBy;

		MatchResult(VoteMilestone m, long value, int order, TriggerType matchedBy) {
			this.m = m;
			this.value = value;
			this.order = order;
			this.matchedBy = matchedBy;
		}
	}

	private TriggerType getMatchedTriggerType(VoteMilestone m, long value) {
		if (m.getAtMatcher() != null && m.getAtMatcher().matches(value)) {
			return TriggerType.AT;
		}

		Integer every = m.getEvery();
		if (every != null && every > 0 && value > 0 && (value % every == 0)) {
			return TriggerType.EVERY;
		}

		return TriggerType.NONE;
	}

	private List<MatchResult> selectMatches(VoteMilestoneGroupSelect select, List<MatchResult> matches) {
		if (select == null) {
			select = VoteMilestoneGroupSelect.ALL;
		}

		switch (select) {
		case ALL:
			return matches;

		case FIRST: {
			MatchResult first = null;
			for (MatchResult mr : matches) {
				if (first == null || mr.order < first.order) {
					first = mr;
				}
			}
			return first == null ? java.util.Collections.emptyList() : java.util.Collections.singletonList(first);
		}

		case HIGHEST: {
			MatchResult best = null;
			for (MatchResult mr : matches) {
				if (best == null || isBetterHighest(mr, best)) {
					best = mr;
				}
			}
			return best == null ? java.util.Collections.emptyList() : java.util.Collections.singletonList(best);
		}

		default:
			return matches;
		}
	}

	private boolean isBetterHighest(MatchResult a, MatchResult b) {
		int rankA = triggerRank(a);
		int rankB = triggerRank(b);
		if (rankA != rankB) {
			return rankA > rankB;
		}

		if (a.matchedBy == TriggerType.AT && b.matchedBy == TriggerType.AT) {
			if (a.value != b.value) {
				return a.value > b.value;
			}
		}

		if (a.matchedBy == TriggerType.EVERY && b.matchedBy == TriggerType.EVERY) {
			int ea = safeEvery(a.m);
			int eb = safeEvery(b.m);
			if (ea != eb) {
				return ea > eb;
			}
		}

		if (a.value != b.value) {
			return a.value > b.value;
		}
		return a.order < b.order;
	}

	private int triggerRank(MatchResult mr) {
		if (mr.matchedBy == TriggerType.AT) {
			return 3;
		}
		if (mr.matchedBy == TriggerType.EVERY) {
			return 2;
		}
		return 1;
	}

	private int safeEvery(VoteMilestone m) {
		Integer e = (m == null ? null : m.getEvery());
		return e == null ? -1 : e.intValue();
	}

	private String safeGroupId(VoteMilestone m) {
		String group = (m == null ? null : m.getGroupKey());
		if (group == null || group.trim().isEmpty()) {
			return "default";
		}
		return group.trim();
	}

	private String safe(String s) {
		return (s == null ? "" : s.replace('\n', ' ').replace('\r', ' ').trim());
	}

	private String buildTriggerDebug(VoteMilestone m) {
		StringBuilder sb = new StringBuilder();
		boolean hasAny = false;

		if (m.getAtMatcher() != null) {
			sb.append("AT(").append(m.getAtMatcher().toDebugString(8, 3)).append(")");
			hasAny = true;
		}

		Integer every = m.getEvery();
		if (every != null && every > 0) {
			if (hasAny) {
				sb.append("+");
			}
			sb.append("EVERY(").append(every).append(")");
			hasAny = true;
		}

		return hasAny ? sb.toString() : "NONE";
	}

	private void executeRewards(VotingPluginUser user, VoteMilestone m, HashMap<String, String> placeholders,
			boolean forceBungee) {
		String path = m.getRewardPath();
		if (path == null || path.trim().isEmpty()) {
			return;
		}

		RewardOptions opts = new RewardOptions().setPrefix("VoteMilestone-" + m.getId()).setServer(forceBungee)
				.setPlaceholders(placeholders);

		plugin.getRewardHandler().giveReward(user, plugin.getSpecialRewardsConfig().getData(), path, opts);
	}
}
