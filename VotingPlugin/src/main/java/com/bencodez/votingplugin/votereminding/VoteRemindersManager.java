// File: com/bencodez/votingplugin/votereminding/VoteRemindersManager.java
package com.bencodez.votingplugin.votereminding;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votereminding.store.VoteReminderCooldownStore;

/**
 * VoteReminders core manager (no Bukkit Listener).
 *
 * Goals:
 * - Single top-level config section: VoteReminderOptions + VoteReminders (map keys)
 * - Map-based reminders (no lists)
 * - Unified trigger model: "Type" is the trigger (no separate event enum)
 * - Global + per-reminder cooldowns stored in user data (1 global, 1 map column)
 * - StopAfterMatch behaves correctly when multiple triggers happen at once via queue+flush
 * - Delay: schedules a delayed evaluation (delay counts as match for StopAfterMatch)
 * - Conditions: CanVoteAny / CanVoteAll / MinOnlineTime / FirstJoin
 *
 * NOTE: COOLDOWN_END_* triggers are intended to be called from your existing CoolDownCheck
 * listener events (PlayerVoteCoolDownEndEvent / PlayerVoteSiteCoolDownEndEvent).
 */
public final class VoteRemindersManager {

	/*
	 * ========================= Enums / Models =========================
	 */

	/**
	 * "Type" is the trigger. No separate Event enum.
	 */
	public enum VoteReminderType {
		// Player lifecycle
		LOGIN,
		FIRST_JOIN,

		// Voting lifecycle
		VOTE_CAST,

		// Cooldown-driven (from CoolDownCheck)
		COOLDOWN_END_ANY_SITE,
		COOLDOWN_END_ALL_SITES,

		// Time-driven
		INTERVAL
	}

	public static final class VoteReminderConditions {
		private Boolean canVoteAny;
		private Boolean canVoteAll;

		private ParsedDuration minOnlineTime;
		private Boolean firstJoin;

		public Boolean getCanVoteAny() {
			return canVoteAny;
		}

		public void setCanVoteAny(Boolean canVoteAny) {
			this.canVoteAny = canVoteAny;
		}

		public Boolean getCanVoteAll() {
			return canVoteAll;
		}

		public void setCanVoteAll(Boolean canVoteAll) {
			this.canVoteAll = canVoteAll;
		}

		public ParsedDuration getMinOnlineTime() {
			return minOnlineTime;
		}

		public void setMinOnlineTime(ParsedDuration minOnlineTime) {
			this.minOnlineTime = minOnlineTime;
		}

		public Boolean getFirstJoin() {
			return firstJoin;
		}

		public void setFirstJoin(Boolean firstJoin) {
			this.firstJoin = firstJoin;
		}
	}

	public static final class VoteReminderOptions {
		private final boolean enabled;
		private final boolean stopAfterMatch;
		private final ParsedDuration globalCooldown;
		private final int defaultPriority;

		private final ParsedDuration defaultCooldown;
		private final ParsedDuration defaultDelay;
		private final VoteReminderConditions defaultsConditions;

		// Full path that RewardBuilder can use for defaults
		private final String defaultRewardsPath;

		public VoteReminderOptions(boolean enabled, boolean stopAfterMatch, ParsedDuration globalCooldown,
				int defaultPriority, ParsedDuration defaultCooldown, ParsedDuration defaultDelay,
				VoteReminderConditions defaultsConditions, String defaultRewardsPath) {
			this.enabled = enabled;
			this.stopAfterMatch = stopAfterMatch;
			this.globalCooldown = globalCooldown == null ? ParsedDuration.parse("") : globalCooldown;
			this.defaultPriority = defaultPriority;
			this.defaultCooldown = defaultCooldown == null ? ParsedDuration.parse("") : defaultCooldown;
			this.defaultDelay = defaultDelay == null ? ParsedDuration.parse("") : defaultDelay;
			this.defaultsConditions = defaultsConditions == null ? new VoteReminderConditions() : defaultsConditions;
			this.defaultRewardsPath = defaultRewardsPath;
		}

		public boolean isEnabled() {
			return enabled;
		}

		public boolean isStopAfterMatch() {
			return stopAfterMatch;
		}

		public ParsedDuration getGlobalCooldown() {
			return globalCooldown;
		}

		public int getDefaultPriority() {
			return defaultPriority;
		}

		public ParsedDuration getDefaultCooldown() {
			return defaultCooldown;
		}

		public ParsedDuration getDefaultDelay() {
			return defaultDelay;
		}

		public VoteReminderConditions getDefaultsConditions() {
			return defaultsConditions;
		}

		public String getDefaultRewardsPath() {
			return defaultRewardsPath;
		}
	}

	public static final class VoteReminderDefinition {
		private final String name;
		private final VoteReminderType type;
		private final int priority;

		private final ParsedDuration cooldown;
		private final ParsedDuration delay;

		private final String rewardsPath;
		private final VoteReminderConditions conditions;

		private final ParsedDuration interval;

		public VoteReminderDefinition(String name, VoteReminderType type, int priority, ParsedDuration cooldown,
				ParsedDuration delay, String rewardsPath, VoteReminderConditions conditions, ParsedDuration interval) {
			this.name = name;
			this.type = type;
			this.priority = priority;
			this.cooldown = cooldown == null ? ParsedDuration.parse("") : cooldown;
			this.delay = delay == null ? ParsedDuration.parse("") : delay;
			this.rewardsPath = rewardsPath;
			this.conditions = conditions == null ? new VoteReminderConditions() : conditions;
			this.interval = interval == null ? ParsedDuration.parse("") : interval;
		}

		public String getName() {
			return name;
		}

		public VoteReminderType getType() {
			return type;
		}

		public int getPriority() {
			return priority;
		}

		public ParsedDuration getCooldown() {
			return cooldown;
		}

		public ParsedDuration getDelay() {
			return delay;
		}

		public String getRewardsPath() {
			return rewardsPath;
		}

		public VoteReminderConditions getConditions() {
			return conditions;
		}

		public ParsedDuration getInterval() {
			return interval;
		}
	}

	/*
	 * ========================= Cooldowns wrapper =========================
	 */

	public static final class VoteReminderCooldowns {
		private final VoteReminderCooldownStore store;
		private final ParsedDuration globalCooldown;

		public VoteReminderCooldowns(VoteReminderCooldownStore store, ParsedDuration globalCooldown) {
			this.store = store;
			this.globalCooldown = globalCooldown == null ? ParsedDuration.parse("") : globalCooldown;
		}

		public boolean tryAcquireGlobal(UUID uuid, long nowMs) {
			long cd = ms(globalCooldown);
			if (cd <= 0) {
				return true;
			}
			return store.tryClaimGlobal(uuid, nowMs, cd);
		}

		/**
		 * Gate per-reminder using max(cooldown, interval) in millis.
		 */
		public boolean canFireReminder(UUID uuid, String reminderName, long nowMs, ParsedDuration cooldown,
				ParsedDuration interval) {
			long req = Math.max(ms(cooldown), ms(interval));
			if (req <= 0) {
				return true;
			}

			Map<String, Long> map = store.getPerReminderMap(uuid);
			long last = 0L;
			Long v = map.get(reminderName);
			if (v != null) {
				last = v.longValue();
			}
			return last <= 0 || (nowMs - last) >= req;
		}

		public void markFired(UUID uuid, String reminderName, long nowMs) {
			store.setPerReminderLast(uuid, reminderName, nowMs);
		}

		private long ms(ParsedDuration d) {
			if (d == null || d.isEmpty()) {
				return 0L;
			}
			return Math.max(0L, d.getMillis());
		}
	}

	/*
	 * ========================= Fields =========================
	 */

	private final VotingPluginMain plugin;
	private final VoteReminderCooldownStore store;

	private VoteReminderOptions options;
	private VoteReminderCooldowns cooldowns;

	private List<VoteReminderDefinition> reminders = Collections.emptyList();
	private Map<String, VoteReminderDefinition> byName = Collections.emptyMap();

	private final ConcurrentHashMap<UUID, Long> joinTimes = new ConcurrentHashMap<>();

	// Task ids
	private int minuteTaskId = -1;

	// Trigger queue (smart flush so StopAfterMatch works across multiple triggers)
	private final ConcurrentHashMap<UUID, PendingTriggers> pending = new ConcurrentHashMap<>();

	private static final class PendingTriggers {
		volatile boolean login;
		volatile boolean firstJoin;
		volatile boolean interval;
		volatile boolean voteCast;
		volatile boolean cooldownAny;
		volatile boolean cooldownAll;

		final Map<String, String> placeholders = new HashMap<>();

		void mergePlaceholders(Map<String, String> ph) {
			if (ph == null || ph.isEmpty()) {
				return;
			}
			placeholders.putAll(ph);
		}

		List<VoteReminderType> snapshotTypes() {
			List<VoteReminderType> out = new ArrayList<>();
			if (login)
				out.add(VoteReminderType.LOGIN);
			if (firstJoin)
				out.add(VoteReminderType.FIRST_JOIN);
			if (voteCast)
				out.add(VoteReminderType.VOTE_CAST);
			if (cooldownAll)
				out.add(VoteReminderType.COOLDOWN_END_ALL_SITES);
			if (cooldownAny)
				out.add(VoteReminderType.COOLDOWN_END_ANY_SITE);
			if (interval)
				out.add(VoteReminderType.INTERVAL);
			return out;
		}
	}

	/*
	 * ========================= Constructor =========================
	 */

	public VoteRemindersManager(VotingPluginMain plugin, VoteReminderCooldownStore store) {
		this.plugin = plugin;
		this.store = store;
	}

	/*
	 * ========================= Lifecycle =========================
	 */

	public void reload() {
		stopTasks();

		loadConfig();
		this.cooldowns = new VoteReminderCooldowns(store, options.getGlobalCooldown());

		startTasks();
	}

	public void shutdown() {
		stopTasks();
		joinTimes.clear();
		pending.clear();
	}

	private boolean isEnabled() {
		return options != null && options.isEnabled();
	}
	/*
	 * ========================= Entry points (called by ONE listener)
	 * =========================
	 */

	public void onJoin(Player player, VotingPluginUser user) {
		if (!isEnabled()) {
			return;
		}
		if (player == null || user == null) {
			return;
		}
		if (plugin.getOptions().isTreatVanishAsOffline() && user.isVanished()) {
			return;
		}

		UUID uuid = player.getUniqueId();
		joinTimes.put(uuid, System.currentTimeMillis());

		queueTrigger(user, player, VoteReminderType.LOGIN, null);

		if (!player.hasPlayedBefore()) {
			queueTrigger(user, player, VoteReminderType.FIRST_JOIN, null);
		}

		flushSoon(uuid, user.getPlayerName());
	}

	public void onQuit(Player player) {
		if (player == null) {
			return;
		}
		UUID uuid = player.getUniqueId();
		joinTimes.remove(uuid);
		pending.remove(uuid);
	}

	public void onVoteCast(VotingPluginUser user, String siteKey) {
		if (!isEnabled() || user == null) {
			return;
		}

		Player player = Bukkit.getPlayer(user.getJavaUUID());
		if (player == null || !player.isOnline()) {
			return;
		}
		if (plugin.getOptions().isTreatVanishAsOffline() && user.isVanished()) {
			return;
		}

		Map<String, String> ph = new HashMap<>();
		if (siteKey != null && !siteKey.isEmpty()) {
			ph.put("site", siteKey);
		}

		queueTrigger(user, player, VoteReminderType.VOTE_CAST, ph);
		flushSoon(user.getJavaUUID(), user.getPlayerName());
	}

	/**
	 * Call this from your existing CoolDownCheck events:
	 * - PlayerVoteSiteCoolDownEndEvent -> COOLDOWN_END_ANY_SITE (include sitename/url placeholders)
	 * - PlayerVoteCoolDownEndEvent     -> COOLDOWN_END_ALL_SITES
	 */
	public void onCooldownTrigger(VotingPluginUser user, VoteReminderType type, Map<String, String> placeholders) {
		if (!isEnabled() || user == null) {
			return;
		}
		if (type != VoteReminderType.COOLDOWN_END_ANY_SITE && type != VoteReminderType.COOLDOWN_END_ALL_SITES) {
			return;
		}

		Player player = Bukkit.getPlayer(user.getJavaUUID());
		if (player == null || !player.isOnline()) {
			return;
		}
		if (plugin.getOptions().isTreatVanishAsOffline() && user.isVanished()) {
			return;
		}

		queueTrigger(user, player, type, placeholders);
		flushSoon(user.getJavaUUID(), user.getPlayerName());
	}

	/*
	 * ========================= Tasks
	 * =========================
	 */

	private void startTasks() {
		if (!isEnabled()) {
			return;
		}

		// Every minute tick: interval detection
		minuteTaskId = Bukkit.getScheduler().scheduleSyncRepeatingTask(plugin, new Runnable() {
			@Override
			public void run() {
				if (!isEnabled()) {
					return;
				}
				fireIntervalTick();
			}
		}, 20L, 20L * 60L);
	}

	private void stopTasks() {
		if (minuteTaskId != -1) {
			Bukkit.getScheduler().cancelTask(minuteTaskId);
			minuteTaskId = -1;
		}
	}

	private void fireIntervalTick() {
		long nowMs = System.currentTimeMillis();
		long nowMinute = nowMs / 60000L;

		boolean anyIntervalMatched = false;

		// Quick scan: if no INTERVAL reminders exist, skip entirely
		for (VoteReminderDefinition def : reminders) {
			if (def.getType() == VoteReminderType.INTERVAL) {
				anyIntervalMatched = true;
				break;
			}
		}
		if (!anyIntervalMatched) {
			return;
		}

		for (VoteReminderDefinition def : reminders) {
			if (def.getType() != VoteReminderType.INTERVAL) {
				continue;
			}

			long intervalMs = safeMs(def.getInterval());
			if (intervalMs <= 0) {
				continue;
			}

			long intervalMinutes = Math.max(1L, intervalMs / 60000L);
			if ((nowMinute % intervalMinutes) != 0) {
				continue;
			}

			for (Player p : Bukkit.getOnlinePlayers()) {
				VotingPluginUser u = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
				if (u == null) {
					continue;
				}
				if (plugin.getOptions().isTreatVanishAsOffline() && u.isVanished()) {
					continue;
				}

				queueTrigger(u, p, VoteReminderType.INTERVAL, null);
				flushSoon(u.getJavaUUID(), u.getPlayerName());
			}
		}
	}

	/*
	 * ========================= Queue + flush (smart multi-trigger handling)
	 * =========================
	 */

	private void queueTrigger(VotingPluginUser user, Player player, VoteReminderType type,
			Map<String, String> placeholders) {
		UUID uuid = user.getJavaUUID();

		PendingTriggers pt = pending.computeIfAbsent(uuid, k -> new PendingTriggers());
		switch (type) {
		case LOGIN:
			pt.login = true;
			break;
		case FIRST_JOIN:
			pt.firstJoin = true;
			break;
		case VOTE_CAST:
			pt.voteCast = true;
			break;
		case COOLDOWN_END_ANY_SITE:
			pt.cooldownAny = true;
			break;
		case COOLDOWN_END_ALL_SITES:
			pt.cooldownAll = true;
			break;
		case INTERVAL:
			pt.interval = true;
			break;
		default:
			break;
		}
		pt.mergePlaceholders(placeholders);
	}

	private void flushSoon(UUID uuid, String playerName) {
		// one-tick coalesce
		Bukkit.getScheduler().runTask(plugin, () -> flush(uuid, playerName));
	}

	private void flush(UUID uuid, String playerName) {
		if (!isEnabled()) {
			return;
		}

		Player player = Bukkit.getPlayer(uuid);
		if (player == null || !player.isOnline()) {
			pending.remove(uuid);
			return;
		}

		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
		if (user == null) {
			pending.remove(uuid);
			return;
		}
		if (plugin.getOptions().isTreatVanishAsOffline() && user.isVanished()) {
			pending.remove(uuid);
			return;
		}

		PendingTriggers pt = pending.remove(uuid);
		if (pt == null) {
			return;
		}

		List<VoteReminderType> types = pt.snapshotTypes();
		plugin.extraDebug("[VoteReminders] flush " + user.getPlayerName() + " triggers=" + types);

		// Evaluate reminders in priority order, but only those whose type exists in this flush.
		for (VoteReminderDefinition def : reminders) {
			if (!types.contains(def.getType())) {
				continue;
			}

			boolean matched = attemptOrSchedule(user, player, def, pt.placeholders);

			if (matched) {
				plugin.extraDebug("[VoteReminders] matched " + def.getName() + " for " + user.getPlayerName()
						+ " type=" + def.getType() + " stopAfterMatch=" + options.isStopAfterMatch());
				if (options.isStopAfterMatch()) {
					return;
				}
			}
		}
	}

	/*
	 * ========================= Config parsing
	 * =========================
	 */

	private void loadConfig() {
		// VoteReminderOptions
		boolean enabled = plugin.getConfig().getBoolean("VoteReminderOptions.Enabled", true);
		boolean stopAfterMatch = plugin.getConfig().getBoolean("VoteReminderOptions.StopAfterMatch", true);
		ParsedDuration globalCooldown = ParsedDuration
				.parse(plugin.getConfig().getString("VoteReminderOptions.GlobalCooldown", ""));
		int defaultPriority = plugin.getConfig().getInt("VoteReminderOptions.DefaultPriority", 0);

		ParsedDuration defaultCooldown = ParsedDuration
				.parse(plugin.getConfig().getString("VoteReminderOptions.Defaults.Cooldown", ""));
		ParsedDuration defaultDelay = ParsedDuration
				.parse(plugin.getConfig().getString("VoteReminderOptions.Defaults.Delay", ""));

		VoteReminderConditions defaultsConditions = new VoteReminderConditions();

		if (plugin.getConfig().contains("VoteReminderOptions.Defaults.Conditions.CanVoteAny")) {
			defaultsConditions.setCanVoteAny(
					plugin.getConfig().getBoolean("VoteReminderOptions.Defaults.Conditions.CanVoteAny"));
		}
		if (plugin.getConfig().contains("VoteReminderOptions.Defaults.Conditions.CanVoteAll")) {
			defaultsConditions.setCanVoteAll(
					plugin.getConfig().getBoolean("VoteReminderOptions.Defaults.Conditions.CanVoteAll"));
		}
		if (plugin.getConfig().contains("VoteReminderOptions.Defaults.Conditions.MinOnlineTime")) {
			defaultsConditions.setMinOnlineTime(ParsedDuration.parse(
					plugin.getConfig().getString("VoteReminderOptions.Defaults.Conditions.MinOnlineTime", "")));
		}
		if (plugin.getConfig().contains("VoteReminderOptions.Defaults.Conditions.FirstJoin")) {
			defaultsConditions.setFirstJoin(
					plugin.getConfig().getBoolean("VoteReminderOptions.Defaults.Conditions.FirstJoin"));
		}

		// Default rewards full path (RewardBuilder path)
		String defaultRewardsPath = "VoteReminderOptions.Defaults.Rewards";

		this.options = new VoteReminderOptions(enabled, stopAfterMatch, globalCooldown, defaultPriority, defaultCooldown,
				defaultDelay, defaultsConditions, defaultRewardsPath);

		// VoteReminders (map)
		List<VoteReminderDefinition> defs = new ArrayList<>();
		Map<String, VoteReminderDefinition> by = new HashMap<>();

		if (plugin.getConfig().isConfigurationSection("VoteReminders")) {
			for (String key : plugin.getConfig().getConfigurationSection("VoteReminders").getKeys(false)) {
				VoteReminderDefinition def = loadOneReminder(key);
				if (def != null) {
					defs.add(def);
				}
			}
		}

		defs.sort(Comparator.comparingInt(VoteReminderDefinition::getPriority).reversed()
				.thenComparing(VoteReminderDefinition::getName, String.CASE_INSENSITIVE_ORDER));

		for (VoteReminderDefinition d : defs) {
			by.put(d.getName(), d);
		}

		this.reminders = Collections.unmodifiableList(defs);
		this.byName = Collections.unmodifiableMap(by);

		plugin.extraDebug("[VoteReminders] loaded reminders=" + reminders.size());
	}

	private VoteReminderDefinition loadOneReminder(String name) {
		String base = "VoteReminders." + name + ".";

		String typeRaw = plugin.getConfig().getString(base + "Type", "INTERVAL");
		VoteReminderType type;
		try {
			type = VoteReminderType.valueOf(typeRaw.toUpperCase(Locale.ROOT));
		} catch (Exception e) {
			plugin.getLogger().warning("[VoteReminders] Invalid Type for " + name + ": " + typeRaw);
			return null;
		}

		int priority = plugin.getConfig().contains(base + "Priority") ? plugin.getConfig().getInt(base + "Priority")
				: options.getDefaultPriority();

		ParsedDuration cooldown = ParsedDuration.parse(plugin.getConfig().getString(base + "Cooldown", ""));
		if (cooldown.isEmpty()) {
			cooldown = options.getDefaultCooldown();
		}

		ParsedDuration delay = ParsedDuration.parse(plugin.getConfig().getString(base + "Delay", ""));
		if (delay.isEmpty()) {
			delay = options.getDefaultDelay();
		}

		ParsedDuration interval = ParsedDuration.parse(plugin.getConfig().getString(base + "Interval", ""));

		if (type == VoteReminderType.INTERVAL && (interval == null || interval.isEmpty())) {
			plugin.getLogger().warning("[VoteReminders] INTERVAL reminder missing Interval: " + name);
			return null;
		}

		VoteReminderConditions cond = mergeConditions(options.getDefaultsConditions(), loadConditions(base + "Conditions."));

		// Full rewards path (if reminder has Rewards section/list, use it; else defaults path)
		String rewardsPath = options.getDefaultRewardsPath();
		if (plugin.getConfig().isConfigurationSection(base + "Rewards") || plugin.getConfig().isList(base + "Rewards")) {
			rewardsPath = base + "Rewards";
		}

		plugin.extraDebug("[VoteReminders] loaded " + name + " type=" + type + " priority=" + priority + " rewardsPath="
				+ rewardsPath);

		return new VoteReminderDefinition(name, type, priority, cooldown, delay, rewardsPath, cond, interval);
	}

	private VoteReminderConditions loadConditions(String base) {
		VoteReminderConditions c = new VoteReminderConditions();

		if (plugin.getConfig().contains(base + "CanVoteAny")) {
			c.setCanVoteAny(plugin.getConfig().getBoolean(base + "CanVoteAny"));
		}
		if (plugin.getConfig().contains(base + "CanVoteAll")) {
			c.setCanVoteAll(plugin.getConfig().getBoolean(base + "CanVoteAll"));
		}
		if (plugin.getConfig().contains(base + "MinOnlineTime")) {
			c.setMinOnlineTime(ParsedDuration.parse(plugin.getConfig().getString(base + "MinOnlineTime", "")));
		}
		if (plugin.getConfig().contains(base + "FirstJoin")) {
			c.setFirstJoin(plugin.getConfig().getBoolean(base + "FirstJoin"));
		}

		return c;
	}

	private VoteReminderConditions mergeConditions(VoteReminderConditions base, VoteReminderConditions override) {
		VoteReminderConditions out = new VoteReminderConditions();

		out.setCanVoteAny(override.getCanVoteAny() != null ? override.getCanVoteAny() : base.getCanVoteAny());
		out.setCanVoteAll(override.getCanVoteAll() != null ? override.getCanVoteAll() : base.getCanVoteAll());
		out.setFirstJoin(override.getFirstJoin() != null ? override.getFirstJoin() : base.getFirstJoin());

		if (override.getMinOnlineTime() != null && !override.getMinOnlineTime().isEmpty()) {
			out.setMinOnlineTime(override.getMinOnlineTime());
		} else {
			out.setMinOnlineTime(base.getMinOnlineTime());
		}

		return out;
	}

	/*
	 * ========================= Trigger processing
	 * =========================
	 */

	/**
	 * Delay scheduling counts as a match for StopAfterMatch.
	 */
	private boolean attemptOrSchedule(VotingPluginUser user, Player player, VoteReminderDefinition def,
			Map<String, String> placeholders) {

		if (!isEnabled()) {
			return false;
		}

		plugin.extraDebug("[VoteReminders] attempt " + def.getName() + " player=" + user.getPlayerName() + " type="
				+ def.getType());

		if (!isUserReminderEnabled(user)) {
			plugin.extraDebug("[VoteReminders] gate disabled-reminders-map for " + user.getPlayerName());
			return false;
		}
		if (!hasBaseReminderPermission(user)) {
			plugin.extraDebug("[VoteReminders] gate base-permission for " + user.getPlayerName());
			return false;
		}

		long delayMs = safeMs(def.getDelay());
		if (delayMs > 0) {
			plugin.extraDebug("[VoteReminders] schedule delay " + def.getName() + " for " + user.getPlayerName()
					+ " delayMs=" + delayMs);
			scheduleDelayedEvaluation(user.getJavaUUID(), def.getName(), placeholders, delayMs);
			return true;
		}

		return attemptFireNow(user, player, def, placeholders);
	}

	private void scheduleDelayedEvaluation(UUID uuid, String reminderName, Map<String, String> placeholders,
			long delayMs) {
		long ticks = Math.max(1L, delayMs / 50L);
		Map<String, String> ph = placeholders == null ? null : new HashMap<>(placeholders);

		Bukkit.getScheduler().runTaskLater(plugin, () -> {
			Player p = Bukkit.getPlayer(uuid);
			if (p == null || !p.isOnline()) {
				return;
			}

			VotingPluginUser u = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
			if (u == null) {
				return;
			}
			if (plugin.getOptions().isTreatVanishAsOffline() && u.isVanished()) {
				return;
			}

			VoteReminderDefinition def = byName.get(reminderName);
			if (def == null) {
				return;
			}

			plugin.extraDebug("[VoteReminders] delayed eval " + def.getName() + " for " + u.getPlayerName());
			attemptFireNow(u, p, def, ph);
		}, ticks);
	}

	private boolean attemptFireNow(VotingPluginUser user, Player player, VoteReminderDefinition def,
			Map<String, String> placeholders) {

		// Old VoteReminding behaviour gates
		if (!user.shouldBeReminded()) {
			plugin.extraDebug("[VoteReminders] gate shouldBeReminded=false " + user.getPlayerName() + " def="
					+ def.getName());
			return false;
		}

		// Conditions
		if (!passesConditions(user, player, def.getConditions())) {
			plugin.extraDebug("[VoteReminders] gate conditions=false " + user.getPlayerName() + " def=" + def.getName());
			return false;
		}

		long now = System.currentTimeMillis();

		// Global gate (proxy-friendly-ish)
		if (!cooldowns.tryAcquireGlobal(user.getJavaUUID(), now)) {
			plugin.extraDebug("[VoteReminders] gate globalCooldown " + user.getPlayerName() + " def=" + def.getName());
			return false;
		}

		// Per reminder gate (max of cooldown+interval)
		if (!cooldowns.canFireReminder(user.getJavaUUID(), def.getName(), now, def.getCooldown(), def.getInterval())) {
			plugin.extraDebug("[VoteReminders] gate perReminderCooldown " + user.getPlayerName() + " def=" + def.getName());
			return false;
		}

		// Fire
		plugin.extraDebug("[VoteReminders] FIRE " + def.getName() + " -> rewardsPath=" + def.getRewardsPath() + " user="
				+ user.getPlayerName());
		giveRewardFromPath(user, def.getRewardsPath(), placeholders);

		// Mark fired
		cooldowns.markFired(user.getJavaUUID(), def.getName(), now);

		plugin.extraDebug("[VoteReminders] fired " + user.getPlayerName() + " via " + def.getName());
		return true;
	}

	/*
	 * ========================= Rewards
	 * =========================
	 */

	private void giveRewardFromPath(VotingPluginUser user, String rewardsPath, Map<String, String> placeholders) {
		RewardBuilder rb = new RewardBuilder(plugin.getConfig(), rewardsPath).setGiveOffline(false)
				.disableDefaultWorlds();

		rb.withPlaceHolder("sitesavailable", "" + user.getSitesNotVotedOn());

		if (placeholders != null) {
			for (Map.Entry<String, String> e : placeholders.entrySet()) {
				rb.withPlaceHolder(e.getKey(), e.getValue());
			}
		}

		rb.send(user);
	}

	/*
	 * ========================= Conditions
	 * =========================
	 */

	private boolean passesConditions(VotingPluginUser user, Player player, VoteReminderConditions c) {
		// CanVoteAny
		if (c.getCanVoteAny() != null) {
			boolean canAny = user.canVoteAny();
			if (c.getCanVoteAny().booleanValue() != canAny) {
				plugin.extraDebug("[VoteReminders] gate CanVoteAny " + user.getPlayerName() + " required="
						+ c.getCanVoteAny() + " actual=" + canAny);
				return false;
			}
		}

		// CanVoteAll
		if (c.getCanVoteAll() != null) {
			boolean canAll = user.canVoteAll();
			if (c.getCanVoteAll().booleanValue() != canAll) {
				plugin.extraDebug("[VoteReminders] gate CanVoteAll " + user.getPlayerName() + " required="
						+ c.getCanVoteAll() + " actual=" + canAll);
				return false;
			}
		}

		// MinOnlineTime
		ParsedDuration mot = c.getMinOnlineTime();
		if (mot != null && !mot.isEmpty()) {
			Long join = joinTimes.get(user.getJavaUUID());
			if (join != null) {
				long onlineMs = System.currentTimeMillis() - join.longValue();
				long need = safeMs(mot);
				if (onlineMs < need) {
					plugin.extraDebug("[VoteReminders] gate MinOnlineTime " + user.getPlayerName() + " onlineMs="
							+ onlineMs + " needMs=" + need);
					return false;
				}
			}
		}

		// FirstJoin
		if (c.getFirstJoin() != null) {
			boolean first = !player.hasPlayedBefore();
			if (c.getFirstJoin().booleanValue() != first) {
				plugin.extraDebug("[VoteReminders] gate FirstJoin " + user.getPlayerName() + " required=" + c.getFirstJoin()
						+ " actual=" + first);
				return false;
			}
		}

		return true;
	}

	/*
	 * ========================= Permission logic (matches old VoteReminding)
	 * =========================
	 */

	private boolean hasBaseReminderPermission(VotingPluginUser user) {
		return user.hasPermission("VotingPlugin.Login.RemindVotes") || user.hasPermission("VotingPlugin.Player");
	}

	private boolean isUserReminderEnabled(VotingPluginUser user) {
		// use later?
		return true;
	}

	/*
	 * ========================= Helpers
	 * =========================
	 */

	private long safeMs(ParsedDuration d) {
		if (d == null || d.isEmpty()) {
			return 0L;
		}
		return Math.max(0L, d.getMillis());
	}
}
