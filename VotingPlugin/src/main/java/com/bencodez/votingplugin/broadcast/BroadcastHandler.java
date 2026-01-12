package com.bencodez.votingplugin.broadcast;

import java.time.Instant;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.StringJoiner;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.bukkit.scheduler.BukkitTask;

import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Core broadcast logic.
 *
 * Decides WHEN to broadcast and builds lines/messages. Uses your existing
 * "eligible online players" behavior (disable broadcast respected).
 *
 * NOTE: Backend servers only.
 */
public final class BroadcastHandler {

	private final VotingPluginMain plugin;

	/**
	 * Only used for day-boundary logic (FIRST_VOTE_OF_DAY).
	 */
	private final ZoneId zoneId;

	private volatile BroadcastSettings settings;

	// Per-player cooldown tracking
	private final ConcurrentHashMap<UUID, Instant> lastBroadcastAt = new ConcurrentHashMap<UUID, Instant>();

	// Batch window tracking
	private final ConcurrentHashMap<UUID, LinkedHashSet<String>> pendingSites = new ConcurrentHashMap<UUID, LinkedHashSet<String>>();
	private final ConcurrentHashMap<UUID, BukkitTask> pendingFlush = new ConcurrentHashMap<UUID, BukkitTask>();

	// First vote of day tracking
	private final ConcurrentHashMap<UUID, LocalDate> firstVoteDay = new ConcurrentHashMap<UUID, LocalDate>();

	// Interval summary tracking (uuid -> set of sites during interval)
	private final ConcurrentHashMap<UUID, LinkedHashSet<String>> intervalSites = new ConcurrentHashMap<UUID, LinkedHashSet<String>>();
	private volatile BukkitTask intervalTask;

	public BroadcastHandler(VotingPluginMain plugin, BroadcastSettings settings, ZoneId zoneId) {
		this.plugin = plugin;
		this.settings = settings;
		this.zoneId = zoneId == null ? ZoneId.systemDefault() : zoneId;

		rescheduleIntervalIfNeeded();
	}

	public void setSettings(BroadcastSettings settings) {
		this.settings = settings;
		rescheduleIntervalIfNeeded();
	}

	/**
	 * Call whenever a vote is received.
	 *
	 * @param uuid       player's uuid
	 * @param playerName player name (optional; if null/empty, resolved from Bukkit)
	 * @param siteName   vote site name (display name)
	 */
	public void broadcastVote(UUID uuid, String playerName, String siteName, boolean wasOnline) {
		BroadcastSettings s = settings;
		if (s == null || s.isDisabled()) {
			return;
		}

		// Always collect for interval summaries (even if Type isn't interval right
		// now).
		recordInterval(uuid, siteName);

		String name = playerName == null || playerName.isEmpty() ? resolveName(uuid) : playerName;

		VoteBroadcastType type = s.getType();

		// New: types that only broadcast if the voting player is online
		if (type == VoteBroadcastType.EVERY_VOTE_ONLINE_ONLY) {
			if (!wasOnline) {
				return;
			}
			broadcastNow(name, single(siteName), "vote_online_only", null);
			return;
		}

		if (type == VoteBroadcastType.EVERY_VOTE) {
			broadcastNow(name, single(siteName), "vote", null);

		} else if (type == VoteBroadcastType.COOLDOWN_PER_PLAYER) {
			if (checkAndMarkCooldown(uuid, s.getDuration())) {
				broadcastNow(name, single(siteName), "cooldown", null);
			}

		} else if (type == VoteBroadcastType.BATCH_WINDOW_PER_PLAYER) {
			bufferBatch(uuid, name, siteName, s.getDuration());

		} else if (type == VoteBroadcastType.FIRST_VOTE_OF_DAY) {
			if (markFirstVoteOfDay(uuid)) {
				broadcastNow(name, single(siteName), "first_day", null);
			}

		} else if (type == VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			// handled by scheduled task
		}
	}

	/*
	 * ======================= Broadcast helpers =======================
	 */

	private void broadcastNow(String playerName, List<String> sites, String reason, Map<String, String> extraContext) {
		BroadcastSettings s = settings;
		if (s == null || s.isDisabled()) {
			return;
		}

		List<String> cleaned = applyMaxSites(sites, s);
		cleaned.sort(new Comparator<String>() {
			@Override
			public int compare(String a, String b) {
				String aa = a == null ? "" : a.toLowerCase(Locale.ROOT);
				String bb = b == null ? "" : b.toLowerCase(Locale.ROOT);
				return aa.compareTo(bb);
			}
		});

		List<String> lines;
		if (extraContext == null) {
			lines = s.getFormat().render(playerName, cleaned, reason);
		} else {
			lines = s.getFormat().render(playerName, cleaned, reason, extraContext);
		}

		for (String line : lines) {
			broadcastToEligiblePlayers(line);
		}
	}

	/**
	 * Your reliable broadcast style: - only sends to online players - respects
	 * per-player disable broadcast toggle - runs PlaceholderAPI style per-player
	 * placeholders (if your PlaceholderUtils does) - supports %newline% / %NewLine%
	 * splitting
	 */
	private void broadcastToEligiblePlayers(String broadcastMsg) {
		if (broadcastMsg == null || broadcastMsg.isEmpty()) {
			return;
		}

		ArrayList<Player> players = new ArrayList<Player>();
		for (Player p : Bukkit.getOnlinePlayers()) {
			VotingPluginUser u = plugin.getVotingPluginUserManager().getVotingPluginUser(p);
			if (u != null && !u.getDisableBroadcast()) {
				players.add(p);
			}
		}

		// If you prefer your manual loop, keep it here (not in main)
		MiscUtils.getInstance().broadcast(broadcastMsg, players);

		Bukkit.getServer().getConsoleSender().sendMessage(MessageAPI.colorize(broadcastMsg));
	}

	/*
	 * ======================= Cooldown logic (fixed millis) =======================
	 */

	private boolean checkAndMarkCooldown(UUID uuid, ParsedDuration cooldown) {
		if (cooldown == null || cooldown.isEmpty()) {
			return true;
		}

		Instant now = Instant.now();
		Instant last = lastBroadcastAt.get(uuid);

		if (last == null) {
			lastBroadcastAt.put(uuid, now);
			return true;
		}

		Instant next = cooldown.addTo(last);
		if (!now.isBefore(next)) {
			lastBroadcastAt.put(uuid, now);
			return true;
		}

		return false;
	}

	/*
	 * ======================= Batch window logic (fixed millis)
	 * =======================
	 */

	private void bufferBatch(UUID uuid, String playerName, String siteName, ParsedDuration window) {
		if (siteName != null && !siteName.isEmpty()) {
			LinkedHashSet<String> set = pendingSites.get(uuid);
			if (set == null) {
				LinkedHashSet<String> created = new LinkedHashSet<String>();
				set = pendingSites.putIfAbsent(uuid, created);
				if (set == null) {
					set = created;
				}
			}
			synchronized (set) {
				set.add(siteName);
			}
		}

		if (!pendingFlush.containsKey(uuid)) {
			BukkitTask task = scheduleBatchFlush(uuid, playerName, window);
			BukkitTask prev = pendingFlush.putIfAbsent(uuid, task);
			if (prev != null) {
				task.cancel();
			}
		}
	}

	private BukkitTask scheduleBatchFlush(final UUID uuid, final String playerName, ParsedDuration window) {
		long delayMs = window == null ? 1L : window.delayMillisFromNow();
		long ticks = Math.max(1L, delayMs / 50L);

		return Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {
			@Override
			public void run() {
				try {
					flushBatch(uuid, playerName);
				} finally {
					pendingFlush.remove(uuid);
				}
			}
		}, ticks);
	}

	private void flushBatch(UUID uuid, String playerName) {
		Set<String> set = pendingSites.remove(uuid);
		if (set == null || set.isEmpty()) {
			return;
		}

		List<String> list;
		synchronized (set) {
			list = new ArrayList<String>(set);
		}
		broadcastNow(playerName, list, "batch", null);
	}

	/*
	 * ======================= First vote of day (calendar) =======================
	 */

	private boolean markFirstVoteOfDay(UUID uuid) {
		LocalDate today = LocalDate.now(zoneId);
		LocalDate last = firstVoteDay.putIfAbsent(uuid, today);

		if (last == null) {
			return true;
		}
		if (!last.equals(today)) {
			firstVoteDay.put(uuid, today);
			return true;
		}
		return false;
	}

	/*
	 * ======================= Interval summary =======================
	 */

	private void rescheduleIntervalIfNeeded() {
		if (intervalTask != null) {
			intervalTask.cancel();
			intervalTask = null;
		}

		BroadcastSettings s = settings;
		if (s == null || s.getType() != VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			return;
		}

		long delayMs = s.getDuration() == null ? 1L : s.getDuration().delayMillisFromNow();
		long ticks = Math.max(1L, delayMs / 50L);

		intervalTask = Bukkit.getScheduler().runTaskTimer(plugin, new Runnable() {
			@Override
			public void run() {
				broadcastIntervalSummary();
			}
		}, ticks, ticks);
	}

	private void recordInterval(UUID uuid, String siteName) {
		if (siteName == null || siteName.isEmpty()) {
			return;
		}

		LinkedHashSet<String> set = intervalSites.get(uuid);
		if (set == null) {
			LinkedHashSet<String> created = new LinkedHashSet<String>();
			set = intervalSites.putIfAbsent(uuid, created);
			if (set == null) {
				set = created;
			}
		}
		synchronized (set) {
			set.add(siteName);
		}
	}

	/**
	 * Interval summary supports extra context placeholders: - %players% /
	 * %numberofplayers% - %sites% / %numberofsites%
	 */
	private void broadcastIntervalSummary() {
		BroadcastSettings s = settings;
		if (s == null || s.isDisabled() || s.getType() != VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			return;
		}

		ConcurrentHashMap<UUID, LinkedHashSet<String>> snapshot = new ConcurrentHashMap<UUID, LinkedHashSet<String>>(
				intervalSites);
		intervalSites.clear();

		if (snapshot.isEmpty()) {
			return;
		}

		List<String> entries = new ArrayList<String>();
		List<String> players = new ArrayList<String>();
		LinkedHashSet<String> uniqueSites = new LinkedHashSet<String>();

		for (Map.Entry<UUID, LinkedHashSet<String>> entry : snapshot.entrySet()) {
			LinkedHashSet<String> siteSet = entry.getValue();
			if (siteSet == null || siteSet.isEmpty()) {
				continue;
			}

			String name = resolveName(entry.getKey());
			players.add(name);

			int siteCount;
			synchronized (siteSet) {
				siteCount = siteSet.size();
				for (String site : siteSet) {
					if (site != null && !site.isEmpty()) {
						uniqueSites.add(site);
					}
				}
			}

			entries.add(name + " (" + siteCount + ")");
		}

		if (entries.isEmpty()) {
			return;
		}

		String playersCsv = join(players);
		String sitesCsv = join(new ArrayList<String>(uniqueSites));

		ConcurrentHashMap<String, String> context = new ConcurrentHashMap<String, String>();
		context.put("players", playersCsv);
		context.put("numberofplayers", String.valueOf(players.size()));
		context.put("sites", sitesCsv);
		context.put("numberofsites", String.valueOf(uniqueSites.size()));

		broadcastNow("Server", entries, "interval", context);
	}

	/*
	 * ======================= Utilities =======================
	 */

	private List<String> single(String site) {
		List<String> list = new ArrayList<String>();
		if (site != null && !site.isEmpty()) {
			list.add(site);
		}
		return list;
	}

	private List<String> applyMaxSites(List<String> sites, BroadcastSettings s) {
		if (sites == null) {
			return new ArrayList<String>();
		}
		int max = s.getMaxSitesListed();
		if (max <= 0 || sites.size() <= max) {
			return new ArrayList<String>(sites);
		}
		return new ArrayList<String>(sites.subList(0, max));
	}

	private String resolveName(UUID uuid) {
		try {
			OfflinePlayer op = Bukkit.getOfflinePlayer(uuid);
			String name = op == null ? null : op.getName();
			return name == null || name.isEmpty() ? "Player" : name;
		} catch (Exception e) {
			return "Player";
		}
	}

	private String join(List<String> list) {
		StringJoiner joiner = new StringJoiner(", ");
		for (String s : list) {
			if (s != null && !s.isEmpty()) {
				joiner.add(s);
			}
		}
		return joiner.toString();
	}
}
