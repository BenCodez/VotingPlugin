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
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Player;
import org.bukkit.scheduler.BukkitTask;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.player.PlayerUtils;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/**
 * Core broadcast logic.
 *
 * Decides when to broadcast and builds lines and messages. Uses the existing
 * eligible online players behavior and respects disabled broadcasts.
 *
 * Backend servers only.
 */
public final class BroadcastHandler {

	private final VotingPluginMain plugin;

	/**
	 * Only used for day-boundary logic for first vote of day broadcasts.
	 */
	private final ZoneId zoneId;

	private volatile BroadcastSettings settings;

	private final ConcurrentHashMap<UUID, Instant> lastBroadcastAt = new ConcurrentHashMap<UUID, Instant>();

	private final ConcurrentHashMap<UUID, LinkedHashSet<String>> pendingSites = new ConcurrentHashMap<UUID, LinkedHashSet<String>>();
	private final ConcurrentHashMap<UUID, BukkitTask> pendingFlush = new ConcurrentHashMap<UUID, BukkitTask>();

	private final ConcurrentHashMap<UUID, LocalDate> firstVoteDay = new ConcurrentHashMap<UUID, LocalDate>();

	private final ConcurrentHashMap<UUID, LinkedHashSet<String>> intervalSites = new ConcurrentHashMap<UUID, LinkedHashSet<String>>();
	private volatile BukkitTask intervalTask;

	/**
	 * Constructs a new broadcast handler.
	 *
	 * @param plugin the main plugin instance
	 * @param settings the broadcast settings
	 * @param zoneId the time zone ID, or null to use the system default
	 */
	public BroadcastHandler(VotingPluginMain plugin, BroadcastSettings settings, ZoneId zoneId) {
		this.plugin = plugin;
		this.settings = settings;
		this.zoneId = zoneId == null ? ZoneId.systemDefault() : zoneId;

		rescheduleIntervalIfNeeded();
	}

	/**
	 * Updates the broadcast settings and reschedules the interval task when
	 * required.
	 *
	 * @param settings the new broadcast settings
	 */
	public void setSettings(BroadcastSettings settings) {
		this.settings = settings;
		rescheduleIntervalIfNeeded();
	}

	/**
	 * Handles a received vote.
	 *
	 * @param uuid the voted player's UUID
	 * @param playerName the player name, or null to resolve it from Bukkit
	 * @param siteName the vote site display name
	 * @param wasOnline whether the voted player was online when the vote was received
	 */
	public void broadcastVote(UUID uuid, String playerName, String siteName, boolean wasOnline) {
		BroadcastSettings currentSettings = settings;
		if (currentSettings == null || currentSettings.isDisabled()) {
			return;
		}

		recordInterval(uuid, siteName);

		String name = playerName == null || playerName.isEmpty() ? resolveName(uuid) : playerName;
		VoteBroadcastType type = currentSettings.getType();

		if (type == VoteBroadcastType.EVERY_VOTE_ONLINE_ONLY) {
			if (!wasOnline) {
				return;
			}
			broadcastNow(uuid, name, single(siteName), "vote_online_only", null);
			return;
		}

		if (type == VoteBroadcastType.EVERY_VOTE) {
			broadcastNow(uuid, name, single(siteName), "vote", null);
		} else if (type == VoteBroadcastType.COOLDOWN_PER_PLAYER) {
			if (checkAndMarkCooldown(uuid, currentSettings.getDuration())) {
				broadcastNow(uuid, name, single(siteName), "cooldown", null);
			}
		} else if (type == VoteBroadcastType.BATCH_WINDOW_PER_PLAYER) {
			bufferBatch(uuid, name, siteName, currentSettings.getDuration());
		} else if (type == VoteBroadcastType.FIRST_VOTE_OF_DAY) {
			if (markFirstVoteOfDay(uuid)) {
				broadcastNow(uuid, name, single(siteName), "first_day", null);
			}
		} else if (type == VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			// Handled by the scheduled interval task.
		}
	}

	/**
	 * Renders and broadcasts a message.
	 *
	 * All PlaceholderAPI placeholders are evaluated using the voted player before
	 * the message is sent to recipients.
	 *
	 * @param votedPlayerUuid the voted player's UUID, or null when no single player applies
	 * @param playerName the rendered player name
	 * @param sites the vote sites
	 * @param reason the broadcast reason
	 * @param extraContext additional format context, or null
	 */
	private void broadcastNow(UUID votedPlayerUuid, String playerName, List<String> sites, String reason,
			Map<String, String> extraContext) {
		BroadcastSettings currentSettings = settings;
		if (currentSettings == null || currentSettings.isDisabled()) {
			return;
		}

		List<String> cleaned = applyMaxSites(sites, currentSettings);
		cleaned.sort(new Comparator<String>() {
			@Override
			public int compare(String first, String second) {
				String firstValue = first == null ? "" : first.toLowerCase(Locale.ROOT);
				String secondValue = second == null ? "" : second.toLowerCase(Locale.ROOT);
				return firstValue.compareTo(secondValue);
			}
		});

		List<String> lines;
		if (extraContext == null) {
			lines = currentSettings.getFormat().render(playerName, cleaned, reason);
		} else {
			lines = currentSettings.getFormat().render(playerName, cleaned, reason, extraContext);
		}

		OfflinePlayer votedPlayer = votedPlayerUuid == null ? null : Bukkit.getOfflinePlayer(votedPlayerUuid);

		for (String line : lines) {
			String parsedLine = PlaceholderUtils.replacePlaceHolders(votedPlayer, line);
			broadcastToEligiblePlayers(parsedLine);
		}
	}

	/**
	 * Broadcasts a message that has already had PlaceholderAPI placeholders parsed.
	 *
	 * Placeholder parsing is intentionally not performed for each recipient because
	 * all placeholders must be based on the voted player. JSON-style click and hover
	 * formatting is still parsed for each recipient.
	 *
	 * @param broadcastMessage the parsed message
	 */
	private void broadcastToEligiblePlayers(String broadcastMessage) {
		if (broadcastMessage == null || broadcastMessage.isEmpty()) {
			return;
		}

		for (Player player : Bukkit.getOnlinePlayers()) {
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
			if (user == null || user.getDisableBroadcast()) {
				continue;
			}

			for (String firstSplit : broadcastMessage.split(Pattern.quote("%newline%"))) {
				for (String message : firstSplit.split(Pattern.quote("%NewLine%"))) {
					String colorizedMessage = MessageAPI.colorize(message);
					PlayerUtils.getServerHandle().sendMessage(player, PlaceholderUtils.parseJson(colorizedMessage));
				}
			}
		}

		Bukkit.getServer().getConsoleSender().sendMessage(MessageAPI.colorize(broadcastMessage));
	}

	/**
	 * Checks and updates a player's broadcast cooldown.
	 *
	 * @param uuid the player's UUID
	 * @param cooldown the cooldown duration
	 * @return true when the broadcast is allowed
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

	/**
	 * Adds a vote site to a player's pending batch.
	 *
	 * @param uuid the player's UUID
	 * @param playerName the player's name
	 * @param siteName the vote site
	 * @param window the batch window
	 */
	private void bufferBatch(UUID uuid, String playerName, String siteName, ParsedDuration window) {
		if (siteName != null && !siteName.isEmpty()) {
			LinkedHashSet<String> sites = pendingSites.get(uuid);
			if (sites == null) {
				LinkedHashSet<String> created = new LinkedHashSet<String>();
				sites = pendingSites.putIfAbsent(uuid, created);
				if (sites == null) {
					sites = created;
				}
			}

			synchronized (sites) {
				sites.add(siteName);
			}
		}

		if (!pendingFlush.containsKey(uuid)) {
			BukkitTask task = scheduleBatchFlush(uuid, playerName, window);
			BukkitTask previous = pendingFlush.putIfAbsent(uuid, task);
			if (previous != null) {
				task.cancel();
			}
		}
	}

	/**
	 * Schedules a pending batch flush.
	 *
	 * @param uuid the player's UUID
	 * @param playerName the player's name
	 * @param window the batch window
	 * @return the scheduled Bukkit task
	 */
	private BukkitTask scheduleBatchFlush(final UUID uuid, final String playerName, ParsedDuration window) {
		long delayMillis = window == null ? 1L : window.delayMillisFromNow();
		long ticks = Math.max(1L, delayMillis / 50L);

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

	/**
	 * Flushes a player's pending vote-site batch.
	 *
	 * @param uuid the player's UUID
	 * @param playerName the player's name
	 */
	private void flushBatch(UUID uuid, String playerName) {
		Set<String> sites = pendingSites.remove(uuid);
		if (sites == null || sites.isEmpty()) {
			return;
		}

		List<String> siteList;
		synchronized (sites) {
			siteList = new ArrayList<String>(sites);
		}

		broadcastNow(uuid, playerName, siteList, "batch", null);
	}

	/**
	 * Marks and checks a player's first vote of the current day.
	 *
	 * @param uuid the player's UUID
	 * @return true when this is the first vote of the day
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

	/**
	 * Reschedules the global interval summary task when required.
	 */
	private void rescheduleIntervalIfNeeded() {
		if (intervalTask != null) {
			intervalTask.cancel();
			intervalTask = null;
		}

		BroadcastSettings currentSettings = settings;
		if (currentSettings == null
				|| currentSettings.getType() != VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			return;
		}

		long delayMillis = currentSettings.getDuration() == null
				? 1L
				: currentSettings.getDuration().delayMillisFromNow();
		long ticks = Math.max(1L, delayMillis / 50L);

		intervalTask = Bukkit.getScheduler().runTaskTimer(plugin, new Runnable() {
			@Override
			public void run() {
				broadcastIntervalSummary();
			}
		}, ticks, ticks);
	}

	/**
	 * Records a vote site for the global interval summary.
	 *
	 * @param uuid the player's UUID
	 * @param siteName the vote site
	 */
	private void recordInterval(UUID uuid, String siteName) {
		if (siteName == null || siteName.isEmpty()) {
			return;
		}

		LinkedHashSet<String> sites = intervalSites.get(uuid);
		if (sites == null) {
			LinkedHashSet<String> created = new LinkedHashSet<String>();
			sites = intervalSites.putIfAbsent(uuid, created);
			if (sites == null) {
				sites = created;
			}
		}

		synchronized (sites) {
			sites.add(siteName);
		}
	}

	/**
	 * Broadcasts the global interval summary.
	 *
	 * The interval summary has no single voted player, so PlaceholderAPI
	 * placeholders are left unchanged.
	 */
	private void broadcastIntervalSummary() {
		BroadcastSettings currentSettings = settings;
		if (currentSettings == null || currentSettings.isDisabled()
				|| currentSettings.getType() != VoteBroadcastType.INTERVAL_SUMMARY_GLOBAL) {
			return;
		}

		ConcurrentHashMap<UUID, LinkedHashSet<String>> snapshot =
				new ConcurrentHashMap<UUID, LinkedHashSet<String>>(intervalSites);
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

		broadcastNow(null, "Server", entries, "interval", context);
	}

	/**
	 * Creates a single-value site list.
	 *
	 * @param site the site name
	 * @return the site list
	 */
	private List<String> single(String site) {
		List<String> sites = new ArrayList<String>();
		if (site != null && !site.isEmpty()) {
			sites.add(site);
		}
		return sites;
	}

	/**
	 * Applies the configured maximum number of listed sites.
	 *
	 * @param sites the original site list
	 * @param currentSettings the current broadcast settings
	 * @return the limited site list
	 */
	private List<String> applyMaxSites(List<String> sites, BroadcastSettings currentSettings) {
		if (sites == null) {
			return new ArrayList<String>();
		}

		int maximum = currentSettings.getMaxSitesListed();
		if (maximum <= 0 || sites.size() <= maximum) {
			return new ArrayList<String>(sites);
		}

		return new ArrayList<String>(sites.subList(0, maximum));
	}

	/**
	 * Resolves a player's name from Bukkit.
	 *
	 * @param uuid the player's UUID
	 * @return the resolved player name
	 */
	private String resolveName(UUID uuid) {
		try {
			OfflinePlayer offlinePlayer = Bukkit.getOfflinePlayer(uuid);
			String name = offlinePlayer == null ? null : offlinePlayer.getName();
			return name == null || name.isEmpty() ? "Player" : name;
		} catch (Exception exception) {
			return "Player";
		}
	}

	/**
	 * Joins non-empty strings with a comma and space.
	 *
	 * @param values the values to join
	 * @return the joined string
	 */
	private String join(List<String> values) {
		StringJoiner joiner = new StringJoiner(", ");
		for (String value : values) {
			if (value != null && !value.isEmpty()) {
				joiner.add(value);
			}
		}
		return joiner.toString();
	}
}