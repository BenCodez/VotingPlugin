package com.bencodez.votingplugin.control;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Pattern;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.plugin.Plugin;

import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.util.ServiceSiteValidator;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServiceHealth;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServerCount;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.ServiceCount;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogCounts;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogEntry;
import com.bencodez.votingplugin.votelog.VoteLogMysqlTable.VoteLogEvent;
import com.bencodez.votingplugin.votesites.VoteSite;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;

/**
 * Typed, read-only data surface used by VotingPlugin Control.
 *
 * <p>This class deliberately exposes a fixed query allow-list rather than SQL,
 * commands, filesystem access, or user enumeration. All filters and result
 * counts are bounded. Reward and vote-site simulations only describe what
 * would happen; they never create a site, execute a reward, or update data.</p>
 */
public final class ControlInspectionService {
	public static final int SCHEMA_VERSION = 1;
	public static final int MAX_DATA_BYTES = 512 * 1024;
	private static final int MAX_ROWS = 100;
	private static final int MAX_TOP_ROWS = 20;
	private static final Comparator<ServiceHealth> SERVICE_HEALTH_ORDER = Comparator
			.comparingLong(ServiceHealth::lastVoteTime).reversed()
			.thenComparing(ServiceHealth::service, String.CASE_INSENSITIVE_ORDER)
			.thenComparing(ServiceHealth::service);
	private static final Pattern PLAYER_NAME = Pattern.compile("[A-Za-z0-9_]{1,16}");
	private static final Pattern SERVICE_NAME = Pattern.compile("[^\\p{Cntrl}]{1,64}");
	private static final Set<String> KINDS = Set.of("overview", "vote-site-health", "player",
			"vote-log-summary", "vote-log-search", "vote-trace", "vote-site-resolution",
			"reward-simulation", "diagnostics");

	private final VotingPluginMain plugin;

	public ControlInspectionService(VotingPluginMain plugin) {
		this.plugin = java.util.Objects.requireNonNull(plugin, "plugin");
	}

	/** Validation-only constructor used by unit tests for plugin-independent kinds. */
	ControlInspectionService() {
		this.plugin = null;
	}

	/** Executes one validated query on the caller's background thread. */
	public JsonObject inspect(JsonObject query) {
		if (query == null) throw new IllegalArgumentException("query is required");
		rejectUnknown(query, Set.of("kind", "filters"), "query");
		String kind = requiredString(query, "kind", 64);
		if (!KINDS.contains(kind)) throw new IllegalArgumentException("inspection kind is unsupported");
		JsonObject filters = optionalObject(query, "filters");
		JsonObject result = switch (kind) {
		case "overview" -> overview(filters);
		case "vote-site-health" -> voteSiteHealth(filters);
		case "player" -> player(filters);
		case "vote-log-summary" -> voteLogSummary(filters);
		case "vote-log-search" -> voteLogSearch(filters);
		case "vote-trace" -> voteTrace(filters);
		case "vote-site-resolution" -> voteSiteResolution(filters);
		case "reward-simulation" -> rewardSimulation(filters);
		case "diagnostics" -> diagnostics(filters);
		default -> throw new IllegalArgumentException("inspection kind is unsupported");
		};
		JsonObject envelope = new JsonObject();
		envelope.addProperty("schemaVersion", SCHEMA_VERSION);
		envelope.addProperty("kind", kind);
		envelope.addProperty("generatedAt", java.time.Instant.now().toString());
		envelope.add("result", result);
		ensureBounded(envelope);
		return envelope;
	}

	private JsonObject overview(JsonObject filters) {
		rejectUnknown(filters, Set.of(), "overview filters");
		VoteLogMysqlTable voteLog = plugin.getVoteLogMysqlTable();
		boolean voteLoggingEnabled = plugin.getConfigFile().isVoteLoggingEnabled();
		boolean voteLogAvailable = voteLoggingEnabled && voteLog != null;
		JsonObject result = new JsonObject();
		result.addProperty("pluginVersion", safe(plugin.getDescription().getVersion(), 80));
		result.addProperty("platform", "BUKKIT");
		result.addProperty("serverSoftware", safe(plugin.getServer().getName(), 80));
		result.addProperty("serverVersion", safe(plugin.getServer().getBukkitVersion(), 80));
		result.addProperty("configuredVoteSites", allConfiguredVoteSiteNames().size());
		result.addProperty("enabledVoteSites", loadedVoteSites().stream().filter(VoteSite::isEnabled).count());
		result.addProperty("autoCreateVoteSites", plugin.getConfigFile().isAutoCreateVoteSites());
		result.addProperty("processRewards", plugin.getConfigFile().getData().getBoolean("ProcessRewards", true));
		result.addProperty("dataStorage", safe(plugin.getConfigFile().getData().getString("DataStorage", ""), 32));
		result.addProperty("voteLoggingEnabled", voteLoggingEnabled);
		result.addProperty("voteLogAvailable", voteLogAvailable);
		result.addProperty("voteLogReadable", voteLogAvailable && voteLog.isReadable());
		result.addProperty("proxyMode", plugin.getBungeeSettings().isUseBungeecoord());
		result.addProperty("proxyMethod", safe(plugin.getBungeeSettings().getBungeeMethod(), 32));
		result.addProperty("votifierDetected", plugin.isVotifierLoaded());
		result.addProperty("configurationHealthy", !plugin.isYmlError());
		return result;
	}

	private JsonObject voteSiteHealth(JsonObject filters) {
		rejectUnknown(filters, Set.of("days"), "vote-site-health filters");
		int days = boundedInt(filters, "days", 30, 1, 365);
		ConfigurationSection root = plugin.getConfigVoteSites().getData().getConfigurationSection("VoteSites");
		List<String> configuredNames = allConfiguredVoteSiteNames();
		Set<String> configuredServices = new HashSet<>();
		Set<String> displayedServices = new java.util.LinkedHashSet<>();
		for (int index = 0; index < configuredNames.size(); index++) {
			ConfigurationSection site = root == null ? null : root.getConfigurationSection(configuredNames.get(index));
			if (site == null) continue;
			String service = site.getString("ServiceSite", "");
			if (service.isBlank()) continue;
			configuredServices.add(lower(service));
			if (index < MAX_ROWS) displayedServices.add(lower(service));
		}
		Map<String, ServiceHealth> logged = new HashMap<>();
		List<ServiceHealth> recentHealth = List.of();
		VoteLogMysqlTable table = plugin.getVoteLogMysqlTable();
		boolean voteLoggingEnabled = plugin.getConfigFile().isVoteLoggingEnabled();
		boolean voteLoggingAvailable = voteLoggingEnabled && table != null;
		boolean voteLogReadable = voteLoggingAvailable && table.isReadable();
		if (voteLogReadable) {
			recentHealth = normalizedServiceHealth(table.getServiceHealth(days, MAX_ROWS));
			for (ServiceHealth health : recentHealth) {
				logged.put(lower(health.service()), health);
			}
			for (ServiceHealth health : normalizedServiceHealth(
					table.getServiceHealthForServices(days, List.copyOf(displayedServices)))) {
				logged.put(lower(health.service()), health);
			}
		}
		JsonArray sites = new JsonArray();
		for (String name : configuredNames.stream().limit(MAX_ROWS).toList()) {
			ConfigurationSection site = root == null ? null : root.getConfigurationSection(name);
			if (site == null) continue;
			String fullService = site.getString("ServiceSite", "");
			String service = safe(fullService, 64);
			ServiceHealth health = logged.get(lower(fullService));
			JsonObject row = new JsonObject();
			row.addProperty("key", safe(name, 64));
			row.addProperty("displayName", safe(site.getString("Name", name), 100));
			row.addProperty("serviceSite", service);
			row.addProperty("enabled", site.getBoolean("Enabled", true));
			row.addProperty("hidden", site.getBoolean("Hidden", false));
			row.addProperty("priority", site.getInt("Priority", 5));
			row.addProperty("voteDelay", safe(String.valueOf(site.get("VoteDelay", "24h")), 80));
			row.addProperty("hasRewards", hasRewardConfiguration(site));
			if (voteLogReadable) addHealth(row, health);
			row.addProperty("status", !site.getBoolean("Enabled", true) ? "DISABLED"
					: fullService.isBlank() ? "SERVICE_SITE_MISSING"
					: !voteLoggingAvailable ? "VOTE_LOG_UNAVAILABLE"
					: !voteLogReadable ? "VOTE_LOG_UNREADABLE"
					: health == null ? "NO_RECENT_VOTES" : "ACTIVE");
			sites.add(row);
		}
		JsonArray unmatched = new JsonArray();
		recentHealth.stream().filter(health -> !configuredServices.contains(lower(health.service())))
				.sorted(SERVICE_HEALTH_ORDER).limit(MAX_ROWS)
				.forEach(health -> {
					JsonObject row = new JsonObject();
					row.addProperty("serviceSite", safe(health.service(), 64));
					addHealth(row, health);
					unmatched.add(row);
				});
		Map<String, String> detected = new java.util.TreeMap<>();
		for (String observed : plugin.getServerData().getServiceSitesReadOnly()) {
			String sanitized = safe(observed, 64);
			if (!sanitized.isBlank() && !configuredServices.contains(lower(observed))) {
				detected.putIfAbsent(lower(observed), sanitized);
			}
		}
		JsonArray detectedUnconfigured = new JsonArray();
		detected.values().stream().sorted(String.CASE_INSENSITIVE_ORDER).limit(MAX_ROWS)
				.forEach(detectedUnconfigured::add);
		JsonObject result = new JsonObject();
		result.addProperty("days", days);
		result.addProperty("voteLoggingEnabled", voteLoggingEnabled);
		result.addProperty("voteLoggingAvailable", voteLoggingAvailable);
		result.addProperty("voteLogReadable", voteLogReadable);
		result.addProperty("autoCreateVoteSites", plugin.getConfigFile().isAutoCreateVoteSites());
		result.add("sites", sites);
		result.add("unmatchedLoggedServices", unmatched);
		result.add("detectedUnconfiguredServices", detectedUnconfigured);
		result.addProperty("detectedUnconfiguredServicesTruncated", detected.size() > MAX_ROWS);
		result.addProperty("truncated", configuredNames.size() > MAX_ROWS || recentHealth.size() >= MAX_ROWS);
		return result;
	}

	private static List<ServiceHealth> normalizedServiceHealth(List<ServiceHealth> values) {
		Map<String, ServiceHealth> merged = new HashMap<>();
		if (values != null) {
			for (ServiceHealth value : values) {
				if (value == null || value.service() == null || value.service().isBlank()) continue;
				merged.merge(lower(value.service()), value, ControlInspectionService::mergeServiceHealth);
			}
		}
		return merged.values().stream().sorted(SERVICE_HEALTH_ORDER).toList();
	}

	private static ServiceHealth mergeServiceHealth(ServiceHealth left, ServiceHealth right) {
		String representative = left.service().compareTo(right.service()) <= 0 ? left.service() : right.service();
		return new ServiceHealth(representative, left.votes() + right.votes(),
				Math.max(left.lastVoteTime(), right.lastVoteTime()), left.immediate() + right.immediate(),
				left.cached() + right.cached());
	}

	private JsonObject player(JsonObject filters) {
		rejectUnknown(filters, Set.of("name", "uuid"), "player filters");
		boolean hasName = filters.has("name");
		boolean hasUuid = filters.has("uuid");
		if (hasName == hasUuid) throw new IllegalArgumentException("exactly one of name or uuid is required");
		VotingPluginUser user;
		if (hasUuid) {
			UUID uuid = parseUuid(requiredString(filters, "uuid", 36), "uuid");
			if (!plugin.getUserManager().userExist(uuid)) return notFound("player");
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, true);
		} else {
			String name = requiredString(filters, "name", 16);
			if (!PLAYER_NAME.matcher(name).matches()) throw new IllegalArgumentException("player name is invalid");
			if (!plugin.getUserManager().userExist(name)) return notFound("player");
			user = plugin.getVotingPluginUserManager().getVotingPluginUser(name);
		}
		user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		JsonObject result = new JsonObject();
		result.addProperty("found", true);
		result.addProperty("uuid", safe(user.getUUID(), 36));
		result.addProperty("name", safe(user.getPlayerName(), 16));
		result.addProperty("lastOnline", user.getLastOnline());
		result.addProperty("online", user.isOnline());
		JsonObject totals = new JsonObject();
		totals.addProperty("daily", user.getDailyTotal());
		totals.addProperty("weekly", user.getWeeklyTotal());
		totals.addProperty("monthly", user.getMonthTotal());
		totals.addProperty("allTime", user.getAllTimeTotal());
		result.add("totals", totals);
		result.addProperty("points", user.getPoints());
		JsonObject streaks = new JsonObject();
		streaks.addProperty("daily", user.getDayVoteStreak());
		streaks.addProperty("weekly", user.getWeekVoteStreak());
		streaks.addProperty("monthly", user.getMonthVoteStreak());
		result.add("streaks", streaks);
		result.addProperty("lastVoteTime", user.getLastVoteTime());
		Map<VoteSite, Long> lastVoteSnapshot = new HashMap<>(user.getLastVotes());
		JsonArray lastVotes = new JsonArray();
		lastVoteSnapshot.entrySet().stream().filter(entry -> entry.getKey() != null)
				.sorted(Comparator.comparing(
						(Map.Entry<VoteSite, Long> entry) -> safe(entry.getKey().getKey(), 64),
						String.CASE_INSENSITIVE_ORDER)
						.thenComparing(entry -> safe(entry.getKey().getKey(), 64)))
				.limit(MAX_ROWS).forEach(entry -> {
					VoteSite site = entry.getKey();
					JsonObject row = new JsonObject();
					row.addProperty("siteKey", safe(site.getKey(), 64));
					row.addProperty("displayName", safe(site.getDisplayName(), 100));
					row.addProperty("serviceSite", safe(site.getServiceSite(), 64));
					row.addProperty("time", entry.getValue() == null ? 0 : entry.getValue());
					lastVotes.add(row);
				});
		result.add("lastVotes", lastVotes);
		result.addProperty("lastVotesTruncated", lastVoteSnapshot.size() > MAX_ROWS);
		result.addProperty("pendingOfflineVotes", Math.min(user.getOfflineVotes().size(), 100000));
		return result;
	}

	private JsonObject voteLogSummary(JsonObject filters) {
		rejectUnknown(filters, Set.of("days"), "vote-log-summary filters");
		int days = boundedInt(filters, "days", 30, 1, 365);
		VoteLogMysqlTable table = requireVoteLog();
		VoteLogCounts counts = table.getCounts(days);
		JsonObject result = new JsonObject();
		result.addProperty("days", days);
		result.addProperty("total", counts.total);
		result.addProperty("immediate", counts.immediate);
		result.addProperty("cached", counts.cached);
		result.addProperty("uniqueVoters", table.getUniqueVoters(days));
		JsonArray services = new JsonArray();
		table.getTopServices(days, MAX_TOP_ROWS).stream()
				.sorted(Comparator.comparingLong((ServiceCount count) -> count.votes).reversed()
						.thenComparing(count -> safe(count.service, 64), String.CASE_INSENSITIVE_ORDER)
						.thenComparing(count -> safe(count.service, 64)))
				.forEach(count -> {
			JsonObject row = new JsonObject();
			row.addProperty("service", safe(count.service, 64));
			row.addProperty("votes", count.votes);
			services.add(row);
		});
		JsonArray servers = new JsonArray();
		table.getTopServers(days, MAX_TOP_ROWS).stream()
				.sorted(Comparator.comparingLong((ServerCount count) -> count.votes).reversed()
						.thenComparing(count -> safe(count.server, 64), String.CASE_INSENSITIVE_ORDER)
						.thenComparing(count -> safe(count.server, 64)))
				.forEach(count -> {
			JsonObject row = new JsonObject();
			row.addProperty("server", safe(count.server, 64));
			row.addProperty("votes", count.votes);
			servers.add(row);
		});
		result.add("topServices", services);
		result.add("topServers", servers);
		return result;
	}

	private JsonObject voteLogSearch(JsonObject filters) {
		rejectUnknown(filters, Set.of("player", "service", "server", "event", "days", "limit"),
				"vote-log-search filters");
		int days = boundedInt(filters, "days", 30, 1, 365);
		int limit = boundedInt(filters, "limit", 25, 1, MAX_ROWS);
		String player = optionalString(filters, "player", 16);
		String service = optionalString(filters, "service", 64);
		String server = optionalString(filters, "server", 64);
		long selectors = List.of(player, service, server).stream().filter(value -> !value.isBlank()).count();
		if (selectors > 1) throw new IllegalArgumentException("only one of player, service, or server may be filtered");
		if (!player.isBlank() && !PLAYER_NAME.matcher(player).matches()) {
			throw new IllegalArgumentException("player filter is invalid");
		}
		if (!service.isBlank() && !SERVICE_NAME.matcher(service).matches()) {
			throw new IllegalArgumentException("service filter is invalid");
		}
		if (!server.isBlank() && !SERVICE_NAME.matcher(server).matches()) {
			throw new IllegalArgumentException("server filter is invalid");
		}
		VoteLogEvent event = optionalEvent(filters);
		VoteLogMysqlTable table = requireVoteLog();
		List<VoteLogEntry> rows;
		if (!player.isBlank()) rows = table.getByPlayerName(player, event, days, limit);
		else if (!service.isBlank()) rows = table.getByService(service, event, days, limit);
		else if (!server.isBlank()) rows = table.getByServer(server, event, days, limit);
		else rows = table.getRecent(days, event, limit);
		JsonObject result = new JsonObject();
		result.addProperty("days", days);
		result.addProperty("limit", limit);
		result.add("entries", entries(rows));
		result.addProperty("truncated", rows.size() >= limit);
		return result;
	}

	private JsonObject voteTrace(JsonObject filters) {
		rejectUnknown(filters, Set.of("voteId", "days", "limit"), "vote-trace filters");
		String voteId = parseUuid(requiredString(filters, "voteId", 36), "voteId").toString();
		int days = boundedInt(filters, "days", 30, 1, 365);
		int limit = boundedInt(filters, "limit", 50, 1, MAX_ROWS);
		List<VoteLogEntry> rows = requireVoteLog().getByVoteIdAll(voteId, days, limit);
		rows = new ArrayList<>(rows);
		rows.sort(Comparator.comparingLong(entry -> entry.voteTime));
		JsonObject result = new JsonObject();
		result.addProperty("voteId", voteId);
		result.addProperty("found", !rows.isEmpty());
		result.add("events", entries(rows));
		result.addProperty("truncated", rows.size() >= limit);
		return result;
	}

	private JsonObject voteSiteResolution(JsonObject filters) {
		rejectUnknown(filters, Set.of("serviceSite", "includeDisabled"), "vote-site-resolution filters");
		String serviceSite = requiredString(filters, "serviceSite", 64);
		if (!ServiceSiteValidator.isValid(serviceSite)) {
			throw new IllegalArgumentException("serviceSite is invalid");
		}
		boolean includeDisabled = optionalBoolean(filters, "includeDisabled", false);
		String advancedMatch = serviceSite;
		if (plugin.getConfigFile().isAdvancedServiceSiteHandling() && plugin.getServiceSiteHandler() != null) {
			advancedMatch = plugin.getServiceSiteHandler().matchReverse(serviceSite);
		}
		String configuredName = plugin.getVoteSiteManager().getResolver()
				.getConfiguredVoteSiteName(serviceSite, advancedMatch);
		String resolvedName = plugin.getVoteSiteManager().getVoteSiteName(!includeDisabled,
				serviceSite, advancedMatch);
		VoteSite loaded = plugin.getVoteSiteManager().resolveVoteSite(resolvedName, !includeDisabled);
		String creationName = plugin.getVoteSiteManager().getVoteSiteName(false, serviceSite, advancedMatch);
		boolean unconfigured = !plugin.getVoteSiteManager().hasVoteSite(creationName)
				&& !plugin.getVoteSiteManager().hasConfiguredVoteSite(creationName);
		JsonObject result = new JsonObject();
		result.addProperty("serviceSite", serviceSite);
		result.addProperty("includeDisabled", includeDisabled);
		result.addProperty("matched", loaded != null || configuredName != null && includeDisabled);
		String key = loaded != null ? loaded.getKey() : includeDisabled ? configuredName : null;
		if (key != null) {
			ConfigurationSection section = plugin.getConfigVoteSites().getData()
					.getConfigurationSection("VoteSites." + key);
			result.addProperty("key", safe(key, 64));
			if (section != null) {
				result.addProperty("displayName", safe(section.getString("Name", key), 100));
				result.addProperty("configuredServiceSite", safe(section.getString("ServiceSite", ""), 64));
				result.addProperty("enabled", section.getBoolean("Enabled", true));
			}
		}
		result.addProperty("wouldAutoCreate", unconfigured && plugin.getConfigFile().isAutoCreateVoteSites());
		result.addProperty("sideEffects", false);
		return result;
	}

	private JsonObject rewardSimulation(JsonObject filters) {
		rejectUnknown(filters, Set.of("proposal"), "reward-simulation filters");
		ControlRewardProposal.Parsed proposal = ControlRewardProposal.parse(
				requiredString(filters, "proposal", ControlRewardProposal.MAX_JSON_BYTES));
		if (plugin != null && "site".equals(proposal.scope()) && !plugin.getConfigVoteSites().getData()
				.isConfigurationSection("VoteSites." + proposal.site())) {
			throw new IllegalArgumentException("reward proposal site is not configured");
		}
		JsonObject result = new JsonObject();
		result.addProperty("valid", true);
		result.addProperty("actionCount", proposal.actionCount());
		result.addProperty("wouldExecute", false);
		result.addProperty("sideEffects", false);
		result.add("normalizedProposal", proposal.json());
		JsonArray warnings = new JsonArray();
		if (!proposal.commands().isEmpty()) warnings.add("Commands are displayed only and were not executed");
		if (proposal.chancePercent() < 100D) {
			warnings.add("Chance is reported as a probability; no random outcome was selected");
		}
		result.add("warnings", warnings);
		return result;
	}

	private JsonObject diagnostics(JsonObject filters) {
		rejectUnknown(filters, Set.of(), "diagnostics filters");
		JsonObject result = overview(new JsonObject());
		result.addProperty("buildNumber", safe(plugin.getBuildNumber(), 80));
		result.addProperty("profile", safe(plugin.getProfile(), 80));
		result.addProperty("javaVersion", safe(System.getProperty("java.version", "unknown"), 80));
		result.addProperty("backgroundTaskSeconds", plugin.getLastBackgroundTaskTimeTaken());
		JsonArray detected = new JsonArray();
		Plugin[] plugins = plugin.getServer().getPluginManager().getPlugins();
		java.util.Arrays.stream(plugins).map(installed -> installed.getDescription().getName())
				.filter(name -> name != null && !name.isBlank()).distinct().sorted(String.CASE_INSENSITIVE_ORDER)
				.limit(128).forEach(name -> detected.add(safe(name, 80)));
		result.add("detectedPlugins", detected);
		JsonArray redacted = new JsonArray();
		List.of("credentials", "database hosts and credentials", "Redis/MQTT hosts and credentials",
				"webhook URLs", "raw configuration", "raw logs", "player records")
				.forEach(redacted::add);
		result.add("omittedSensitiveData", redacted);
		return result;
	}

	private List<String> allConfiguredVoteSiteNames() {
		ConfigurationSection root = plugin.getConfigVoteSites().getData().getConfigurationSection("VoteSites");
		if (root == null) return List.of();
		return root.getKeys(false).stream().filter(name -> root.isConfigurationSection(name))
				.sorted(String.CASE_INSENSITIVE_ORDER).toList();
	}

	private List<VoteSite> loadedVoteSites() {
		List<VoteSite> sites = plugin.getVoteSiteManager().getVoteSites();
		synchronized (sites) {
			return List.copyOf(sites);
		}
	}

	private VoteLogMysqlTable requireVoteLog() {
		VoteLogMysqlTable table = plugin.getVoteLogMysqlTable();
		if (!plugin.getConfigFile().isVoteLoggingEnabled() || table == null || !table.isReadable()) {
			throw new InspectionUnavailableException("vote logging is not enabled, initialized, or readable");
		}
		return table;
	}

	private static JsonObject notFound(String entity) {
		JsonObject result = new JsonObject();
		result.addProperty("found", false);
		result.addProperty("entity", entity);
		return result;
	}

	private static boolean hasRewardConfiguration(ConfigurationSection site) {
		return site.isConfigurationSection("Rewards") || site.isConfigurationSection("Reward")
				|| site.getKeys(false).stream().anyMatch(key -> key.toLowerCase(Locale.ROOT).contains("reward"));
	}

	private static void addHealth(JsonObject row, ServiceHealth health) {
		row.addProperty("loggedVotes", health == null ? 0 : health.votes());
		row.addProperty("lastVoteTime", health == null ? 0 : health.lastVoteTime());
		row.addProperty("immediateVotes", health == null ? 0 : health.immediate());
		row.addProperty("cachedVotes", health == null ? 0 : health.cached());
	}

	private static JsonArray entries(List<VoteLogEntry> rows) {
		JsonArray result = new JsonArray();
		rows.stream().limit(MAX_ROWS).forEach(entry -> {
			JsonObject row = new JsonObject();
			row.addProperty("voteId", safe(entry.voteId, 36));
			row.addProperty("voteTime", entry.voteTime);
			row.addProperty("playerUuid", safe(entry.playerUuid, 36));
			row.addProperty("playerName", safe(entry.playerName, 16));
			row.addProperty("service", safe(entry.service, 64));
			row.addProperty("server", safe(entry.server, 64));
			row.addProperty("event", safe(entry.event, 64));
			row.addProperty("context", safe(entry.context, 255));
			row.addProperty("status", safe(entry.status, 16));
			row.addProperty("cachedTotal", entry.proxyCachedTotal);
			result.add(row);
		});
		return result;
	}

	private static VoteLogEvent optionalEvent(JsonObject filters) {
		String event = optionalString(filters, "event", 64);
		if (event.isBlank()) return null;
		try {
			return VoteLogEvent.valueOf(event.toUpperCase(Locale.ROOT));
		} catch (IllegalArgumentException failure) {
			throw new IllegalArgumentException("event filter is invalid");
		}
	}

	private static void rejectUnknown(JsonObject object, Set<String> accepted, String label) {
		for (String key : object.keySet()) {
			if (!accepted.contains(key)) throw new IllegalArgumentException(label + " contains unsupported field " + key);
		}
	}

	private static JsonObject optionalObject(JsonObject object, String name) {
		if (!object.has(name) || object.get(name).isJsonNull()) return new JsonObject();
		return requiredObject(object, name);
	}

	private static JsonObject requiredObject(JsonObject object, String name) {
		JsonElement value = object.get(name);
		if (value == null || !value.isJsonObject()) throw new IllegalArgumentException(name + " must be an object");
		return value.getAsJsonObject();
	}

	private static String requiredString(JsonObject object, String name, int maximumLength) {
		String value = optionalString(object, name, maximumLength);
		if (value.isBlank()) throw new IllegalArgumentException(name + " is required");
		return value;
	}

	private static String optionalString(JsonObject object, String name, int maximumLength) {
		if (!object.has(name) || object.get(name).isJsonNull()) return "";
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isString()) {
			throw new IllegalArgumentException(name + " must be text");
		}
		String text = primitive.getAsString();
		if (text.length() > maximumLength || text.indexOf('\0') >= 0 || text.indexOf('\r') >= 0
				|| text.indexOf('\n') >= 0) throw new IllegalArgumentException(name + " is invalid");
		return text;
	}

	private static boolean optionalBoolean(JsonObject object, String name, boolean defaultValue) {
		if (!object.has(name) || object.get(name).isJsonNull()) return defaultValue;
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isString()) {
			throw new IllegalArgumentException(name + " must be true or false encoded as text");
		}
		String text = primitive.getAsString();
		if (!"true".equals(text) && !"false".equals(text)) {
			throw new IllegalArgumentException(name + " must be true or false encoded as text");
		}
		return Boolean.parseBoolean(text);
	}

	private static int boundedInt(JsonObject object, String name, int defaultValue, int minimum, int maximum) {
		if (!object.has(name) || object.get(name).isJsonNull()) return defaultValue;
		JsonElement value = object.get(name);
		if (!(value instanceof JsonPrimitive primitive) || !primitive.isString()) {
			throw new IllegalArgumentException(name + " must be a number encoded as text");
		}
		try {
			java.math.BigDecimal decimal = new java.math.BigDecimal(primitive.getAsString());
			int parsed = decimal.intValueExact();
			if (parsed < minimum || parsed > maximum) throw new ArithmeticException();
			return parsed;
		} catch (ArithmeticException | NumberFormatException failure) {
			throw new IllegalArgumentException(name + " is outside the allowed range");
		}
	}

	private static UUID parseUuid(String value, String name) {
		try {
			UUID parsed = UUID.fromString(value);
			if (value.length() != 36 || !parsed.toString().equalsIgnoreCase(value)) {
				throw new IllegalArgumentException();
			}
			return parsed;
		} catch (IllegalArgumentException failure) {
			throw new IllegalArgumentException(name + " is not a canonical UUID");
		}
	}

	private static String safe(String value, int maximumLength) {
		if (value == null) return "";
		String safe = value.replaceAll("[\\p{Cntrl}]", " ").trim();
		return safe.length() <= maximumLength ? safe : safe.substring(0, maximumLength);
	}

	private static String lower(String value) {
		return value == null ? "" : value.toLowerCase(Locale.ROOT);
	}

	private static void ensureBounded(JsonObject data) {
		if (data.toString().getBytes(StandardCharsets.UTF_8).length > MAX_DATA_BYTES) {
			throw new ResultTooLargeException("inspection data exceeds the 512 KiB limit");
		}
	}

	@SuppressWarnings("serial")
	public static final class InspectionUnavailableException extends IllegalStateException {
		public InspectionUnavailableException(String message) { super(message); }
	}

	@SuppressWarnings("serial")
	public static final class ResultTooLargeException extends IllegalStateException {
		private ResultTooLargeException(String message) { super(message); }
	}
}
