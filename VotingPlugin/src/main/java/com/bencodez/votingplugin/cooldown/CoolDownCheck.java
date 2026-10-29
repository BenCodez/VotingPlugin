package com.bencodez.votingplugin.cooldown;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.NextSite;
import com.bencodez.votingplugin.votesites.VoteSite;

import lombok.Getter;

/**
 * Manages cooldown checks for vote rewards.
 */
public class CoolDownCheck implements Listener {

	private final VotingPluginMain plugin;

	private final Map<UUID, ScheduledFuture<?>> perSiteTasks = new ConcurrentHashMap<>();
	private final Map<UUID, Object> perSiteGenerations = new ConcurrentHashMap<>();
	private final Map<UUID, ScheduledFuture<?>> allSiteTasks = new ConcurrentHashMap<>();

	@Getter
	private final ScheduledExecutorService timer;

	private volatile boolean cooldownCheckEnabled = false;
	private volatile boolean stopped = false;

	/**
	 * Constructs a new cooldown check manager.
	 *
	 * @param plugin the plugin instance
	 */
	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;

		// Better behavior when you cancel+reschedule a lot (removes cancelled tasks from queue)
		ScheduledThreadPoolExecutor exec = new ScheduledThreadPoolExecutor(1);
		exec.setRemoveOnCancelPolicy(true);
		this.timer = exec;
	}

	/**
	 * Checks the cooldown for a player by UUID.
	 *
	 * @param uuid the player UUID
	 */
	public void check(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		check(user);
	}

	/**
	 * Shuts down the cooldown check timer.
	 */
	public void shutdown() {
		stopped = true;
		perSiteGenerations.clear();
		timer.shutdownNow();

		for (ScheduledFuture<?> f : perSiteTasks.values()) {
			if (f != null) {
				f.cancel(false);
			}
		}
		for (ScheduledFuture<?> f : allSiteTasks.values()) {
			if (f != null) {
				f.cancel(false);
			}
		}

		perSiteTasks.clear();
		allSiteTasks.clear();
	}

	/**
	 * Checks the cooldown for a user.
	 *
	 * @param user the voting plugin user
	 */
	public synchronized void check(VotingPluginUser user) {
		if (!user.getCoolDownCheck()) {
			return;
		}

		if (user.canVoteAll()) {
			user.setCoolDownCheck(false);

			PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
			plugin.getServer().getPluginManager().callEvent(event);

			ScheduledFuture<?> f = allSiteTasks.remove(user.getJavaUUID());
			if (f != null) {
				f.cancel(false);
			}
			return;
		}

		// Keep per-site scheduling going while waiting for "all sites available"
		schedulePerSite(user, false);
	}

	/**
	 * Check ONLY a specific votesite for all users (uses streaming keys/cols).
	 *
	 * @param site the vote site to check
	 */
	public void checkAllVoteSite(VoteSite site) {
		if (site == null) {
			return;
		}

		plugin.getLogger().info("Checking vote cooldown rewards for all players: " + site.getKey());

		plugin.getUserManager().forEachUserKeys((uuid, cols) -> {
			if (!plugin.isEnabled() || uuid == null) {
				return;
			}

			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			if (user == null) {
				return;
			}

			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);

			try {
				if (cols != null) {
					user.updateTempCacheWithColumns(cols);
					cols.clear(); // help GC (only useful if reused)
				}
				capturePlayerName(user);

				checkPerSite(site, user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.getLogger().info("Finished checking vote cooldown rewards for all players: " + site.getKey()
				+ " (processed: " + count + ")"));
	}

	/**
	 * Checks if cooldown check is enabled.
	 */
	public void checkEnabled() {
		cooldownCheckEnabled = !plugin.getConfigFile().isDisableCoolDownCheck()
				&& plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						"VoteCoolDownEndedReward");
	}

	/**
	 * Checks per-site cooldown for a player by UUID.
	 *
	 * @param uuid the player UUID
	 */
	public void checkPerSite(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		checkPerSite(user);
	}

	/**
	 * Checks per-site cooldown for a user.
	 *
	 * @param user the voting plugin user
	 */
	public synchronized void checkPerSite(VotingPluginUser user) {
		if (!plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			return;
		}

		HashMap<String, Boolean> coolDownChecks = user.getCoolDownCheckSiteList();
		boolean changed = false;

		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (site == null) {
				continue;
			}

			if (!Boolean.TRUE.equals(coolDownChecks.get(site.getKey()))) {
				if (user.canVoteSite(site)) {
					coolDownChecks.put(site.getKey(), Boolean.TRUE);
					changed = true;

					plugin.extraDebug("Triggering votesitecooldownend event for " + user.getUUID());
					PlayerVoteSiteCoolDownEndEvent event = new PlayerVoteSiteCoolDownEndEvent(user, site);
					plugin.getServer().getPluginManager().callEvent(event);
				}
			}
		}

		if (changed) {
			user.setCoolDownCheckSite(coolDownChecks);
		}

		schedulePerSite(user, false);
	}

	/**
	 * Site-specific version used by batch checks / checkAllVoteSite.
	 */
	private void checkPerSite(VoteSite site, VotingPluginUser user) {
		checkPerSite(site, user, null);
	}

	private HashMap<String, Boolean> checkPerSite(VoteSite site, VotingPluginUser user,
			HashMap<String, Boolean> coolDownChecks) {
		if (!plugin.getConfigFile().isPerSiteCoolDownEvents() || site == null) {
			return coolDownChecks;
		}

		if (coolDownChecks == null) {
			coolDownChecks = user.getCoolDownCheckSiteList();
		}
		if (Boolean.TRUE.equals(coolDownChecks.get(site.getKey()))) {
			return coolDownChecks;
		}

		if (user.canVoteSite(site)) {
			coolDownChecks.put(site.getKey(), Boolean.TRUE);
			user.setCoolDownCheckSite(coolDownChecks);

			plugin.extraDebug(
					"Triggering votesitecooldownend event on site " + site.getKey() + " for " + user.getUUID());
			PlayerVoteSiteCoolDownEndEvent event = new PlayerVoteSiteCoolDownEndEvent(user, site);
			plugin.getServer().getPluginManager().callEvent(event);
		}
		return coolDownChecks;
	}

	/**
	 * Loads the cooldown check system.
	 */
	public void load() {
		plugin.addUserStartup(new UserStartup() {

			@Override
			public void onFinish() {
				checkAllVoteSites();
			}

			@Override
			public void onStart() {
			}

			@Override
			public void onStartUp(AdvancedCoreUser advancedcoreUser) {
			}
		});
	}

	/**
	 * Handles the cooldown end event.
	 *
	 * @param event the cooldown end event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		dispatchCooldownReward(event.getPlayer(), plugin.getSpecialRewardsConfig().getData(),
				"VoteCoolDownEndedReward", "all", new RewardOptions());
	}

	/**
	 * Handles the per-site cooldown end event.
	 *
	 * @param event the per-site cooldown end event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteSiteCoolDownEndEvent event) {
		dispatchCooldownReward(event.getPlayer(), event.getSite().getSiteData(), "CoolDownEndRewards",
				"site:" + event.getSite().getKey(), new RewardOptions()
						.addPlaceholder("sitename", event.getSite().getDisplayName())
						.addPlaceholder("url", event.getSite().getVoteURL(false)));
	}

	private void dispatchCooldownReward(VotingPluginUser user, ConfigurationSection data, String path,
			String site, RewardOptions options) {
		boolean configured = false;
		try {
			configured = hasConfiguredCooldownReward(data, path);
		} catch (RuntimeException ignored) {
			// Preserve legacy dispatch even when diagnostic configuration inspection fails.
		}
		if (!configured) {
			plugin.getRewardHandler().giveReward(user, data, path, options);
			return;
		}
		String occurrence = UUID.randomUUID().toString();
		String player = user.getUUID();
		String safeSite = String.valueOf(site).replaceAll("[\\r\\n\\p{Cntrl}]", "?");
		long time = System.currentTimeMillis();
		String context = " occurrence=" + occurrence + " player=" + player + " site=" + safeSite + " time=" + time;
		logCooldownDiagnostic(false, "Cooldown reward status=attempted" + context);
		options.setAsyncFailureObserver(failure -> logCooldownFailure(context, failure));
		try {
			plugin.getRewardHandler().giveReward(user, data, path, options);
		} catch (RuntimeException | Error failure) {
			logCooldownFailure(context, failure);
			throw failure;
		}
	}

	private boolean hasConfiguredCooldownReward(ConfigurationSection data, String path) {
		if (data == null) return false;
		if (data instanceof FileConfiguration) {
			return plugin.getRewardHandler().hasRewards((FileConfiguration) data, path);
		}
		if (data.isList(path)) return !data.getList(path, new ArrayList<>()).isEmpty();
		if (data.isConfigurationSection(path)) return !data.getConfigurationSection(path).getKeys(false).isEmpty();
		return data.isString(path) && !data.getString(path, "").isEmpty();
	}

	private void logCooldownFailure(String context, Throwable failure) {
		int completed = -1;
		Throwable current = failure;
		for (int depth = 0; current != null && depth < 32; depth++, current = current.getCause()) {
			if (current instanceof Reward.RewardReplayFailure) {
				completed = ((Reward.RewardReplayFailure) current).getCompletedInjectionCount();
				break;
			}
		}
		String error = failure == null ? "unknown" : failure.getClass().getSimpleName();
		logCooldownDiagnostic(true, "Cooldown reward outcome=unknown/partial" + context + " error=" + error
				+ " completedInjections=" + (completed < 0 ? "unknown" : completed)
				+ "; inspect effects before reissuing");
	}

	private void logCooldownDiagnostic(boolean warning, String message) {
		try {
			if (warning) plugin.getLogger().warning(message);
			else plugin.getLogger().info(message);
		} catch (RuntimeException ignored) {
			// A broken logger must not suppress or retry the reward.
		}
	}

	/**
	 * Schedules a cooldown check for a user.
	 *
	 * @param user the voting plugin user
	 * @param force whether to force the schedule
	 */
	public void schedule(VotingPluginUser user, boolean force) {
		if (user.getSitesVotedOn() <= 0 && !force) {
			return;
		}

		final UUID uuid = user.getJavaUUID();
		final long time = user.getNextTimeAllSitesAvailable();

		allSiteTasks.compute(uuid, (k, existing) -> {
			if (existing != null) {
				existing.cancel(false);
			}

			if (time <= 0) {
				plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling cooldown check");
				return null; // remove entry
			}

			user.setCoolDownCheck(true);

			return timer.schedule(() -> {
				if (plugin.isEnabled()) {
					check(uuid);
				}
			}, time + 2, TimeUnit.SECONDS);
		});
	}

	/**
	 * Schedules per-site cooldown checks for a user.
	 *
	 * @param user the voting plugin user
	 * @param force whether to force the schedule
	 */
	public void schedulePerSite(VotingPluginUser user, boolean force) {
		if (stopped || !plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			return;
		}

		if (user.getSitesVotedOn() <= 0 && !force) {
			return;
		}

		final UUID uuid = user.getJavaUUID();
		final String scheduledName = user.getPlayerName();

		final NextSite next = user.getNextSiteAvailable();

		perSiteTasks.compute(uuid, (k, existing) -> {
			if (existing != null) {
				existing.cancel(false);
			}
			if (stopped) {
				perSiteGenerations.remove(uuid);
				return null;
			}

			if (next == null || next.getSite() == null || next.getSecondsUntilAvailable() <= 0) {
				perSiteGenerations.remove(uuid);
				plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling per-site cooldown check");
				return null; // remove entry
			}

			plugin.devDebug("PerSiteCoolDownEvent schedule: site=" + next.getSite().getKey() + " time="
					+ next.getSecondsUntilAvailable() + " seconds");

			Object generation = new Object();
			perSiteGenerations.put(uuid, generation);
			try {
				return timer.schedule(() -> {
					if (stopped || !plugin.isEnabled() || perSiteGenerations.get(uuid) != generation) {
						return;
					}
					UUID occurrence = UUID.randomUUID();
					try {
						// The batch user's temporary row may have been cleared or replaced by a newer vote.
						VotingPluginUser current = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
						if (!current.isCached()) {
							current.userDataFetechMode(UserDataFetchMode.NO_CACHE);
						}
						String storedName = current.getData().getString("PlayerName", current.getUserDataFetchMode());
						String name = storedName != null && !storedName.isBlank() ? storedName : scheduledName;
						if (name != null && !name.isBlank()) {
							current.setPlayerName(name);
						}
						if (perSiteGenerations.get(uuid) != generation) return;
						checkPerSite(next.getSite(), current);
						if (perSiteGenerations.get(uuid) == generation) schedulePerSite(current, false);
					} catch (RuntimeException | Error failure) {
						String siteKey = String.valueOf(next.getSite().getKey()).replaceAll("[\\r\\n\\p{Cntrl}]", "?");
						logCooldownDiagnostic(true, "Cooldown timer failed: occurrence=" + occurrence + " player=" + uuid
								+ " site=" + siteKey + " time=" + System.currentTimeMillis() + " error="
								+ failure.getClass().getSimpleName() + "; reward outcome is uncertain; inspect effects before reissuing");
						throw failure;
					} finally {
						perSiteGenerations.remove(uuid, generation);
					}
				}, next.getSecondsUntilAvailable() + 2, TimeUnit.SECONDS);
			} catch (RuntimeException | Error failure) {
				perSiteGenerations.remove(uuid, generation);
				throw failure;
			}
		});
	}

	/**
	 * Handles a vote event for cooldown scheduling.
	 *
	 * @param user the voting plugin user
	 * @param site the vote site
	 */
	public void vote(VotingPluginUser user, VoteSite site) {
		if (cooldownCheckEnabled) {
			schedule(user, true);
		}

		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			user.setCoolDownCheckSite(site, Boolean.FALSE);
			schedulePerSite(user, true);
		}
	}

	/** Preserve the name needed by asynchronous rewards after the temporary row is cleared. */
	private static void capturePlayerName(VotingPluginUser user) {
		String name = user.getPlayerName();
		if (name != null && !name.isBlank()) {
			user.setPlayerName(name);
		}
	}

	/**
	 * Performance-optimized: scan users ONCE, process all daily-delay sites.
	 */
	public void checkAllVoteSites() {
		final List<VoteSite> sites = plugin.getVoteSiteManager().getVoteSites();
		if (sites == null || sites.isEmpty()) {
			return;
		}

		final ArrayList<VoteSite> dailySites = new ArrayList<>();
		for (int i = 0; i < sites.size(); i++) {
			VoteSite site = sites.get(i);
			if (site != null && site.isVoteDelayDaily()) {
				dailySites.add(site);
			}
		}

		if (dailySites.isEmpty()) {
			return;
		}

		plugin.debug("Checking vote cooldown rewards for all players across " + dailySites.size() + " sites...");

		plugin.getUserManager().forEachUserKeys((uuid, cols) -> {
			if (!plugin.isEnabled() || uuid == null) {
				return;
			}

			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			if (user == null) {
				return;
			}

			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);

			try {
				if (cols != null) {
					user.updateTempCacheWithColumns(cols);
					cols.clear();
				}
				capturePlayerName(user);

				HashMap<String, Boolean> coolDownChecks = null;
				for (int i = 0; i < dailySites.size(); i++) {
					coolDownChecks = checkPerSite(dailySites.get(i), user, coolDownChecks);
				}

				check(user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.debug("Finished checking vote cooldown rewards (processed: " + count + " users)"));
	}
}
