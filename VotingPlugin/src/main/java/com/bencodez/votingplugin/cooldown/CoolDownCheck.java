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

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

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

public class CoolDownCheck implements Listener {

	private final VotingPluginMain plugin;

	private final Map<UUID, ScheduledFuture<?>> perSiteTasks = new ConcurrentHashMap<>();
	private final Map<UUID, ScheduledFuture<?>> allSiteTasks = new ConcurrentHashMap<>();

	@Getter
	private final ScheduledExecutorService timer;

	private volatile boolean cooldownCheckEnabled = false;

	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;

		// Better behavior when you cancel+reschedule a lot (removes cancelled tasks from queue)
		ScheduledThreadPoolExecutor exec = new ScheduledThreadPoolExecutor(1);
		exec.setRemoveOnCancelPolicy(true);
		this.timer = exec;
	}

	public void check(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		check(user);
	}

	public void shutdown() {
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

				checkPerSite(site, user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.getLogger().info("Finished checking vote cooldown rewards for all players: " + site.getKey()
				+ " (processed: " + count + ")"));
	}

	public void checkEnabled() {
		cooldownCheckEnabled = !plugin.getConfigFile().isDisableCoolDownCheck()
				&& plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(),
						"VoteCoolDownEndedReward");
	}

	public void checkPerSite(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		checkPerSite(user);
	}

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
		if (!plugin.getConfigFile().isPerSiteCoolDownEvents() || site == null) {
			return;
		}

		HashMap<String, Boolean> coolDownChecks = user.getCoolDownCheckSiteList();
		if (Boolean.TRUE.equals(coolDownChecks.get(site.getKey()))) {
			return;
		}

		if (user.canVoteSite(site)) {
			coolDownChecks.put(site.getKey(), Boolean.TRUE);
			user.setCoolDownCheckSite(coolDownChecks);

			plugin.extraDebug(
					"Triggering votesitecooldownend event on site " + site.getKey() + " for " + user.getUUID());
			PlayerVoteSiteCoolDownEndEvent event = new PlayerVoteSiteCoolDownEndEvent(user, site);
			plugin.getServer().getPluginManager().callEvent(event);
		}
	}

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

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		plugin.getRewardHandler().giveReward(event.getPlayer(), plugin.getSpecialRewardsConfig().getData(),
				"VoteCoolDownEndedReward", new RewardOptions());
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteSiteCoolDownEndEvent event) {
		plugin.getRewardHandler().giveReward(event.getPlayer(), event.getSite().getSiteData(), "CoolDownEndRewards",
				new RewardOptions().addPlaceholder("sitename", event.getSite().getDisplayName())
						.addPlaceholder("url", event.getSite().getVoteURL(false)));
	}

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

	public void schedulePerSite(VotingPluginUser user, boolean force) {
		if (!plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			return;
		}

		if (user.getSitesVotedOn() <= 0 && !force) {
			return;
		}

		final UUID uuid = user.getJavaUUID();

		final NextSite next = user.getNextSiteAvailable();

		perSiteTasks.compute(uuid, (k, existing) -> {
			if (existing != null) {
				existing.cancel(false);
			}

			if (next == null || next.getSite() == null || next.getSecondsUntilAvailable() <= 0) {
				plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling per-site cooldown check");
				return null; // remove entry
			}

			plugin.devDebug("PerSiteCoolDownEvent schedule: site=" + next.getSite().getKey() + " time="
					+ next.getSecondsUntilAvailable() + " seconds");

			return timer.schedule(() -> {
				if (!plugin.isEnabled()) {
					return;
				}

				// Only check the next site; this will set the cooldown-end flag + fire event if ready
				checkPerSite(next.getSite(), user);

				// Schedule the following site (if any)
				schedulePerSite(user, false);

			}, next.getSecondsUntilAvailable() + 2, TimeUnit.SECONDS);
		});
	}

	public void vote(VotingPluginUser user, VoteSite site) {
		if (cooldownCheckEnabled) {
			schedule(user, true);
		}

		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			user.setCoolDownCheckSite(site, Boolean.FALSE);
			schedulePerSite(user, true);
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

				for (int i = 0; i < dailySites.size(); i++) {
					checkPerSite(dailySites.get(i), user);
				}

				check(user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.debug("Finished checking vote cooldown rewards (processed: " + count + " users)"));
	}
}
