package com.bencodez.votingplugin.cooldown;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
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
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

public class CoolDownCheck implements Listener {

	private final VotingPluginMain plugin;

	private final Map<UUID, ScheduledFuture<?>> perSiteTasks = new HashMap<>();
	private final Map<UUID, ScheduledFuture<?>> allSiteTasks = new HashMap<>();

	@Getter
	private final ScheduledExecutorService timer;

	private volatile boolean cooldownCheckEnabled = false;

	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.timer = Executors.newScheduledThreadPool(1);
	}

	public void check(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		check(user);
	}

	public synchronized void check(VotingPluginUser user) {
		if (!user.getCoolDownCheck()) {
			return;
		}

		if (user.canVoteAll()) {
			user.setCoolDownCheck(false);
			PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
			plugin.getServer().getPluginManager().callEvent(event);
			allSiteTasks.remove(user.getJavaUUID());
			return;
		}

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
					cols.clear(); // help GC
				}

				checkPerSite(site, user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.getLogger().info("Finished checking vote cooldown rewards for all players: "
				+ site.getKey() + " (processed: " + count + ")"));
	}

	public void checkEnabled() {
		cooldownCheckEnabled = !plugin.getConfigFile().isDisableCoolDownCheck() && plugin.getRewardHandler()
				.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward");
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

		for (VoteSite site : plugin.getVoteSitesEnabled()) {
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

			plugin.extraDebug("Triggering votesitecooldownend event for " + user.getUUID());
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
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(advancedcoreUser);
				user.userDataFetechMode(UserDataFetchMode.NO_CACHE);

				if (cooldownCheckEnabled) {
					check(user);
				}
				if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
					checkPerSite(user);
				}
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
				new RewardOptions().addPlaceholder("sitename", event.getSite().getDisplayName()).addPlaceholder("url",
						event.getSite().getVoteURL(false)));
	}

	public void schedule(VotingPluginUser user, boolean force) {
		if (user.getSitesVotedOn() <= 0 && !force) {
			return;
		}

		final UUID uuid = UUID.fromString(user.getUUID());
		final long time = user.getNextTimeAllSitesAvailable();

		ScheduledFuture<?> existing = allSiteTasks.remove(uuid);
		if (existing != null) {
			existing.cancel(false);
		}

		if (time > 0) {
			user.setCoolDownCheck(true);
			ScheduledFuture<?> scheduledFuture = timer.schedule(() -> {
				if (plugin.isEnabled()) {
					check(uuid);
				}
			}, time + 2, TimeUnit.SECONDS);
			allSiteTasks.put(uuid, scheduledFuture);
		} else {
			plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling cooldown check");
		}
	}

	public void schedulePerSite(VotingPluginUser user, boolean force) {
		if (user.getSitesVotedOn() <= 0 && !force) {
			return;
		}

		final UUID uuid = UUID.fromString(user.getUUID());
		final long time = user.getNextTimeFirstSiteAvailable();

		ScheduledFuture<?> existing = perSiteTasks.remove(uuid);
		if (existing != null) {
			existing.cancel(false);
		}

		if (time > 0) {
			plugin.devDebug("PerSiteCoolDownEvent schedule time: " + time + " seconds");
			ScheduledFuture<?> scheduledFuture = timer.schedule(() -> {
				if (plugin.isEnabled()) {
					checkPerSite(uuid);
				}
			}, time + 2, TimeUnit.SECONDS);
			perSiteTasks.put(uuid, scheduledFuture);
		} else {
			plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling cooldown check");
		}
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
		final List<VoteSite> sites = plugin.getVoteSites();
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

		plugin.getLogger()
				.info("Checking vote cooldown rewards for all players across " + dailySites.size() + " sites...");

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
			} finally {
				user.clearTempCache();
			}
		}, (count) -> plugin.getLogger()
				.info("Finished checking vote cooldown rewards (processed: " + count + " users)"));
	}
}
