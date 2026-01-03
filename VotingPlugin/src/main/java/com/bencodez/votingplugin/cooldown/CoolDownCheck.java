package com.bencodez.votingplugin.cooldown;

import java.util.HashMap;
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

	private VotingPluginMain plugin;

	private HashMap<UUID, ScheduledFuture<?>> perSiteTasks = new HashMap<>();

	private HashMap<UUID, ScheduledFuture<?>> allSiteTasks = new HashMap<>();

	@Getter
	private ScheduledExecutorService timer;

	private boolean cooldownCheckEnabled = false;

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
		boolean coolDownCheck = user.getCoolDownCheck();
		if (coolDownCheck) {
			if (user.canVoteAll()) {
				user.setCoolDownCheck(false);
				PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
				plugin.getServer().getPluginManager().callEvent(event);
				allSiteTasks.remove(user.getJavaUUID());
			} else {
				schedulePerSite(user, false);
			}
		}
	}

	public void checkAllVoteSite(VoteSite site) {
		plugin.getLogger().info("Checking vote cooldown rewards for all players: " + site.getKey());

		plugin.getUserManager().forEachUserKeys((uuid, cols) -> {
			if (plugin == null || !plugin.isEnabled() || uuid == null) {
				return;
			}

			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			if (user == null) {
				return;
			}

			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);

			try {
				// Populate temp cache from the provided columns (no persistent loads)
				if (cols != null) {
					user.updateTempCacheWithColumns(cols);
					cols.clear(); // help GC a bit
				}

				checkPerSite(user);
			} finally {
				user.clearTempCache();
			}
		}, (count) -> {
			plugin.getLogger().info(
					"Finished checking vote cooldown rewards for all players: " + site.getKey() + " (processed: " + count + ")");
		});
	}

	public void checkEnabled() {
		if (!plugin.getConfigFile().isDisableCoolDownCheck() && plugin.getRewardHandler()
				.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward")) {
			cooldownCheckEnabled = true;
		} else {
			cooldownCheckEnabled = false;
		}
	}

	public void checkPerSite(UUID uuid) {
		VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid);
		if (!user.isOnline()) {
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
		}
		checkPerSite(user);
	}

	public synchronized void checkPerSite(VotingPluginUser user) {
		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			HashMap<String, Boolean> coolDownChecks = user.getCoolDownCheckSiteList();
			boolean changed = false;
			for (VoteSite site : plugin.getVoteSitesEnabled()) {
				if (!coolDownChecks.containsKey(site.getKey()) || !coolDownChecks.get(site.getKey()).booleanValue()) {
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

	}

	public void load() {
		plugin.addUserStartup(new UserStartup() {

			@Override
			public void onFinish() {

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
		if (user.getSitesVotedOn() > 0 || force) {
			final UUID uuid = UUID.fromString(user.getUUID());
			long time = user.getNextTimeAllSitesAvailable();
			if (allSiteTasks.containsKey(uuid)) {
				allSiteTasks.get(uuid).cancel(false);
				allSiteTasks.remove(uuid);
			}
			if (time > 0) {
				user.setCoolDownCheck(true);
				ScheduledFuture<?> scheduledFuture = timer.schedule(new Runnable() {

					@Override
					public void run() {
						if (plugin != null && plugin.isEnabled()) {
							check(uuid);
						}
					}
				}, time + 2, TimeUnit.SECONDS);
				allSiteTasks.put(uuid, scheduledFuture);
			} else {
				plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling cooldown check");
			}
		}

	}

	public void schedulePerSite(VotingPluginUser user, boolean force) {
		if (user.getSitesVotedOn() > 0 || force) {
			final UUID uuid = UUID.fromString(user.getUUID());
			long time = user.getNextTimeFirstSiteAvailable();
			if (perSiteTasks.containsKey(uuid)) {
				perSiteTasks.get(uuid).cancel(false);
				perSiteTasks.remove(uuid);
			}
			if (time > 0) {
				plugin.devDebug("PerSiteCoolDownEvent schedule time: " + time + " seconds");
				ScheduledFuture<?> scheduledFuture = timer.schedule(new Runnable() {

					@Override
					public void run() {
						if (plugin != null && plugin.isEnabled()) {
							checkPerSite(uuid);
						}
					}
				}, time + 2, TimeUnit.SECONDS);
				perSiteTasks.put(uuid, scheduledFuture);
			} else {
				plugin.extraDebug(user.getUUID() + "/" + user.getPlayerName() + " not scheduling cooldown check");
			}
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

}
