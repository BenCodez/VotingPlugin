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
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class CoolDownCheck implements Listener {

	private VotingPluginMain plugin;

	private HashMap<UUID, ScheduledFuture<?>> perSiteTasks = new HashMap<UUID, ScheduledFuture<?>>();

	private HashMap<UUID, ScheduledFuture<?>> allSiteTasks = new HashMap<UUID, ScheduledFuture<?>>();

	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.timer = Executors.newScheduledThreadPool(1);
	}

	private ScheduledExecutorService timer;

	private boolean cooldownCheckEnabled = false;

	public void checkEnabled() {
		if (!plugin.getConfigFile().isDisableCoolDownCheck() && plugin.getRewardHandler()
				.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward")) {
			cooldownCheckEnabled = true;
		} else {
			cooldownCheckEnabled = false;
		}
	}

	public void check(UUID uuid) {
		check(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid));
	}

	public void checkPerSite(UUID uuid) {
		checkPerSite(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid));
	}

	public void vote(VotingPluginUser user, VoteSite site) {
		if (cooldownCheckEnabled) {
			schedule(user);
		}

		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			user.setCoolDownCheckSite(site, Boolean.FALSE);
			schedulePerSite(user);
		}
	}

	public void check(VotingPluginUser user) {
		boolean coolDownCheck = user.getCoolDownCheck();
		if (user.canVoteAll() && coolDownCheck) {
			user.setCoolDownCheck(false);
			PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
			plugin.getServer().getPluginManager().callEvent(event);
			allSiteTasks.remove(user.getJavaUUID());
		} else if (!coolDownCheck) {
			schedule(user);
		}
	}

	public synchronized void checkPerSite(VotingPluginUser user) {
		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			HashMap<VoteSite, Boolean> coolDownChecks = user.getCoolDownCheckSiteList();
			boolean changed = false;
			for (VoteSite site : plugin.getVoteSites()) {
				if (user.canVoteSite(site)) {
					if (!coolDownChecks.containsKey(site) || !coolDownChecks.get(site).booleanValue()) {
						coolDownChecks.put(site, Boolean.TRUE);
						changed = true;
						PlayerVoteSiteCoolDownEndEvent event = new PlayerVoteSiteCoolDownEndEvent(user, site);
						plugin.getServer().getPluginManager().callEvent(event);
					}
				}
			}
			if (changed) {
				user.setCoolDownCheckSite(coolDownChecks);
			}
		}

		schedulePerSite(user);

	}

	public void schedulePerSite(VotingPluginUser user) {
		final UUID uuid = UUID.fromString(user.getUUID());
		long time = user.getNextTimeFirstSiteAvailable();
		if (time > 0) {
			if (perSiteTasks.containsKey(uuid)) {
				perSiteTasks.get(uuid).cancel(false);
				perSiteTasks.remove(uuid);
			}
			plugin.devDebug("PerSiteCoolDownEvent schedule time: " + time);
			ScheduledFuture<?> scheduledFuture = timer.schedule(new Runnable() {

				@Override
				public void run() {
					if (plugin != null && plugin.isEnabled()) {
						checkPerSite(uuid);
					}
				}
			}, time + 2, TimeUnit.SECONDS);
			perSiteTasks.put(uuid, scheduledFuture);
		}

	}

	public void schedule(VotingPluginUser user) {
		final UUID uuid = UUID.fromString(user.getUUID());
		long time = user.getNextTimeAllSitesAvailable();
		if (time > 0) {
			user.setCoolDownCheck(true);
			if (allSiteTasks.containsKey(uuid)) {
				allSiteTasks.get(uuid).cancel(false);
				allSiteTasks.remove(uuid);
			}
			ScheduledFuture<?> scheduledFuture = timer.schedule(new Runnable() {

				@Override
				public void run() {
					check(uuid);
				}
			}, time + 2, TimeUnit.SECONDS);
			allSiteTasks.put(uuid, scheduledFuture);
		}

	}

	public void load() {

		plugin.addUserStartup(new UserStartup() {

			@Override
			public void onStartUp(AdvancedCoreUser advancedcoreUser) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(advancedcoreUser);
				if (cooldownCheckEnabled) {
					check(user);
				}
				if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
					checkPerSite(user);
				}
			}

			@Override
			public void onStart() {

			}

			@Override
			public void onFinish() {

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
				new RewardOptions());
	}

}
