package com.bencodez.votingplugin.cooldown;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
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
import com.bencodez.advancedcore.api.user.userstorage.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.events.PlayerVoteSiteCoolDownEndEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

public class CoolDownCheck implements Listener {

	private VotingPluginMain plugin;

	private HashMap<UUID, ScheduledFuture<?>> perSiteTasks = new HashMap<UUID, ScheduledFuture<?>>();

	private HashMap<UUID, ScheduledFuture<?>> allSiteTasks = new HashMap<UUID, ScheduledFuture<?>>();

	private HashMap<String, ScheduledFuture<?>> voteSiteChecks = new HashMap<String, ScheduledFuture<?>>();

	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.timer = Executors.newScheduledThreadPool(1);
	}

	@Getter
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

	public synchronized void check(VotingPluginUser user) {
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
			HashMap<String, Boolean> coolDownChecks = user.getCoolDownCheckSiteList();
			boolean changed = false;
			for (VoteSite site : plugin.getVoteSites()) {
				if (user.canVoteSite(site)) {
					if (!coolDownChecks.containsKey(site.getKey())
							|| !coolDownChecks.get(site.getKey()).booleanValue()) {
						coolDownChecks.put(site.getKey(), Boolean.TRUE);
						changed = true;
						plugin.extraDebug("Trigger votesitecooldownend event for " + user.getUUID());
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
			}, time + 61, TimeUnit.SECONDS);
			perSiteTasks.put(uuid, scheduledFuture);
		}

	}

	public void schedule(VotingPluginUser user) {
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
			}, time + 60, TimeUnit.SECONDS);
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

		scheduleGlobalCheckVoteSite();
	}

	public void scheduleGlobalCheckVoteSite() {
		if (plugin.getConfigFile().isPerSiteCoolDownEvents()) {
			for (VoteSite site : plugin.getVoteSites()) {
				scheduleGlobalCheckVoteSite(site);
			}
		}
	}

	public void scheduleGlobalCheckVoteSite(VoteSite site) {
		if (voteSiteChecks.containsKey(site.getKey())) {
			voteSiteChecks.get(site.getKey()).cancel(false);
			voteSiteChecks.remove(site.getKey());
		}
		if (site.isVoteDelayDaily()) {
			LocalDateTime resetTime = plugin.getTimeChecker().getTime().withHour(site.getVoteDelayDailyHour())
					.withMinute(0).withSecond(0);
			LocalDateTime resetTimeTomorrow = resetTime.plusHours(24);
			long time = 0;
			LocalDateTime now = plugin.getTimeChecker().getTime();
			if (now.isBefore(resetTime)) {
				Duration dur = Duration.between(now, resetTime);
				time = dur.getSeconds();
			} else if (now.isBefore(resetTimeTomorrow)) {
				Duration dur = Duration.between(now, resetTimeTomorrow);
				time = dur.getSeconds();
			}

			ScheduledFuture<?> scheduledFuture = timer.schedule(new Runnable() {

				@Override
				public void run() {
					checkAllVoteSite(site);
				}
			}, time + 65, TimeUnit.SECONDS);
			plugin.getLogger().info(
					"Checking vote delay daily cooldown events/rewards in " + time + " seconds for " + site.getKey());

			voteSiteChecks.put(site.getKey(), scheduledFuture);
		}

	}

	public void checkAllVoteSite(VoteSite site) {
		plugin.getLogger().info("Checking vote cooldown rewards for all players: " + site.getKey());
		HashMap<UUID, ArrayList<Column>> cols = plugin.getUserManager().getAllKeys();
		for (Entry<UUID, ArrayList<Column>> playerData : cols.entrySet()) {

			String uuid = playerData.getKey().toString();
			if (plugin != null && plugin.isEnabled()) {
				if (uuid != null && !uuid.isEmpty()) {
					VotingPluginUser user = plugin.getVotingPluginUserManager()
							.getVotingPluginUser(UUID.fromString(uuid), false);
					user.dontCache();
					user.updateTempCacheWithColumns(playerData.getValue());
					cols.put(playerData.getKey(), null);
					checkPerSite(user);
					user.clearTempCache();
					user = null;
				}
			}
		}
		cols.clear();
		cols = null;

		plugin.getLogger().info("Finished checking vote cooldown rewards for all players: " + site.getKey());
		scheduleGlobalCheckVoteSite(site);

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

}
