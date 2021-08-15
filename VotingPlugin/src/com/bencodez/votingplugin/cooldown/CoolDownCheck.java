package com.bencodez.votingplugin.cooldown;

import java.util.HashSet;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.advancedcore.api.user.UserStartup;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class CoolDownCheck implements Listener {

	private boolean cooldownCheckEnabled = false;

	private VotingPluginMain plugin;

	private Timer timer;

	private Set<UUID> uuids = new HashSet<UUID>();

	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.timer = new Timer();
	}

	public void check(UUID uuid) {
		check(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid));
	}

	public void check(VotingPluginUser user) {
		boolean coolDownCheck = user.getCoolDownCheck();
		if (user.canVoteAll() && coolDownCheck) {
			user.setCoolDownCheck(false);
			PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
			plugin.getServer().getPluginManager().callEvent(event);
			uuids.remove(UUID.fromString(user.getUUID()));
		} else if (!coolDownCheck) {
			schedule(user);
		}

	}

	public void checkEnabled() {
		if (!plugin.getConfigFile().isDisableCoolDownCheck() && RewardHandler.getInstance()
				.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward")) {
			cooldownCheckEnabled = true;
		} else {
			cooldownCheckEnabled = false;
		}
	}

	public void load() {
		if (cooldownCheckEnabled) {
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
					check(user);
				}
			});
		}
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		RewardHandler.getInstance().giveReward(event.getPlayer(), plugin.getSpecialRewardsConfig().getData(),
				"VoteCoolDownEndedReward", new RewardOptions());
	}

	public void schedule(VotingPluginUser user) {
		final UUID uuid = UUID.fromString(user.getUUID());
		long time = user.getNextTimeAllSitesAvailable();
		if (time > 0) {
			user.setCoolDownCheck(true);
			uuids.add(uuid);
			timer.schedule(new TimerTask() {

				@Override
				public void run() {
					check(uuid);
				}
			}, time * 1000 + 1500);
		}

	}

	public void vote(VotingPluginUser user) {
		if (cooldownCheckEnabled) {
			UUID uuid = UUID.fromString(user.getUUID());
			if (!uuids.contains(uuid)) {
				schedule(user);
			}
		}
	}
}
