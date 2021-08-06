package com.bencodez.votingplugin.cooldown;

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

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class CoolDownCheck implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new vote reminding.
	 *
	 * @param plugin the plugin
	 */
	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.timer = new Timer();
	}

	private Timer timer;

	public void check(UUID uuid) {
		check(plugin.getVotingPluginUserManager().getVotingPluginUser(uuid));
	}

	public void check(VotingPluginUser user) {
		if (user.canVoteAll()) {
			PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(user);
			plugin.getServer().getPluginManager().callEvent(event);
		} else {
			schedule(user);
		}

	}

	public void schedule(VotingPluginUser user) {
		final UUID uuid = UUID.fromString(user.getUUID());
		long time = user.getNextTimeAllSitesAvailable();
		if (time > 0) {
			timer.schedule(new TimerTask() {

				@Override
				public void run() {
					check(uuid);
				}
			}, time * 1000);
		}

	}

	public void load() {
		if (!plugin.getConfigFile().isDisableCoolDownCheck() && RewardHandler.getInstance()
				.hasRewards(plugin.getSpecialRewardsConfig().getData(), "VoteCoolDownEndedReward")) {
			plugin.addUserStartup(new UserStartup() {

				@Override
				public void onStartUp(AdvancedCoreUser advancedcoreUser) {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(advancedcoreUser);
					check(user);
				}

				@Override
				public void onStart() {

				}

				@Override
				public void onFinish() {

				}
			});
		}
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		RewardHandler.getInstance().giveReward(event.getPlayer(), plugin.getSpecialRewardsConfig().getData(),
				"VoteCoolDownEndedReward", new RewardOptions());
	}
}
