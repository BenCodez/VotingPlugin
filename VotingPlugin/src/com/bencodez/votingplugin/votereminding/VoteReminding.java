package com.bencodez.votingplugin.votereminding;

import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class VoteReminding {

	/** The plugin. */
	private VotingPluginMain plugin;

	private ScheduledExecutorService timer;

	@Getter
	private ConcurrentHashMap<UUID, Boolean> remindersEnabled = new ConcurrentHashMap<UUID, Boolean>();

	public VoteReminding(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check remind.
	 *
	 * @param user the user
	 */
	public void checkRemind(VotingPluginUser user) {
		if (user.hasPermission("VotingPlugin.Login.RemindVotes") || user.hasPermission("VotingPlugin.Player")) {
			if (shouldRemind(user)) {
				if (!plugin.getConfigFile().getVoteRemindingRemindOnlyOnce()) {
					runRemind(user);
				} else if (!user.isReminded()) {
					runRemind(user);
				}
			}

		}
	}

	private void giveReward(VotingPluginUser user) {
		new RewardBuilder(plugin.getConfigFile().getData(), plugin.getConfigFile().getVoteRemindingRewardsPath())
				.withPlaceHolder("sitesavailable", "" + user.getSitesNotVotedOn()).setGiveOffline(false).send(user);
	}

	/**
	 * Load remind checking.
	 */
	public void loadRemindChecking() {
		if (timer != null) {
			timer.shutdownNow();
		}
		if (plugin.getConfigFile().getVoteRemindingRemindDelay() > 0) {
			timer = Executors.newScheduledThreadPool(1);
			timer.scheduleWithFixedDelay(new Runnable() {

				@Override
				public void run() {
					if (plugin != null && plugin.isEnabled()) {
						for (Player player : Bukkit.getServer().getOnlinePlayers()) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
							if (!plugin.getOptions().isTreatVanishAsOffline() || !user.isVanished()) {
								checkRemind(user);
							}
						}
					} else {
						timer.shutdown();
					}
				}
			}, 30, plugin.getConfigFile().getVoteRemindingRemindDelay() * 60, TimeUnit.SECONDS);
		}
	}

	/**
	 * Run remind.
	 *
	 * @param user the user
	 */
	public void runRemind(VotingPluginUser user) {
		if (plugin.getConfigFile().getVoteRemindingEnabled()) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(user.getPrimaryAccount());
			}
			giveReward(user);
			if (user.getData().hasData() && plugin.getConfigFile().getVoteRemindingRemindOnlyOnce()) {
				user.setReminded(true);
			}

			plugin.debug(user.getPlayerName() + " was reminded!");
		}
	}

	public boolean shouldRemind(VotingPluginUser user) {
		if (remindersEnabled.containsKey(user.getJavaUUID())) {
			if (!remindersEnabled.get(user.getJavaUUID()).booleanValue()) {

				return false;
			}
		}
		if (user.shouldBeReminded()) {
			boolean hasPermAll = user.hasPermission("VotingPlugin.Login.RemindVotes.All");
			if (hasPermAll) {
				return user.canVoteAll();
			} else {
				return user.canVoteAny();
			}
		} else {
			plugin.debug(user.getUUID() + " no need to remind because plaeyr has VotingPlugin.NoRemind");
			return false;
		}
	}

	public void runRemindLogin(VotingPluginUser user) {
		if (plugin.getConfigFile().getVoteRemindingEnabled()) {
			if (user.hasPermission("VotingPlugin.Login.RemindVotes") || user.hasPermission("VotingPlugin.Player")) {
				if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser(user.getPrimaryAccount());
				}
				if ((!plugin.getVotingPluginUserManager().getAllUUIDs().contains(user.getUUID()) || shouldRemind(user))) {
					giveReward(user);
					if (user.getData().hasData()) {
						user.setReminded(true);
					}
					plugin.debug(user.getPlayerName() + " was reminded!");

				} else {
					plugin.debug("Not reminding for " + user.getUUID());
				}
			} else {
				plugin.debug(user.getUUID() + " has no permission for reminders");
			}
		}
	}
}
