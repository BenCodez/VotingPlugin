package com.bencodez.votingplugin.votereminding;

import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
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
	private ConcurrentHashMap<UUID, Boolean> remindersEnabled = new ConcurrentHashMap<>();

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
				if (!plugin.getConfigFile().isVoteRemindingRemindOnlyOnce()) {
					runRemind(user);
				} else if (!user.isReminded()) {
					runRemind(user);
				}
			}

		}
	}

	private void giveReward(VotingPluginUser user) {
		new RewardBuilder(plugin.getConfigFile().getData(), plugin.getConfigFile().getVoteRemindingRewardsPath())
				.withPlaceHolder("sitesavailable", "" + user.getSitesNotVotedOn()).setGiveOffline(false)
				.disableDefaultWorlds().send(user);
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

	public void loadReminds() {
		List<String> uuidsStr = plugin.getServerData().getDisabledReminders();

		for (String str : uuidsStr) {
			remindersEnabled.put(UUID.fromString(str), Boolean.FALSE);
		}

		plugin.getTimer().scheduleAtFixedRate(new Runnable() {

			@Override
			public void run() {
				saveReminds();
			}
		}, 24, 24, TimeUnit.HOURS);
	}

	/**
	 * Run remind.
	 *
	 * @param user the user
	 */
	public void runRemind(VotingPluginUser user) {
		if (plugin.getConfigFile().isVoteRemindingEnabled()) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(user.getPrimaryAccount());
			}
			giveReward(user);
			if (user.getData().hasData() && plugin.getConfigFile().isVoteRemindingRemindOnlyOnce()) {
				user.setReminded(true);
			}

			plugin.debug(user.getPlayerName() + " was reminded!");
		}
	}

	public void runRemindLogin(VotingPluginUser user) {
		if (plugin.getConfigFile().isVoteRemindingEnabled()) {
			if (user.hasPermission("VotingPlugin.Login.RemindVotes") || user.hasPermission("VotingPlugin.Player")) {
				if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
					user = plugin.getVotingPluginUserManager().getVotingPluginUser(user.getPrimaryAccount());
				}
				if ((!plugin.getVotingPluginUserManager().getAllUUIDs().contains(user.getUUID())
						|| shouldRemind(user))) {
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

	public void saveReminds() {
		ArrayList<UUID> remindersDisabled = new ArrayList<>();
		for (Entry<UUID, Boolean> entry : remindersEnabled.entrySet()) {
			if (remindersEnabled.containsKey(entry.getKey())) {
				if (!remindersEnabled.get(entry.getKey()).booleanValue()) {
					remindersDisabled.add(entry.getKey());
				}
			}
		}

		plugin.getServerData().saveDisabledReminders(remindersDisabled);
	}

	public boolean shouldRemind(VotingPluginUser user) {
		if (remindersEnabled.containsKey(user.getJavaUUID())) {
			if (!remindersEnabled.get(user.getJavaUUID()).booleanValue()) {

				return false;
			}
		}
		if (!user.shouldBeReminded()) {
			plugin.debug(user.getUUID() + " no need to remind because player has VotingPlugin.NoRemind");
			return false;
		}
		boolean hasPermAll = user.hasPermission("VotingPlugin.Login.RemindVotes.All");
		if (hasPermAll) {
			return user.canVoteAll();
		}
		return user.canVoteAny();
	}
}
