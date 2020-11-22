package com.bencodez.votingplugin.votereminding;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class VoteReminding {

	/** The plugin. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new vote reminding.
	 */
	public VoteReminding(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check remind.
	 *
	 * @param user the user
	 */
	public void checkRemind(VotingPluginUser user) {
		String playerName = user.getPlayerName();

		if (PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Login.RemindVotes")
				|| PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Player")) {
			if (user.canVoteAll() || (user.canVoteAny() && user.hasPermission("VotingPlugin.Login.RemindVotes.Any"))) {
				Player player = Bukkit.getPlayer(playerName);
				if (player != null) {
					if (!plugin.getConfigFile().getVoteRemindingRemindOnlyOnce()) {
						runRemind(user);
						user.setReminded(true);
					} else if (!user.isReminded()) {
						runRemind(user);
						user.setReminded(true);
					}
				}

			}

		}
	}

	private void giveReward(VotingPluginUser user) {
		new RewardBuilder(plugin.getConfigFile().getData(), plugin.getConfigFile().getVoteRemindingRewardsPath())
				.setGiveOffline(false).send(user);
	}

	/**
	 * Load remind checking.
	 */
	public void loadRemindChecking() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				for (Player player : Bukkit.getServer().getOnlinePlayers()) {
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
					checkRemind(user);
				}
			}
		}, 10, plugin.getConfigFile().getVoteRemindingRemindDelay() * 20 * 60);
	}

	/**
	 * Run remind.
	 *
	 * @param user the user
	 */
	public void runRemind(VotingPluginUser user) {
		if (plugin.getConfigFile().getVoteRemindingEnabled()) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
			}
			if (user.canVoteAll() && user.shouldBeReminded()) {
				user.setReminded(true);
				giveReward(user);

				plugin.debug(user.getPlayerName() + " was reminded!");

			}
		}
	}

	public void runRemindLogin(VotingPluginUser user) {
		if (plugin.getConfigFile().getVoteRemindingEnabled()) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
			}
			if ((!UserManager.getInstance().getAllUUIDs().contains(user.getUUID()) || user.canVoteAll())
					&& user.shouldBeReminded()) {
				giveReward(user);
				if (user.getData().hasData()) {
					user.setReminded(true);
				}
				plugin.debug(user.getPlayerName() + " was reminded!");

			}
		}
	}
}
