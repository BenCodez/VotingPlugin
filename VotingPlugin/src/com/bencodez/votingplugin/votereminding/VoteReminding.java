package com.bencodez.votingplugin.votereminding;

import java.util.Timer;
import java.util.TimerTask;

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

	private Timer timer;

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
			if (user.canVoteAny() || (user.canVoteAll() && user.hasPermission("VotingPlugin.Login.RemindVotes.All"))) {
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
				.withPlaceHolder("sitesavailable", "" + user.getSitesNotVotedOn()).setGiveOffline(false).send(user);
	}

	/**
	 * Load remind checking.
	 */
	public void loadRemindChecking() {
		if (timer != null) {
			timer.cancel();
		}
		if (plugin.getConfigFile().getVoteRemindingRemindDelay() > 0) {
			timer = new Timer();
			timer.schedule(new TimerTask() {

				@Override
				public void run() {
					if (plugin != null) {
						for (Player player : Bukkit.getServer().getOnlinePlayers()) {
							VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
							checkRemind(user);
						}
					}
				}
			}, 1000 * 30, plugin.getConfigFile().getVoteRemindingRemindDelay() * 1000 * 60);
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
			if ((!UserManager.getInstance().getAllUUIDs().contains(user.getUUID()) || (user.canVoteAll()
					|| (user.canVoteAny() && user.hasPermission("VotingPlugin.Login.RemindVotes.Any"))))
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
