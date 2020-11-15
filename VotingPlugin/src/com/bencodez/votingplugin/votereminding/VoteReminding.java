package com.bencodez.votingplugin.votereminding;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.misc.PlayerUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.usermanager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class VoteReminding {

	/** The instance. */
	static VoteReminding instance = new VoteReminding();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Gets the single instance of VoteReminding.
	 *
	 * @return single instance of VoteReminding
	 */
	public static VoteReminding getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new vote reminding.
	 */
	private VoteReminding() {
	}

	/**
	 * Instantiates a new vote reminding.
	 *
	 * @param plugin the plugin
	 */
	public VoteReminding(VotingPluginMain plugin) {
		VoteReminding.plugin = plugin;
	}

	/**
	 * Check remind.
	 *
	 * @param user the user
	 */
	public void checkRemind(User user) {
		String playerName = user.getPlayerName();

		if (PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Login.RemindVotes")
				|| PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Player")) {
			if (user.canVoteAll() || (user.canVoteAny() && user.hasPermission("VotingPlugin.Login.RemindVotes.Any"))) {
				Player player = Bukkit.getPlayer(playerName);
				if (player != null) {
					if (!Config.getInstance().getVoteRemindingRemindOnlyOnce()) {
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

	private void giveReward(User user) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getVoteRemindingRewardsPath())
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
					User user = UserManager.getInstance().getVotingPluginUser(player);
					checkRemind(user);
				}
			}
		}, 10, Config.getInstance().getVoteRemindingRemindDelay() * 20 * 60);
	}

	/**
	 * Run remind.
	 *
	 * @param user the user
	 */
	public void runRemind(User user) {
		if (Config.getInstance().getVoteRemindingEnabled()) {
			if (Config.getInstance().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
			}
			if (user.canVoteAll() && user.shouldBeReminded()) {
				user.setReminded(true);
				giveReward(user);

				plugin.debug(user.getPlayerName() + " was reminded!");

			}
		}
	}

	public void runRemindLogin(User user) {
		if (Config.getInstance().getVoteRemindingEnabled()) {
			if (Config.getInstance().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
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
