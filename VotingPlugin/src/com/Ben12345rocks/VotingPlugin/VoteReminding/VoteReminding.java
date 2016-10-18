package com.Ben12345rocks.VotingPlugin.VoteReminding;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Configs.ConfigRewards;
import com.Ben12345rocks.AdvancedCore.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class VoteReminding {

	/** The instance. */
	static VoteReminding instance = new VoteReminding();

	/** The plugin. */
	static Main plugin = Main.plugin;

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
	 * @param plugin
	 *            the plugin
	 */
	public VoteReminding(Main plugin) {
		VoteReminding.plugin = plugin;
	}

	/**
	 * Check remind.
	 *
	 * @param user
	 *            the user
	 */
	public void checkRemind(User user) {
		String playerName = user.getPlayerName();

		if (Utils.getInstance().hasPermission(playerName,
				"VotingPlugin.Login.RemindVotes")
				|| Utils.getInstance().hasPermission(playerName,
						"VotingPlugin.Player")) {
			if (user.canVoteAll()) {
				Player player = Bukkit.getPlayer(playerName);
				if (player != null) {
					if (!Config.getInstance().getVoteRemindingRemindOnlyOnce()) {
						runRemind(user);
						user.setReminded(true);
					} else if (!user.getReminded()) {
						runRemind(user);
						user.setReminded(true);
					}
				}

			}

		}
	}

	/**
	 * Load remind checking.
	 */
	public void loadRemindChecking() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(
				plugin,
				new Runnable() {

					@Override
					public void run() {
						for (Player player : Bukkit.getServer()
								.getOnlinePlayers()) {
							User user = new User(player);
							checkRemind(user);
						}
					}
				}, 10,
				Config.getInstance().getVoteRemindingRemindDelay() * 20 * 60);
	}

	/**
	 * Run remind.
	 *
	 * @param user
	 *            the user
	 */
	public void runRemind(User user) {
		if (Config.getInstance().getVoteRemindingEnabled() && user.canVoteAll()) {
			user.setReminded(true);
			for (String reward : Config.getInstance().getVoteRemindingRewards()) {
				if (!reward.equalsIgnoreCase("")) {
					if (!ConfigRewards.getInstance().isRewardValid(reward)) {
						ConfigRewards.getInstance().setMessagesReward(reward,
								"&cRemember to vote");
					}
					Reward rewardFile = ConfigRewards.getInstance().getReward(
							reward);

					rewardFile.giveReward(user, true);
				}
			}

			plugin.debug(user.getPlayerName() + " was reminded!");

		}
	}

}
