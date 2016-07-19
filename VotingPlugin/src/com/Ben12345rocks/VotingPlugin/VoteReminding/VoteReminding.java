package com.Ben12345rocks.VotingPlugin.VoteReminding;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteReminding;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class VoteReminding {

	static VoteReminding instance = new VoteReminding();

	static Main plugin = Main.plugin;

	public static VoteReminding getInstance() {
		return instance;
	}

	private VoteReminding() {
	}

	public VoteReminding(Main plugin) {
		VoteReminding.plugin = plugin;
	}

	public void checkRemind(User user) {
		String playerName = user.getPlayerName();

		if (Utils.getInstance().hasPermission(playerName,
				"VotingPlugin.Login.RemindVotes")
				|| Utils.getInstance().hasPermission(playerName,
						"VotingPlugin.Player")) {
			if (user.canVoteAll()) {
				Player player = Bukkit.getPlayer(playerName);
				if (player != null) {
					if (!ConfigVoteReminding.getInstance().getRemindOnlyOnce()) {
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
				ConfigVoteReminding.getInstance().getRemindDelay() * 20 * 60);
	}

	public void runRemind(User user) {
		if (ConfigVoteReminding.getInstance().getEnabled() && user.canVoteAll()) {
			user.setReminded(true);
			for (String reward : ConfigVoteReminding.getInstance().getRewards()) {
				if (!reward.equalsIgnoreCase("")) {
					if (!ConfigRewards.getInstance().getRewardFile(reward)
							.exists()) {
						ConfigRewards.getInstance().setMessagesReward(reward,
								"&cRemember to vote");
					}
					Reward rewardFile = ConfigRewards.getInstance().getReward(
							reward);

					rewardFile.giveReward(user);
				}
			}

			plugin.debug(user.getPlayerName() + " was reminded!");

		}
	}

}
