package com.Ben12345rocks.VotingPlugin.VoteReminding;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.VoteUser;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import ninja.egg82.patterns.ServiceLocator;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class VoteReminding {

	/** The instance. */
	static VoteReminding instance = new VoteReminding();

	private Main main = ServiceLocator.getService(Main.class);

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
	 * Check remind.
	 *
	 * @param user
	 *            the user
	 */
	public void checkRemind(VoteUser user) {
		String playerName = user.getPlayerName();

		if (PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Login.RemindVotes")
				|| PlayerUtils.getInstance().hasServerPermission(playerName, "VotingPlugin.Player")) {
			if (user.canVoteAll()) {
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

	private void giveReward(VoteUser user) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getVoteRemindingRewardsPath())
				.setGiveOffline(false).send(user);
	}

	/**
	 * Load remind checking.
	 */
	public void loadRemindChecking() {
		Bukkit.getScheduler().runTaskTimerAsynchronously(main, new Runnable() {

			@Override
			public void run() {
				for (Player player : Bukkit.getServer().getOnlinePlayers()) {
					VoteUser user = UserManager.getInstance().getVotingPluginUser(player);
					checkRemind(user);
				}
			}
		}, 10, Config.getInstance().getVoteRemindingRemindDelay() * 20 * 60);
	}

	/**
	 * Run remind.
	 *
	 * @param user
	 *            the user
	 */
	public void runRemind(VoteUser user) {
		if (Config.getInstance().getVoteRemindingEnabled() && user.canVoteAll() && user.shouldBeReminded()) {
			user.setReminded(true);
			giveReward(user);

			main.debug(user.getPlayerName() + " was reminded!");

		}
	}

	public void runRemindLogin(VoteUser user) {
		if (Config.getInstance().getVoteRemindingEnabled()
				&& (!UserManager.getInstance().getAllUUIDs().contains(user.getUUID()) || user.canVoteAll())
				&& user.shouldBeReminded()) {
			giveReward(user);
			if (user.getData().hasData()) {
				user.setReminded(true);
			}
			main.debug(user.getPlayerName() + " was reminded!");

		}
	}
}
