package com.Ben12345rocks.VotingPlugin.OtherRewards;

import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.MonthChangeEvent;
import com.Ben12345rocks.AdvancedCore.Objects.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class OtherVoteReward implements Listener {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The instance. */
	static OtherVoteReward instance = new OtherVoteReward();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of OtherVoteReward.
	 *
	 * @return single instance of OtherVoteReward
	 */
	public static OtherVoteReward getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new other vote reward.
	 */
	private OtherVoteReward() {
	}

	/**
	 * Instantiates a new other vote reward.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public OtherVoteReward(Main plugin) {
		OtherVoteReward.plugin = plugin;
	}

	/**
	 * Check all sites.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkAllSites(User user) {
		boolean checkAllVotes = user.checkAllVotes();
		if (checkAllVotes) {
			giveAllSitesRewards(user, user.isOnline());
		}
		return checkAllVotes;

	}

	/**
	 * Check cumualative votes.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkCumualativeVotes(User user) {
		boolean gotCumulative = false;
		Set<String> votes = Config.getInstance().getCumulativeVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (Config.getInstance().getCumulativeRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getCumulativeRewardsPath(votesRequired))) {
						int total = 0;
						if (Config.getInstance().getCumulativeVotesInSameDay(votesRequired)) {
							total = user.getDailyTotal();
						} else if (Config.getInstance().getCumulativeVotesInSameWeek(votesRequired)) {
							total = user.getWeeklyTotal();
						} else {
							total = user.getAllTimeTotal();
						}

						if ((total % votesRequired) == 0 && total != 0) {
							giveCumulativeVoteReward(user, user.isOnline(), votesRequired);
							gotCumulative = true;
						}

						if (gotCumulative) {
							plugin.debug(user.getPlayerName() + " got cumulative " + votesRequired);
						}
					}
				}
			} else {
				plugin.debug("Invalid cumulative number: " + vote);
			}
		}
		return gotCumulative;
	}

	/**
	 * Check first vote.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkFirstVote(User user) {
		if (RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
				Config.getInstance().getFirstVoteRewardsPath())) {
			if (!user.hasGottenFirstVote()) {
				giveFirstVoteRewards(user, user.isOnline());
				return true;
			}

		}
		return false;
	}

	/**
	 * Check milestone.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkMilestone(User user) {
		boolean gotMilestone = false;
		Set<String> votes = Config.getInstance().getMilestoneVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (Config.getInstance().getMilestoneRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getMilestoneRewardsPath(votesRequired))) {

						int userVotesTotal = user.getAllTimeTotal();
						if (userVotesTotal >= votesRequired && !user.hasGottenMilestone(votesRequired)) {
							giveMilestoneVoteReward(user, user.isOnline(), votesRequired);
							user.setHasGotteMilestone(votesRequired, true);
							plugin.debug(user.getPlayerName() + " got milestone " + votesRequired);

						}
					}
				}
			} else {
				plugin.debug("Invalid milestone number: " + vote);
			}
		}
		return gotMilestone;
	}

	public boolean checkVoteStreak(User user, String type) {
		boolean gotReward = false;

		Set<String> streaks = Config.getInstance().getVoteStreakVotes(type);
		for (String streak : streaks) {
			if (StringUtils.getInstance().isInt(streak)) {
				int streakRequired = Integer.parseInt(streak);
				if (streakRequired != 0) {
					if (Config.getInstance().getVoteStreakRewardEnabled(type, streakRequired)
							&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
									Config.getInstance().getVoteStreakRewardsPath(type, streakRequired))) {
						int curStreak = 0;
						if (type.equalsIgnoreCase("day")) {
							curStreak = user.getDayVoteStreak();
						} else if (type.equalsIgnoreCase("week")) {
							curStreak = user.getWeekVoteStreak();
						} else if (type.equalsIgnoreCase("month")) {
							curStreak = user.getMonthVoteStreak();
						}
						if (curStreak == streakRequired) {
							giveVoteStreakReward(user, user.isOnline(), type, streakRequired);
							gotReward = true;
						}
						if (gotReward) {
							plugin.debug(user.getPlayerName() + " got VoteStreak " + streakRequired + " for " + type);
						}
					}
				}
			}

		}

		return gotReward;

	}

	/**
	 * Give all sites rewards.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 */
	public void giveAllSitesRewards(User user, boolean online) {
		RewardHandler.getInstance().giveReward(user, Config.getInstance().getData(),
				Config.getInstance().getAllSitesRewardPath(), online);
	}

	/**
	 * Give cumulative vote reward.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 * @param cumulative
	 *            the cumulative
	 */
	public void giveCumulativeVoteReward(User user, boolean online, int cumulative) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getCumulativeRewardsPath(cumulative))
				.setOnline(online).withPlaceHolder("Cumulative", "" + cumulative).send(user);
	}

	/**
	 * Give first vote rewards.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 */
	public void giveFirstVoteRewards(User user, boolean online) {
		RewardHandler.getInstance().giveReward(user, Config.getInstance().getData(),
				Config.getInstance().getFirstVoteRewardsPath(), online);
	}

	/**
	 * Give milestone vote reward.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 * @param milestone
	 *            the milestone
	 */
	public void giveMilestoneVoteReward(User user, boolean online, int milestone) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getMilestoneRewardsPath(milestone))
				.setOnline(online).withPlaceHolder("Milestone", "" + milestone).send(user);
	}

	public void giveVoteStreakReward(User user, boolean online, String type, int streak) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getVoteStreakRewardsPath(type, streak))
				.setOnline(online).withPlaceHolder("Type", type).withPlaceHolder("Streak", "" + streak).send(user);
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@Override
			public void run() {
				Set<String> votes = Config.getInstance().getMilestoneVotes();
				for (String vote : votes) {
					if (StringUtils.getInstance().isInt(vote)) {
						int votesRequired = Integer.parseInt(vote);
						plugin.debug("Is int: " + vote);
						if (votesRequired != 0) {
							plugin.debug("not 0");
							if (Config.getInstance().getMilestoneRewardEnabled(votesRequired)
									&& RewardHandler.getInstance().hasRewards(Config.getInstance().getData(),
											Config.getInstance().getMilestoneRewardsPath(votesRequired))) {
								if (Config.getInstance().getMilestoneResetMonthly(votesRequired)) {
									for (String uuid : UserManager.getInstance().getAllUUIDs()) {
										User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
										user.setHasGotteMilestone(votesRequired, false);
									}
								}
							}
						}
					}
				}
			}
		});

	}
}
