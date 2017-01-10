package com.Ben12345rocks.VotingPlugin.OtherRewards;

import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.MonthChangeEvent;
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
		return user.checkAllVotes();

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
							&& Config.getInstance().getCumulativeRewards(votesRequired).size() != 0) {
						if (Config.getInstance().getCumulativeVotesInSameDay(votesRequired)) {
							int userVotesTotal = user.getDailyTotal();
							if ((userVotesTotal % votesRequired) == 0 && userVotesTotal != 0) {
								user.addOfflineOtherReward("Cumulative" + votesRequired);
								gotCumulative = true;
							}
						} else if (Config.getInstance().getCumulativeVotesInSameWeek(votesRequired)) {
							int userVotesTotal = user.getWeeklyTotal();
							if ((userVotesTotal % votesRequired) == 0 && userVotesTotal != 0) {
								user.addOfflineOtherReward("Cumulative" + votesRequired);
								gotCumulative = true;
							}
						} else {
							int userVotesTotal = user.getAllTimeTotal();
							if ((userVotesTotal % votesRequired) == 0 && userVotesTotal != 0) {
								user.addOfflineOtherReward("Cumulative" + votesRequired);
								gotCumulative = true;
							}
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
		if (Config.getInstance().getFirstVoteRewards().size() != 0) {
			if (!user.hasGottenFirstVote()) {
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
							&& Config.getInstance().getMilestoneRewards(votesRequired).size() != 0) {

						int userVotesTotal = user.getMileStoneTotal();
						if (userVotesTotal >= votesRequired && !user.hasGottenMilestone(votesRequired)) {
							user.addOfflineOtherReward("MileStone" + votesRequired);
							user.setHasGotteMilestone(votesRequired, true);
							gotMilestone = true;
						}
					}
				}
			} else {
				plugin.debug("Invalid milestone number: " + vote);
			}
		}
		return gotMilestone;
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
		for (String reward : Config.getInstance().getAllSitesReward()) {
			RewardHandler.getInstance().giveReward(user, reward, online);
		}
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
		for (String reward : Config.getInstance().getCumulativeRewards(cumulative)) {
			if (reward != "") {
				RewardHandler.getInstance().giveReward(user, reward, online);
			}
		}
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
		for (String reward : Config.getInstance().getFirstVoteRewards()) {

			RewardHandler.getInstance().giveReward(user, reward, online);

		}
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
		for (String reward : Config.getInstance().getMilestoneRewards(milestone)) {
			RewardHandler.getInstance().giveReward(user, reward, online);
		}
	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		Set<String> votes = Config.getInstance().getMilestoneVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				plugin.debug("Is int: " + vote);
				if (votesRequired != 0) {
					plugin.debug("not 0");
					if (Config.getInstance().getMilestoneRewardEnabled(votesRequired)
							&& Config.getInstance().getMilestoneRewards(votesRequired).size() != 0) {
						if (Config.getInstance().getMilestoneResetMonthly(votesRequired)) {
							for (String uuid : UserManager.getInstance().getAllUUIDs()) {
								User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
								user.setMileStoneTotal(0);
								user.setHasGotteMilestone(votesRequired, false);
							}
						}
					}
				}
			}
		}

	}
}
