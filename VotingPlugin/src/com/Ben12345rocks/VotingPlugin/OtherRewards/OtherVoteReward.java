package com.Ben12345rocks.VotingPlugin.OtherRewards;

import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Listeners.MonthChangeEvent;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class OtherVoteReward implements Listener {

	/** The bonus reward. */
	static ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

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

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		Set<String> votes = ConfigOtherRewards.getInstance().getMilestoneVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				plugin.debug("Is int: " + vote);
				if (votesRequired != 0) {
					plugin.debug("not 0");
					if (ConfigOtherRewards.getInstance().getMilestoneRewardEnabled(votesRequired)
							&& ConfigOtherRewards.getInstance().getMilestoneRewards(votesRequired).size() != 0) {
						if (ConfigOtherRewards.getInstance().getMilestoneResetMonthly(votesRequired)) {
							for (User user : UserManager.getInstance().getVotingPluginUsers()) {
								user.setTotalMileStone(votesRequired, 0);
								user.setHasGotteMilestone(votesRequired, false);
							}
						}
					}
				}
			}
		}

	}

	/**
	 * Check cumualative votes.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkCumualativeVotes(User user) {
		Set<String> votes = ConfigOtherRewards.getInstance().getCumulativeVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (ConfigOtherRewards.getInstance().getCumulativeRewardEnabled(votesRequired)
							&& ConfigOtherRewards.getInstance().getCumulativeRewards(votesRequired).size() != 0) {
						if (ConfigOtherRewards.getInstance().getCumulativeVotesInSameDay(votesRequired)) {
							int userVotesTotal = user.getTotalVotesToday();
							if ((userVotesTotal % votesRequired) == 0) {
								Data.getInstance().setCumuatliveVotesOffline(user, votesRequired,
										Data.getInstance().getCumulativeVotesOffline(user, votesRequired) + 1);
								return true;
							}
						} else if (ConfigOtherRewards.getInstance().getCumulativeVotesInSameWeek(votesRequired)) {
							int userVotesTotal = user.getTotalWeeklyAll();
							if ((userVotesTotal % votesRequired) == 0) {
								Data.getInstance().setCumuatliveVotesOffline(user, votesRequired,
										Data.getInstance().getCumulativeVotesOffline(user, votesRequired) + 1);
								return true;
							}
						} else {
							int userVotesTotal = user.getTotalVotes();
							if ((userVotesTotal % votesRequired) == 0) {
								Data.getInstance().setCumuatliveVotesOffline(user, votesRequired,
										Data.getInstance().getCumulativeVotesOffline(user, votesRequired) + 1);
								return true;
							}
						}
					}
				}
			} else {
				plugin.debug("Invalid cumulative number: " + vote);
			}
		}
		return false;
	}

	/**
	 * Check first vote.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkFirstVote(User user) {
		int userVotesTotal = user.getTotalVotes();
		if (ConfigOtherRewards.getInstance().getFirstVoteRewards().size() != 0) {
			if (userVotesTotal <= 1 && !user.hasGottenFirstVote()) {
				user.setHasGottenFirstVote(true);
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
		Set<String> votes = ConfigOtherRewards.getInstance().getMilestoneVotes();
		for (String vote : votes) {
			if (StringUtils.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				plugin.debug("Is int: " + vote);
				if (votesRequired != 0) {
					plugin.debug("not 0");
					if (ConfigOtherRewards.getInstance().getMilestoneRewardEnabled(votesRequired)
							&& ConfigOtherRewards.getInstance().getMilestoneRewards(votesRequired).size() != 0) {

						int userVotesTotal = user.getTotalMileStone(votesRequired);
						if (userVotesTotal >= votesRequired && !user.hasGottenMilestone(votesRequired)) {
							user.setOfflineMilestoneVotes(votesRequired,
									user.getOfflineMilestoneVotes(votesRequired) + 1);
							user.setHasGotteMilestone(votesRequired, true);
							return true;

						}
					}
				}
			} else {
				plugin.debug("Invalid milestone number: " + vote);
			}
		}
		return false;
	}

	/**
	 * Check min votes.
	 *
	 * @param user
	 *            the user
	 * @param votes
	 *            the votes
	 * @return true, if successful
	 */
	public boolean checkMinVotes(User user, int votes) {
		if (ConfigOtherRewards.getInstance().getMinVotesEnabled()) {
			int minVotes = ConfigOtherRewards.getInstance().getMinVotesVotes();
			if (minVotes > votes) {
				return true;
			}
		}

		return false;
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
		for (String reward : ConfigOtherRewards.getInstance().getAllSitesReward()) {
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
		for (String reward : ConfigOtherRewards.getInstance().getCumulativeRewards(cumulative)) {
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
		for (String reward : ConfigOtherRewards.getInstance().getFirstVoteRewards()) {

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
		for (String reward : ConfigOtherRewards.getInstance().getMilestoneRewards(milestone)) {
			RewardHandler.getInstance().giveReward(user, reward, online);
		}
	}

	/**
	 * Give min votes reward.
	 *
	 * @param user
	 *            the user
	 * @param online
	 *            the online
	 */
	public void giveMinVotesReward(User user, boolean online) {
		for (String reward : ConfigOtherRewards.getInstance().getMinVotesRewards()) {
			RewardHandler.getInstance().giveReward(user, reward, online);
		}

	}

}
