package com.Ben12345rocks.VotingPlugin.SpecialRewards;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Config.SpecialRewardsConfig;
import com.Ben12345rocks.VotingPlugin.Events.PlayerSpecialRewardEvent;
import com.Ben12345rocks.VotingPlugin.Events.SpecialRewardType;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class SpecialRewards {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The instance. */
	static SpecialRewards instance = new SpecialRewards();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of OtherVoteReward.
	 *
	 * @return single instance of OtherVoteReward
	 */
	public static SpecialRewards getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new other vote reward.
	 */
	private SpecialRewards() {
	}

	/**
	 * Instantiates a new other vote reward.
	 *
	 * @param plugin the plugin
	 */
	public SpecialRewards(Main plugin) {
		SpecialRewards.plugin = plugin;
	}

	/**
	 * Check all sites.
	 *
	 * @param user the user
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
	 * @param user the user
	 * @return true, if successful
	 */
	public boolean checkCumualativeVotes(User user, String bungeeTextTotals) {
		boolean gotCumulativeAny = false;
		Set<String> votes = SpecialRewardsConfig.getInstance().getCumulativeVotes();
		for (String vote : votes) {
			if (StringParser.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (SpecialRewardsConfig.getInstance().getCumulativeRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(SpecialRewardsConfig.getInstance().getData(),
									SpecialRewardsConfig.getInstance().getCumulativeRewardsPath(votesRequired))) {
						boolean gotCumulative = false;
						int total = 0;
						boolean useBungeeTotalNum = false;
						String[] data = new String[] { "0", "0", "0", "0" };
						if (bungeeTextTotals != null) {
							data = bungeeTextTotals.split("//");
							useBungeeTotalNum = true;
						}
						if (SpecialRewardsConfig.getInstance().getCumulativeVotesInSameDay(votesRequired)) {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.Daily);
							} else {
								total = Integer.parseInt(data[3]);
							}
						} else if (SpecialRewardsConfig.getInstance().getCumulativeVotesInSameWeek(votesRequired)) {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.Weekly);
							} else {
								total = Integer.parseInt(data[2]);
							}
						} else if (SpecialRewardsConfig.getInstance().getCumulativeVotesInSameMonth(votesRequired)) {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.Monthly);
							} else {
								total = Integer.parseInt(data[1]);
							}
						} else {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.AllTime);
							} else {
								total = Integer.parseInt(data[0]);
							}
						}

						if (total != 0 && total >= votesRequired) {
							if ((total % votesRequired) == 0) {
								gotCumulative = true;
							}
						}

						if (gotCumulative) {
							gotCumulativeAny = true;
							giveCumulativeVoteReward(user, user.isOnline(), votesRequired);
							plugin.debug(user.getPlayerName() + " got cumulative " + votesRequired);
						} else {
							plugin.devDebug(user.getPlayerName() + " not able to get cumulative " + votesRequired);
						}
					}
				}
			} else {
				plugin.debug("Invalid cumulative number: " + vote);
			}
		}
		return gotCumulativeAny;
	}

	/**
	 * Check first vote.
	 *
	 * @param user the user
	 * @return true, if successful
	 */
	public boolean checkFirstVote(User user) {
		if (RewardHandler.getInstance().hasRewards(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getFirstVoteRewardsPath())) {
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
	 * @param user the user
	 * @return true, if successful
	 */
	public boolean checkMilestone(User user, String bungeeTextTotals) {
		int milestoneCount = user.getMilestoneCount();
		String[] data = bungeeTextTotals.split("//");
		if (data.length > 4) {
			try {
				milestoneCount = Integer.parseInt(data[5]);
			} catch (Exception e) {
				e.printStackTrace();
				milestoneCount = user.getMilestoneCount();
			}
		}
		if (Config.getInstance().isPreventRepeatMilestones()) {
			ArrayList<Integer> nums = new ArrayList<Integer>();

			for (String str : SpecialRewardsConfig.getInstance().getMilestoneVotes()) {
				try {
					nums.add(Integer.parseInt(str));
				} catch (Exception e) {
					plugin.getLogger().warning("Failed to get number from " + str);
				}
			}

			for (int num : nums) {
				if (milestoneCount > num) {
					if (!user.hasGottenMilestone(num)) {
						plugin.getLogger()
								.info("Milestone " + num + " for " + user.getPlayerName()
										+ " not already given when it should be, Current AllTimeTotal: "
										+ user.getTotal(TopVoter.AllTime) + ", Current MileStoneCount: "
										+ user.getMilestoneCount());
						user.setHasGotteMilestone(num, true);
					}
				}
			}
		}

		boolean gotMilestone = false;
		Set<String> votes = SpecialRewardsConfig.getInstance().getMilestoneVotes();
		for (String vote : votes) {
			if (StringParser.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (SpecialRewardsConfig.getInstance().getMilestoneRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(SpecialRewardsConfig.getInstance().getData(),
									SpecialRewardsConfig.getInstance().getMilestoneRewardsPath(votesRequired))) {

						int userVotesTotal = milestoneCount;
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

		Set<String> streaks = SpecialRewardsConfig.getInstance().getVoteStreakVotes(type);
		for (String streak : streaks) {
			String s = streak.replaceAll("-", "");
			boolean multiple = false;
			if (streak.contains("-")) {
				// multiple
				multiple = true;
			}
			if (StringParser.getInstance().isInt(s)) {
				int streakRequired = Integer.parseInt(s);
				if (streakRequired != 0) {
					if (SpecialRewardsConfig.getInstance().getVoteStreakRewardEnabled(type, streak)
							&& RewardHandler.getInstance().hasRewards(SpecialRewardsConfig.getInstance().getData(),
									SpecialRewardsConfig.getInstance().getVoteStreakRewardsPath(type, "" + streak))) {
						int curStreak = 0;
						if (type.equalsIgnoreCase("day")) {
							curStreak = user.getDayVoteStreak();
						} else if (type.equalsIgnoreCase("week")) {
							curStreak = user.getWeekVoteStreak();
						} else if (type.equalsIgnoreCase("month")) {
							curStreak = user.getMonthVoteStreak();
						}
						if (!multiple) {
							if (curStreak == streakRequired) {
								giveVoteStreakReward(user, user.isOnline(), type, "" + streakRequired, curStreak);
								gotReward = true;
								plugin.debug(
										user.getPlayerName() + " got VoteStreak " + streakRequired + " for " + type);
							}
						} else {
							if (curStreak != 0 && curStreak % streakRequired == 0) {
								giveVoteStreakReward(user, user.isOnline(), type, streakRequired + "-", curStreak);
								gotReward = true;
								plugin.debug(
										user.getPlayerName() + " got VoteStreak " + streakRequired + "* for " + type);
							}
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
	 * @param user   the user
	 * @param online the online
	 */
	public void giveAllSitesRewards(User user, boolean online) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user, SpecialRewardType.ALLSITE);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		RewardHandler.getInstance().giveReward(user, SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getAllSitesRewardPath(), new RewardOptions().setOnline(online));
	}

	/**
	 * Give cumulative vote reward.
	 *
	 * @param user       the user
	 * @param online     the online
	 * @param cumulative the cumulative
	 */
	public void giveCumulativeVoteReward(User user, boolean online, int cumulative) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.CUMMULATIVE.setAmount(cumulative));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}

		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getCumulativeRewardsPath(cumulative)).setOnline(online)
						.withPlaceHolder("Cumulative", "" + cumulative).send(user);
	}

	/**
	 * Give first vote rewards.
	 *
	 * @param user   the user
	 * @param online the online
	 */
	public void giveFirstVoteRewards(User user, boolean online) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user, SpecialRewardType.FIRSTVOTE);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		RewardHandler.getInstance().giveReward(user, SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getFirstVoteRewardsPath(), new RewardOptions().setOnline(online));
	}

	/**
	 * Give milestone vote reward.
	 *
	 * @param user      the user
	 * @param online    the online
	 * @param milestone the milestone
	 */
	public void giveMilestoneVoteReward(User user, boolean online, int milestone) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.MILESTONE.setAmount(milestone));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getMilestoneRewardsPath(milestone)).setOnline(online)
						.withPlaceHolder("Milestone", "" + milestone).send(user);
	}

	public void giveVoteStreakReward(User user, boolean online, String type, String string, int votes) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.VOTESTREAK.setType(type).setAmount(votes));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getVoteStreakRewardsPath(type, string)).setOnline(online)
						.withPlaceHolder("Type", type).withPlaceHolder("Streak", "" + votes).send(user);
	}

}
