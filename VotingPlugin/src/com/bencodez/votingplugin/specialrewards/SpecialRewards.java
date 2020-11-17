package com.bencodez.votingplugin.specialrewards;

import java.util.ArrayList;
import java.util.Set;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;
import com.bencodez.votingplugin.events.SpecialRewardType;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class SpecialRewards {

	private VotingPluginMain plugin;

	public SpecialRewards(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Check all sites.
	 *
	 * @param user the user
	 * @return true, if successful
	 */
	public boolean checkAllSites(VotingPluginUser user) {
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
	public boolean checkCumualativeVotes(VotingPluginUser user, String bungeeTextTotals) {
		boolean gotCumulativeAny = false;
		Set<String> votes = plugin.getSpecialRewardsConfig().getCumulativeVotes();
		for (String vote : votes) {
			if (StringParser.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (plugin.getSpecialRewardsConfig().getCumulativeRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getCumulativeRewardsPath(votesRequired))) {
						boolean gotCumulative = false;
						int total = 0;
						boolean useBungeeTotalNum = false;
						String[] data = new String[] { "0", "0", "0", "0" };
						if (bungeeTextTotals != null) {
							data = bungeeTextTotals.split("//");
							useBungeeTotalNum = true;
						}
						if (plugin.getSpecialRewardsConfig().getCumulativeVotesInSameDay(votesRequired)) {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.Daily);
							} else {
								total = Integer.parseInt(data[3]);
							}
						} else if (plugin.getSpecialRewardsConfig().getCumulativeVotesInSameWeek(votesRequired)) {
							if (!useBungeeTotalNum) {
								total = user.getTotal(TopVoter.Weekly);
							} else {
								total = Integer.parseInt(data[2]);
							}
						} else if (plugin.getSpecialRewardsConfig().getCumulativeVotesInSameMonth(votesRequired)) {
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
	public boolean checkFirstVote(VotingPluginUser user) {
		if (RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getFirstVoteRewardsPath())) {
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
	public boolean checkMilestone(VotingPluginUser user, String bungeeTextTotals) {
		int milestoneCount = user.getMilestoneCount();
		if (bungeeTextTotals != null) {
			String[] data = bungeeTextTotals.split("//");
			if (data.length > 4) {
				try {
					milestoneCount = Integer.parseInt(data[5]);
				} catch (Exception e) {
					e.printStackTrace();
					milestoneCount = user.getMilestoneCount();
				}
			}
		}
		if (plugin.getConfigFile().isPreventRepeatMilestones()) {
			ArrayList<Integer> nums = new ArrayList<Integer>();

			for (String str : plugin.getSpecialRewardsConfig().getMilestoneVotes()) {
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
		Set<String> votes = plugin.getSpecialRewardsConfig().getMilestoneVotes();
		for (String vote : votes) {
			if (StringParser.getInstance().isInt(vote)) {
				int votesRequired = Integer.parseInt(vote);
				if (votesRequired != 0) {
					if (plugin.getSpecialRewardsConfig().getMilestoneRewardEnabled(votesRequired)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getMilestoneRewardsPath(votesRequired))) {

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

	public boolean checkVoteStreak(VotingPluginUser user, String type) {
		boolean gotReward = false;

		Set<String> streaks = plugin.getSpecialRewardsConfig().getVoteStreakVotes(type);
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
					if (plugin.getSpecialRewardsConfig().getVoteStreakRewardEnabled(type, streak)
							&& RewardHandler.getInstance().hasRewards(plugin.getSpecialRewardsConfig().getData(),
									plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath(type, "" + streak))) {
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
	public void giveAllSitesRewards(VotingPluginUser user, boolean online) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user, SpecialRewardType.ALLSITE);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		RewardHandler.getInstance().giveReward(user, plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getAllSitesRewardPath(), new RewardOptions().setOnline(online));
	}

	/**
	 * Give cumulative vote reward.
	 *
	 * @param user       the user
	 * @param online     the online
	 * @param cumulative the cumulative
	 */
	public void giveCumulativeVoteReward(VotingPluginUser user, boolean online, int cumulative) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.CUMMULATIVE.setAmount(cumulative));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}

		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getCumulativeRewardsPath(cumulative)).setOnline(online)
						.withPlaceHolder("Cumulative", "" + cumulative).send(user);
	}

	/**
	 * Give first vote rewards.
	 *
	 * @param user   the user
	 * @param online the online
	 */
	public void giveFirstVoteRewards(VotingPluginUser user, boolean online) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user, SpecialRewardType.FIRSTVOTE);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		RewardHandler.getInstance().giveReward(user, plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getFirstVoteRewardsPath(), new RewardOptions().setOnline(online));
	}

	/**
	 * Give milestone vote reward.
	 *
	 * @param user      the user
	 * @param online    the online
	 * @param milestone the milestone
	 */
	public void giveMilestoneVoteReward(VotingPluginUser user, boolean online, int milestone) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.MILESTONE.setAmount(milestone));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getMilestoneRewardsPath(milestone)).setOnline(online)
						.withPlaceHolder("Milestone", "" + milestone).send(user);
	}

	public void giveVoteStreakReward(VotingPluginUser user, boolean online, String type, String string, int votes) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.VOTESTREAK.setType(type).setAmount(votes));
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath(type, string)).setOnline(online)
						.withPlaceHolder("Type", type).withPlaceHolder("Streak", "" + votes).send(user);
	}

}
