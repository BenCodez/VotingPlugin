package com.bencodez.votingplugin.specialrewards;

import java.util.Set;
import java.util.UUID;

import org.bukkit.Bukkit;

import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;
import com.bencodez.votingplugin.events.SpecialRewardType;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class SpecialRewards {

	private VotingPluginMain plugin;

	/**
	 * Constructs a new SpecialRewards handler.
	 *
	 * @param plugin the main plugin instance
	 */
	public SpecialRewards(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	/**
	 * Checks and processes vote streak rewards for a user.
	 *
	 * @param voteUUID the vote UUID
	 * @param user the voting user
	 * @param type the streak type (day/week/month)
	 * @param forceBungee whether to force bungee mode
	 * @return true if a reward was given
	 */
	public boolean checkVoteStreak(UUID voteUUID, VotingPluginUser user, String type, boolean forceBungee) {
		boolean gotReward = false;

		Set<String> streaks = plugin.getSpecialRewardsConfig().getVoteStreakVotes(type);
		for (String streak : streaks) {
			boolean multiple = false;
			if (streak.contains("-")) {
				// multiple
				multiple = true;

			}
			String s = streak.replaceAll("-", "");

			plugin.debug("Streak: " + streak + " multiple: " + multiple);
			if (MessageAPI.isInt(s)) {
				int streakRequired = Integer.parseInt(s);
				if (streakRequired != 0) {
					if (plugin.getSpecialRewardsConfig().getVoteStreakRewardEnabled(type, streak)
							&& plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(),
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
								giveVoteStreakReward(voteUUID, user, user.isOnline(), type, "" + streakRequired,
										curStreak, forceBungee);
								gotReward = true;
								plugin.debug(
										user.getPlayerName() + " got VoteStreak " + streakRequired + " for " + type);
							}
						} else {
							if (curStreak != 0 && curStreak % streakRequired == 0) {
								giveVoteStreakReward(voteUUID, user, user.isOnline(), type, streak, curStreak,
										forceBungee);
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
	 * Gives a vote streak reward to a user.
	 *
	 * @param voteUUID the vote UUID
	 * @param user the voting user
	 * @param online whether the user is online
	 * @param type the streak type
	 * @param string the streak value
	 * @param votes the current streak count
	 * @param forceBungee whether to force bungee mode
	 */
	public void giveVoteStreakReward(UUID voteUUID, VotingPluginUser user, boolean online, String type, String string,
			int votes, boolean forceBungee) {
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(user,
				SpecialRewardType.VOTESTREAK.setType(type).setAmount(votes), voteUUID);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getVoteStreakRewardsPath(type, string)).setOnline(online)
				.withPlaceHolder("Type", type).setServer(forceBungee).withPlaceHolder("Streak", "" + votes).send(user);
	}

}
