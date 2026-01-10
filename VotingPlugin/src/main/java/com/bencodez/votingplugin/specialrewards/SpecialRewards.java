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

	public SpecialRewards(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

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
