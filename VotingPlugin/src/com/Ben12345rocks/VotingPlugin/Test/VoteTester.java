package com.Ben12345rocks.VotingPlugin.Test;

import java.util.ArrayList;

import com.Ben12345rocks.AdvancedCore.Rewards.Reward;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class VoteTester {

	private static VoteTester instance = new VoteTester();

	public static VoteTester getInstance() {
		return instance;
	}

	private Main plugin = Main.plugin;

	public void testRewards(int amount, String name, String rewardName) {
		long time1 = System.currentTimeMillis();
		ArrayList<Long> timesPerReward = new ArrayList<Long>();
		User user = UserManager.getInstance().getVotingPluginUser(name);
		Reward reward = RewardHandler.getInstance().getReward(rewardName);
		int rewardsGiven = 0;
		for (int i = 0; i < amount; i++) {
			long start1 = System.currentTimeMillis();
			if (reward.canGiveReward(user, new RewardOptions())) {
				rewardsGiven++;
				RewardHandler.getInstance().giveReward(user, reward, new RewardOptions().setIgnoreRequirements(true));
			}
			long start2 = System.currentTimeMillis();
			timesPerReward.add(start2 - start1);
		}
		long time2 = System.currentTimeMillis();
		long time = time2 - time1;
		long timeTotal = 0;
		for (Long t : timesPerReward) {
			timeTotal += t.longValue();
		}
		long timePerRewardAvg = timeTotal / timesPerReward.size();
		Main.plugin.getLogger().info("Time to process rewards (" + amount + "): " + time + " ms, average per reward "
				+ timePerRewardAvg + " ms. " + Main.plugin.getStorageType() + ", "
				+ UserManager.getInstance().getAllUUIDs().size() + " users. " + rewardsGiven + " rewards given");
	}

	public void testVotes(int amount, String name, String site) {
		long time1 = System.currentTimeMillis();
		ArrayList<Long> timesPerVote = new ArrayList<Long>();
		for (int i = 0; i < amount; i++) {
			long start1 = System.currentTimeMillis();
			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(site), name,
					plugin.getVoteSiteServiceSite(site), false);
			plugin.getServer().getPluginManager().callEvent(voteEvent);
			long start2 = System.currentTimeMillis();
			timesPerVote.add(start2 - start1);
		}
		long time2 = System.currentTimeMillis();
		long time = time2 - time1;
		long timeTotal = 0;
		for (Long t : timesPerVote) {
			timeTotal += t.longValue();
		}
		long timePerVoteAvg = timeTotal / timesPerVote.size();
		Main.plugin.getLogger()
				.info("Time to process votes (" + amount + "): " + time + " ms, average per vote " + timePerVoteAvg
						+ " ms. " + Main.plugin.getStorageType() + ", " + UserManager.getInstance().getAllUUIDs().size()
						+ " users. " + Main.plugin.getVoteSites().size() + " votesites");
	}

}
