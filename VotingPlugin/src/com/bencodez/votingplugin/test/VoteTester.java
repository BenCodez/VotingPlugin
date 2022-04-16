package com.bencodez.votingplugin.test;

import java.util.ArrayList;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteTester {

	private static VoteTester instance = new VoteTester();

	public static VoteTester getInstance() {
		return instance;
	}

	private VotingPluginMain plugin = VotingPluginMain.plugin;

	public void testRewards(int amount, String name, String rewardName) {
		long time1 = System.currentTimeMillis();
		ArrayList<Long> timesPerReward = new ArrayList<Long>();
		VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(name);
		Reward reward = plugin.getRewardHandler().getReward(rewardName);
		int rewardsGiven = 0;
		for (int i = 0; i < amount; i++) {
			long start1 = System.currentTimeMillis();
			if (reward.canGiveReward(user, new RewardOptions())) {
				rewardsGiven++;
				plugin.getRewardHandler().giveReward(user, reward, new RewardOptions().setIgnoreRequirements(true));
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
		VotingPluginMain.plugin.getLogger().info("Time to process rewards (" + amount + "): " + time
				+ " ms, average per reward " + timePerRewardAvg + " ms. " + VotingPluginMain.plugin.getStorageType()
				+ ", " + UserManager.getInstance().getAllUUIDs().size() + " users. " + rewardsGiven + " rewards given");
	}

	public void testVotes(int amount, String name, String site) {
		long time1 = System.currentTimeMillis();
		ArrayList<Long> timesPerVote = new ArrayList<Long>();
		for (int i = 0; i < amount; i++) {
			long start1 = System.currentTimeMillis();
			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(site, false), name,
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
		VotingPluginMain.plugin.getLogger()
				.info("Time to process votes (" + amount + "): " + time + " ms, average per vote " + timePerVoteAvg
						+ " ms. " + VotingPluginMain.plugin.getStorageType() + ", "
						+ UserManager.getInstance().getAllUUIDs().size() + " users. "
						+ VotingPluginMain.plugin.getVoteSites().size() + " votesites");
	}

}
