package com.bencodez.votingplugin.test;

import java.util.ArrayList;
import java.util.Random;
import java.util.UUID;

import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.scheduler.BukkitScheduler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class VoteTester {

	public VoteTester(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	private VotingPluginMain plugin;

	public void testRewards(int amount, String name, String rewardName) {
		plugin.getVoteTimer().submit(new Runnable() {

			@Override
			public void run() {
				long time1 = System.currentTimeMillis();
				ArrayList<Long> timesPerReward = new ArrayList<Long>();
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(name);
				Reward reward = plugin.getRewardHandler().getReward(rewardName);
				int rewardsGiven = 0;
				for (int i = 0; i < amount; i++) {
					long start1 = System.currentTimeMillis();
					if (reward.canGiveReward(user, new RewardOptions())) {
						rewardsGiven++;
						plugin.getRewardHandler().giveReward(user, reward,
								new RewardOptions().setIgnoreRequirements(true));
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
				VotingPluginMain.plugin.getLogger()
						.info("Time to process rewards (" + amount + "): " + time + " ms, average per reward "
								+ timePerRewardAvg + " ms. " + VotingPluginMain.plugin.getStorageType() + ", "
								+ plugin.getVotingPluginUserManager().getAllUUIDs().size() + " users. " + rewardsGiven
								+ " rewards given");
			}

		});

	}

	private String getSaltString() {
		String SALTCHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";
		StringBuilder salt = new StringBuilder();
		Random rnd = new Random();
		while (salt.length() < 18) { // length of the random string.
			int index = (int) (rnd.nextFloat() * SALTCHARS.length());
			salt.append(SALTCHARS.charAt(index));
		}
		String saltStr = salt.toString();
		return saltStr;

	}

	public void generatePlayers(int numberOfPlayers) {
		if (plugin.getOptions().getDebug().isDebug()) {
			int num = 1;
			String salt = getSaltString();
			Random random = new Random();
			for (int i = 0; i < numberOfPlayers; i++) {
				UUID uuid = UUID.randomUUID();
				if (!plugin.getUserManager().userExist(uuid)) {
					String playerName = salt + num;
					if (!plugin.getUserManager().userExist(playerName)) {
						VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid,
								playerName);
						user.dontCache();
						user.setPoints(random.nextInt(100));
						for (TopVoter top : TopVoter.values()) {
							user.setTotal(top, random.nextInt(1000));
						}
						user.setMilestoneCount(random.nextInt(100));
						user.setPlayerName(playerName);
						user.updateName(true);
						num++;
						plugin.debug("Generated user " + uuid.toString() + "/" + playerName);
					}
				}
			}
		}
	}

	public void testVotes(int amount, String name, String site) {
		plugin.getVoteTimer().submit(new Runnable() {

			@Override
			public void run() {
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
						.info("Time to process votes (" + amount + "): " + time + " ms, average per vote "
								+ timePerVoteAvg + " ms. " + VotingPluginMain.plugin.getStorageType() + ", "
								+ plugin.getVotingPluginUserManager().getAllUUIDs().size() + " users. "
								+ VotingPluginMain.plugin.getVoteSites().size() + " votesites");
			}

		});

	}

	public void testSpam(int amount, String name, String site) {
		for (int i = 0; i < amount; i++) {
			BukkitScheduler.runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getVoteTimer().submit(new Runnable() {

						@Override
						public void run() {
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(site, false), name,
									plugin.getVoteSiteServiceSite(site), false);
							plugin.getServer().getPluginManager().callEvent(voteEvent);
						}
					});
				}
			});

		}
	}

}
