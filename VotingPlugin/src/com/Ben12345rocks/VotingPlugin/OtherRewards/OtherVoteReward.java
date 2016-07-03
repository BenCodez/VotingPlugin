package com.Ben12345rocks.VotingPlugin.OtherRewards;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class OtherVoteReward {

	static ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static OtherVoteReward instance = new OtherVoteReward();

	static Main plugin = Main.plugin;

	public static OtherVoteReward getInstance() {
		return instance;
	}

	private OtherVoteReward() {
	}

	public OtherVoteReward(Main plugin) {
		OtherVoteReward.plugin = plugin;
	}

	public boolean checkAllSites(User user) {
		return user.canVoteAll();

	}

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

	public boolean checkNumberOfVotes(User user) {
		if (ConfigOtherRewards.getInstance().getNumberOfVotes().size() != 0) {
			int votesRequired = ConfigOtherRewards.getInstance()
					.getVotesRequired();

			if (votesRequired != 0) {
				int userVotesTotal = user.getTotalVotes();
				if ((userVotesTotal % votesRequired) == 0) {
					return true;
				}
			}
		}
		return false;
	}

	public void giveAllSitesRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getAllSitesReward()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

	public void giveFirstVoteRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getFirstVoteRewards()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

	public void giveNumberOfVotesRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getNumberOfVotes()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

}
