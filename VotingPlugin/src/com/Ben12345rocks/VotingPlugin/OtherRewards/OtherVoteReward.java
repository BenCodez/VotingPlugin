package com.Ben12345rocks.VotingPlugin.OtherRewards;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class OtherVoteReward.
 */
public class OtherVoteReward {

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
	 * Check number of votes.
	 *
	 * @param user
	 *            the user
	 * @return true, if successful
	 */
	public boolean checkNumberOfVotes(User user) {
		if (ConfigOtherRewards.getInstance().getNumberOfVotes().size() != 0) {
			int votesRequired = ConfigOtherRewards.getInstance()
					.getVotesRequired();
			if (votesRequired != 0) {
				if (ConfigOtherRewards.getInstance()
						.getNumberOfVotesVotesInSameDay()) {
					int userVotesTotal = user.getTotalVotesToday();
					if ((userVotesTotal % votesRequired) == 0) {
						return true;
					}
				} else {

					int userVotesTotal = user.getTotalVotes();
					if ((userVotesTotal % votesRequired) == 0) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Give all sites rewards.
	 *
	 * @param user
	 *            the user
	 */
	public void giveAllSitesRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getAllSitesReward()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

	/**
	 * Give first vote rewards.
	 *
	 * @param user
	 *            the user
	 */
	public void giveFirstVoteRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getFirstVoteRewards()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

	/**
	 * Give number of votes rewards.
	 *
	 * @param user
	 *            the user
	 */
	public void giveNumberOfVotesRewards(User user) {
		for (String reward : ConfigOtherRewards.getInstance()
				.getNumberOfVotes()) {
			if (reward != "") {
				ConfigRewards.getInstance().getReward(reward).giveReward(user);
			}
		}
	}

}
