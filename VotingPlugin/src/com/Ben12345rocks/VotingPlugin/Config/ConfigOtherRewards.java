package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigOtherRewards.
 */
public class ConfigOtherRewards extends YMLFile {

	/** The instance. */
	static ConfigOtherRewards instance = new ConfigOtherRewards();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigOtherRewards.
	 *
	 * @return single instance of ConfigOtherRewards
	 */
	public static ConfigOtherRewards getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new config other rewards.
	 */
	public ConfigOtherRewards() {
		super(new File(plugin.getDataFolder(), "Rewards.yml"));
	}

	/**
	 * Gets the all sites reward.
	 *
	 * @return the all sites reward
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getAllSitesReward() {
		try {
			return (ArrayList<String>) getData().getList("AllSites");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the cumulative reward enabled.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative reward enabled
	 */
	public boolean getCumulativeRewardEnabled(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".Enabled");
	}

	/**
	 * Gets the cumulative rewards.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCumulativeRewards(int cumulative) {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"Cumulative." + cumulative + ".Rewards");
			if (list != null) {
				return list;
			}

			return new ArrayList<String>();
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the cumulative votes.
	 *
	 * @return the cumulative votes
	 */
	public Set<String> getCumulativeVotes() {
		try {
			Set<String> set = getData().getConfigurationSection("Cumulative")
					.getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the cumulative votes in same day.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative votes in same day
	 */
	public boolean getCumulativeVotesInSameDay(int cumulative) {
		return getData().getBoolean(
				"Cumulative." + cumulative + ".VotesInSameDay");
	}

	public boolean getCumulativeVotesInSameWeek(int cumulative) {
		return getData().getBoolean(
				"Cumulative." + cumulative + ".VotesInSameWeek");
	}

	/**
	 * Gets the first vote rewards.
	 *
	 * @return the first vote rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFirstVoteRewards() {
		try {
			return (ArrayList<String>) getData().getList("FirstVote");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getAnySiteRewards() {
		return (ArrayList<String>) getData().getList("AnySiteRewards",
				new ArrayList<String>());

	}

	/**
	 * Sets the rewards.
	 *
	 * @param rewards
	 *            the new rewards
	 */
	public void setAnySiteRewards(ArrayList<String> rewards) {
		getData().set("AnySiteRewards", rewards);
		saveData();
	}

	/**
	 * Gets the milestone reward enabled.
	 *
	 * @param milestones
	 *            the milestones
	 * @return the milestone reward enabled
	 */
	public boolean getMilestoneRewardEnabled(int milestones) {
		return getData().getBoolean("MileStones." + milestones + ".Enabled");
	}

	/**
	 * Gets the milestone rewards.
	 *
	 * @param milestones
	 *            the milestones
	 * @return the milestone rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getMilestoneRewards(int milestones) {
		try {
			ArrayList<String> list = (ArrayList<String>) getData().getList(
					"MileStones." + milestones + ".Rewards");
			if (list != null) {
				return list;
			}

			return new ArrayList<String>();
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the milestone votes.
	 *
	 * @return the milestone votes
	 */
	public Set<String> getMilestoneVotes() {
		try {
			Set<String> set = getData().getConfigurationSection("MileStones")
					.getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the min votes enabled.
	 *
	 * @return the min votes enabled
	 */
	public boolean getMinVotesEnabled() {
		return getData().getBoolean("MinVotes.Enabled");
	}

	/**
	 * Gets the min votes rewards.
	 *
	 * @return the min votes rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getMinVotesRewards() {
		return (ArrayList<String>) getData().getList("MinVotes.Rewards",
				new ArrayList<String>());
	}

	/**
	 * Gets the min votes votes.
	 *
	 * @return the min votes votes
	 */
	public int getMinVotesVotes() {
		return getData().getInt("MinVotes.Votes");
	}

	/**
	 * Gets the vote party enabled.
	 *
	 * @return the vote party enabled
	 */
	public boolean getVotePartyEnabled() {
		return getData().getBoolean("VoteParty.Enabled");
	}

	public boolean getVotePartyResetEachDay() {
		return getData().getBoolean("VoteParty.ResetEachDay");
	}

	/**
	 * Gets the vote party give all players.
	 *
	 * @return the vote party give all players
	 */
	public boolean getVotePartyGiveAllPlayers() {
		return getData().getBoolean("VoteParty.GiveAllPlayers");
	}

	/**
	 * Gets the vote party rewards.
	 *
	 * @return the vote party rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotePartyRewards() {
		return (ArrayList<String>) getData().getList("VoteParty.Rewards");
	}

	/**
	 * Gets the vote party votes required.
	 *
	 * @return the vote party votes required
	 */
	public int getVotePartyVotesRequired() {
		return getData().getInt("VoteParty.VotesRequired");
	}

	/**
	 * Gets the votes required.
	 *
	 * @return the votes required
	 */
	public int getVotesRequired() {
		return getData().getInt("VotesRequired");
	}

	/**
	 * Sets the cumulative rewards.
	 *
	 * @param cumulative
	 *            the cumulative
	 * @param rewards
	 *            the rewards
	 */
	public void setCumulativeRewards(int cumulative, ArrayList<String> rewards) {
		getData().set("Cumulative." + cumulative + ".Rewards", rewards);
		saveData();
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("Rewards.yml", true);

	}

}
