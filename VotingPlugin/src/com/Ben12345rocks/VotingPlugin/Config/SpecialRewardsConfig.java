package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.Ben12345rocks.AdvancedCore.Util.Annotation.AnnotationHandler;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataBoolean;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataDouble;
import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

import lombok.Getter;

public class SpecialRewardsConfig extends YMLFile {
	/** The instance. */
	static SpecialRewardsConfig instance = new SpecialRewardsConfig();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static SpecialRewardsConfig getInstance() {
		return instance;
	}

	public SpecialRewardsConfig() {
		super(new File(Main.plugin.getDataFolder(), "SpecialRewards.yml"));
	}

	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	public String getWeeklyAwardRewardsPath(String path) {
		return "WeeklyAwards." + path + ".Rewards";
	}

	/**
	 * Gets the weekly possible reward places.
	 *
	 * @return the weekly possible reward places
	 */
	public Set<String> getWeeklyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("WeeklyAwards").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public String getMonthlyAwardRewardsPath(String path) {
		return "MonthlyAwards." + path + ".Rewards";
	}

	/**
	 * Gets the monthly possible reward places.
	 *
	 * @return the monthly possible reward places
	 */
	public Set<String> getMonthlyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("MonthlyAwards").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public String getDailyAwardRewardsPath(String path) {
		return "DailyAwards." + path + ".Rewards";
	}

	/**
	 * Gets the daily possible reward places.
	 *
	 * @return the daily possible reward places
	 */
	public Set<String> getDailyPossibleRewardPlaces() {
		try {
			return getData().getConfigurationSection("DailyAwards").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();

		}
	}

	@ConfigDataBoolean(path = "EnableWeeklyAwards")
	@Getter
	private boolean enableWeeklyAwards = false;

	@ConfigDataBoolean(path = "EnableDailyRewards")
	@Getter
	private boolean enableDailyRewards = false;

	@ConfigDataBoolean(path = "EnableMonthlyAwards")
	@Getter
	private boolean enableMonthlyAwards = false;

	@Override
	public void onFileCreation() {
		plugin.saveResource("SpecialRewards.yml", true);
	}

	public boolean getCumulativeRewardEnabled(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".Enabled");
	}

	public String getCumulativeRewardsPath(int cumulative) {
		return "Cumulative." + cumulative + ".Rewards";
	}

	public String getVotePartyBroadcast() {
		return getData().getString("VoteParty.Broadcast", "");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getVotePartyCommands() {
		return (ArrayList<String>) getData().getList("VoteParty.Commands", new ArrayList<String>());
	}

	public boolean getVotePartyCountFakeVotes() {
		return getData().getBoolean("VoteParty.CountFakeVotes", true);
	}

	/**
	 * Gets the vote party enabled.
	 *
	 * @return the vote party enabled
	 */
	public boolean getVotePartyEnabled() {
		return getData().getBoolean("VoteParty.Enabled");
	}

	/**
	 * Gets the vote party give all players.
	 *
	 * @return the vote party give all players
	 */
	public boolean getVotePartyGiveAllPlayers() {
		return getData().getBoolean("VoteParty.GiveAllPlayers");
	}

	public int getVotePartyIncreaseVotesRquired() {
		return getData().getInt("VoteParty.IncreaseVotesRquired", 0);
	}

	public boolean getVotePartyResetEachDay() {
		return getData().getBoolean("VoteParty.ResetEachDay");
	}

	public boolean getVotePartyResetMontly() {
		return getData().getBoolean("VoteParty.ResetMonthly");
	}

	public int getVotePartyUserVotesRequired() {
		return getData().getInt("VoteParty.UserVotesRequired");
	}

	/**
	 * Gets the vote party votes required.
	 *
	 * @return the vote party votes required
	 */
	public int getVotePartyVotesRequired() {
		return getData().getInt("VoteParty.VotesRequired");
	}

	@Getter
	private String anySiteRewardsPath = "AnySiteRewards";

	@ConfigDataDouble(path = "VoteStreak.Requirement.Day")
	@Getter
	private double voteStreakRequirementDay = 50;

	@ConfigDataDouble(path = "VoteStreak.Requirement.Week")
	@Getter
	private double voteStreakRequirementWeek = 50;

	@ConfigDataDouble(path = "VoteStreak.Requirement.Month")
	@Getter
	private double voteStreakRequirementMonth = 50;

	@ConfigDataBoolean(path = "VoteStreak.Requirement.UsePercentage")
	@Getter
	private boolean voteStreakRequirementUsePercentage = false;

	public boolean getVoteStreakRewardEnabled(String type, String s) {
		return getData().getBoolean("VoteStreak." + type + "." + s + ".Enabled");
	}

	public String getVoteStreakRewardsPath(String type, String string) {
		return "VoteStreak." + type + "." + string + ".Rewards";
	}

	public boolean getResetMilestonesMonthly() {
		return getData().getBoolean("ResetMilestonesMonthly");
	}

	public Set<String> getVoteStreakVotes(String type) {
		try {
			Set<String> set = getData().getConfigurationSection("VoteStreak." + type).getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the cumulative votes.
	 *
	 * @return the cumulative votes
	 */
	public Set<String> getCumulativeVotes() {
		try {
			Set<String> set = getData().getConfigurationSection("Cumulative").getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
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
	public String getMilestoneRewardsPath(int milestones) {
		return "MileStones." + milestones + ".Rewards";
	}

	@ConfigDataBoolean(path = "VoteParty.ResetExtraVotesMonthly")
	@Getter
	private boolean votePartyResetExtraVotesMonthly = false;

	/**
	 * Gets the milestone votes.
	 *
	 * @return the milestone votes
	 */
	public Set<String> getMilestoneVotes() {
		try {
			Set<String> set = getData().getConfigurationSection("MileStones").getKeys(false);
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
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameDay");
	}

	public boolean getCumulativeVotesInSameWeek(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameWeek");
	}

	public boolean getCumulativeVotesInSameMonth(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameMonth");
	}

	@Getter
	private String allSitesRewardPath = "AllSites";

	@Getter
	private String votePartyRewardsPath = "VoteParty.Rewards";

	@Getter
	private String firstVoteRewardsPath = "FirstVote";
}
