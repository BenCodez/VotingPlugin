package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.bencodez.advancedcore.api.yml.YMLFile;
import com.bencodez.advancedcore.api.yml.annotation.AnnotationHandler;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataBoolean;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataDouble;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;

import lombok.Getter;

public class SpecialRewardsConfig extends YMLFile {

	@Getter
	private String allSitesRewardPath = "AllSites";

	@Getter
	private String anySiteRewardsPath = "AnySiteRewards";

	@ConfigDataBoolean(path = "EnableDailyRewards")
	@Getter
	private boolean enableDailyRewards = false;

	@ConfigDataBoolean(path = "EnableMonthlyAwards")
	@Getter
	private boolean enableMonthlyAwards = true;

	@ConfigDataBoolean(path = "EnableWeeklyAwards")
	@Getter
	private boolean enableWeeklyAwards = false;

	@ConfigDataBoolean(path = "OnlyOneCumulative")
	@Getter
	private boolean onlyOneCumulative = false;

	@Getter
	private String firstVoteRewardsPath = "FirstVote";

	@Getter
	private String firstVoteTodayRewardsPath = "FirstVoteToday";

	/** The plugin. */
	private VotingPluginMain plugin;

	@ConfigDataBoolean(path = "VoteParty.GiveOnlinePlayersOnly")
	@Getter
	private boolean votePartyGiveOnlinePlayersOnly = false;

	@ConfigDataBoolean(path = "VoteParty.ResetCount")
	@Getter
	private boolean votePartyResetCount = true;

	@ConfigDataBoolean(path = "VoteParty.ResetExtraVotesMonthly")
	@Getter
	private boolean votePartyResetExtraVotesMonthly = false;

	@Getter
	private String votePartyRewardsPath = "VoteParty.Rewards";

	@ConfigDataDouble(path = "VoteStreak.Requirement.Day")
	@Getter
	private double voteStreakRequirementDay = 50;

	@ConfigDataDouble(path = "VoteStreak.Requirement.Month")
	@Getter
	private double voteStreakRequirementMonth = 50;

	@ConfigDataBoolean(path = "VoteStreak.Requirement.UsePercentage")
	@Getter
	private boolean voteStreakRequirementUsePercentage = false;

	@ConfigDataDouble(path = "VoteStreak.Requirement.Week")
	@Getter
	private double voteStreakRequirementWeek = 50;

	public SpecialRewardsConfig(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "SpecialRewards.yml"));
		this.plugin = plugin;
	}

	public boolean getCumulativeRewardEnabled(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".Enabled");
	}

	public String getCumulativeRewardsPath(int cumulative) {
		return "Cumulative." + cumulative + ".Rewards";
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
			return new HashSet<>();
		} catch (Exception ex) {
			return new HashSet<>();
		}
	}

	/**
	 * Gets the cumulative votes in same day.
	 *
	 * @param cumulative the cumulative
	 * @return the cumulative votes in same day
	 */
	@Deprecated
	public boolean getCumulativeVotesInSameDay(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameDay");
	}

	@Deprecated
	public boolean getCumulativeVotesInSameMonth(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameMonth");
	}

	@Deprecated
	public boolean getCumulativeVotesInSameWeek(int cumulative) {
		return getData().getBoolean("Cumulative." + cumulative + ".VotesInSameWeek");
	}

	public String getCumulativeVotesTotal(int cumulative) {
		String str = getData().getString("Cumulative." + cumulative + ".TotalToUse", "");
		if (str.isEmpty()) {
			if (getCumulativeVotesInSameMonth(cumulative)) {
				return TopVoter.Monthly.toString();
			} else if (getCumulativeVotesInSameWeek(cumulative)) {
				return TopVoter.Weekly.toString();
			} else if (getCumulativeVotesInSameDay(cumulative)) {
				return TopVoter.Daily.toString();
			}
		}
		return str;
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
			return new HashSet<>();

		}
	}

	/**
	 * Gets the milestone reward enabled.
	 *
	 * @param milestones the milestones
	 * @return the milestone reward enabled
	 */
	public boolean getMilestoneRewardEnabled(int milestones) {
		return getData().getBoolean("MileStones." + milestones + ".Enabled");
	}

	/**
	 * Gets the milestone rewards.
	 *
	 * @param milestones the milestones
	 * @return the milestone rewards
	 */
	public String getMilestoneRewardsPath(int milestones) {
		return "MileStones." + milestones + ".Rewards";
	}

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
			return new HashSet<>();
		} catch (Exception ex) {
			return new HashSet<>();
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
			return new HashSet<>();
		}
	}

	public boolean getResetMilestonesMonthly() {
		return getData().getBoolean("ResetMilestonesMonthly");
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

	public boolean getVotePartyCountOfflineVotes() {
		return getData().getBoolean("VoteParty.CountOfflineVotes", true);
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

	public boolean getVotePartyOnlyOncePerDay() {
		return getData().getBoolean("VoteParty.OnlyOncePerDay");
	}

	public boolean getVotePartyResetEachDay() {
		return getData().getBoolean("VoteParty.ResetEachDay");
	}

	public boolean getVotePartyResetMontly() {
		return getData().getBoolean("VoteParty.ResetMonthly");
	}

	public boolean getVotePartyResetWeekly() {
		return getData().getBoolean("VoteParty.ResetWeekly");
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

	public boolean getVoteStreakRewardEnabled(String type, String s) {
		return getData().getBoolean("VoteStreak." + type + "." + s + ".Enabled");
	}

	public String getVoteStreakRewardsPath(String type, String string) {
		return "VoteStreak." + type + "." + string + ".Rewards";
	}

	public Set<String> getVoteStreakVotes(String type) {
		try {
			Set<String> set = getData().getConfigurationSection("VoteStreak." + type).getKeys(false);
			if (set != null) {
				return set;
			}
			return new HashSet<>();
		} catch (Exception ex) {
			return new HashSet<>();
		}
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
			return new HashSet<>();
		}
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("SpecialRewards.yml", true);
	}

	public void removeCumulative(String votes) {
		getData().set("Cumulative." + votes, null);
		saveData();
	}

	public void removeMilestone(String votes) {
		getData().set("MileStones." + votes, null);
		saveData();
	}

	public void setCumulative(int intValue) {
		getData().set("Cumulative." + intValue + ".Enabled", true);
		getData().set("Cumulative." + intValue + ".TotalToUse", "AllTime");
		getData().set("Cumulative." + intValue + ".Rewards.Messages.Player",
				"&aYou got %cumulative% cumulative votes!");
		saveData();
	}

	public void setMilestone(int intValue) {
		getData().set("Milestones." + intValue + ".Enabled", true);
		getData().set("Milestones." + intValue + ".Rewards.Messages.Player", "&aYou got %milestone% milestone votes!");
		saveData();
	}
}
