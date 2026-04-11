package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import com.bencodez.simpleapi.file.YMLFile;
import com.bencodez.simpleapi.file.annotation.AnnotationHandler;
import com.bencodez.simpleapi.file.annotation.ConfigDataBoolean;
import com.bencodez.simpleapi.file.annotation.ConfigDataDouble;
import com.bencodez.simpleapi.file.annotation.ConfigDataInt;
import com.bencodez.simpleapi.file.annotation.ConfigDataKeys;
import com.bencodez.simpleapi.file.annotation.ConfigDataListInt;
import com.bencodez.simpleapi.file.annotation.ConfigDataListString;
import com.bencodez.simpleapi.file.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

/**
 * The SpecialRewardsConfig class manages special rewards configuration.
 */
public class SpecialRewardsConfig extends YMLFile {

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

	@ConfigDataBoolean(path = "VoteParty.ResetExtraVotesWeekly")
	@Getter
	private boolean votePartyResetExtraVotesWeekly = false;

	@ConfigDataString(path = "VoteParty.VoteReminder.Broadcast", secondPath = "VoteParty.VoteReminderBroadcast")
	@Getter
	private String votePartyVoteReminderBroadcast = "%votesrequired% left to go, go vote!";

	@ConfigDataListInt(path = "VoteParty.VoteReminder.AtVotes", secondPath = "VoteParty.VoteReminderAtVotes")
	@Getter
	private ArrayList<Integer> votePartyVoteReminderAtVotes = new ArrayList<>();

	@ConfigDataListString(path = "VoteParty.VoteReminder.Commands", secondPath = "VoteParty.VoteReminderCommands")
	@Getter
	private ArrayList<String> votePartyVoteReminderCommands = new ArrayList<>();

	@ConfigDataListString(path = "VoteParty.GlobalRandomCommand")
	@Getter
	private ArrayList<String> votePartyGlobalRandomCommand = new ArrayList<>();

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

	@ConfigDataKeys(path = "DailyAwards")
	@Getter
	private Set<String> dailyPossibleRewardPlaces = new HashSet<>();

	@ConfigDataString(path = "VoteParty.Broadcast")
	@Getter
	private String votePartyBroadcast = "";

	@ConfigDataListString(path = "VoteParty.GlobalCommands", secondPath = "VoteParty.Commands")
	@Getter
	private ArrayList<String> votePartyGlobalCommands = new ArrayList<>();

	@ConfigDataBoolean(path = "VoteParty.CountFakeVotes")
	@Getter
	private boolean votePartyCountFakeVotes = true;

	@ConfigDataBoolean(path = "VoteParty.CountOfflineVotes")
	@Getter
	private boolean votePartyCountOfflineVotes = true;

	@ConfigDataBoolean(path = "VoteParty.Enabled")
	@Getter
	private boolean votePartyEnabled = false;

	@ConfigDataBoolean(path = "VoteParty.GiveAllPlayers")
	@Getter
	private boolean votePartyGiveAllPlayers = false;

	@Deprecated
	@ConfigDataInt(path = "VoteParty.IncreaseVotesRquired")
	@Getter
	private int votePartyIncreaseVotesRquired = 0;

	@ConfigDataInt(path = "VoteParty.IncreaseVotesRequired")
	@Getter
	private int votePartyIncreaseVotesRequired = 0;

	@ConfigDataBoolean(path = "VoteParty.OnlyOncePerDay")
	@Getter
	private boolean votePartyOnlyOncePerDay = false;

	@ConfigDataBoolean(path = "VoteParty.OnlyOncePerWeek")
	@Getter
	private boolean votePartyOnlyOncePerWeek = false;

	@ConfigDataBoolean(path = "VoteParty.ResetEachDay")
	@Getter
	private boolean votePartyResetEachDay = false;

	@ConfigDataBoolean(path = "VoteParty.ResetMonthly")
	@Getter
	private boolean votePartyResetMonthly = false;

	@ConfigDataBoolean(path = "VoteParty.ResetWeekly")
	@Getter
	private boolean votePartyResetWeekly = false;

	@ConfigDataInt(path = "VoteParty.UserVotesRequired")
	@Getter
	private int votePartyUserVotesRequired = 0;
	@ConfigDataInt(path = "VoteParty.VotesRequired")
	@Getter
	private int votePartyVotesRequired = 0;

	@ConfigDataKeys(path = "WeeklyAwards")
	@Getter
	private Set<String> weeklyPossibleRewardPlaces = new HashSet<>();

	@ConfigDataBoolean(path = "NameMCLikeReward.Enabled")
	@Getter
	private boolean nameMCLikeRewardEnabled = false;

	@ConfigDataString(path = "NameMCLikeReward.Url")
	@Getter
	private String nameMCLikeRewardUrl = "";

	@ConfigDataInt(path = "NameMCLikeReward.CheckIntervalMinutes")
	@Getter
	private int nameMCLikeRewardCheckIntervalMinutes = 10;

	@Getter
	private final String nameMCLikeRewardPath = "NameMCLikeReward.Rewards";

	/**
	 * Constructs a new SpecialRewardsConfig.
	 *
	 * @param plugin the main plugin instance
	 */
	public SpecialRewardsConfig(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "SpecialRewards.yml"));
		setIgnoreCase(plugin.getConfigFile().isCaseInsensitiveYMLFiles());
		this.plugin = plugin;
	}

	/**
	 * Gets the rewards path for a daily award.
	 *
	 * @param path the award path
	 * @return the daily award rewards path
	 */
	public String getDailyAwardRewardsPath(String path) {
		return "DailyAwards." + path + ".Rewards";
	}

	/**
	 * Gets the rewards path for a monthly award.
	 *
	 * @param path the award path
	 * @return the monthly award rewards path
	 */
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

	/**
	 * Gets the increase votes required with the old typo field.
	 *
	 * @return the increase votes required
	 * @deprecated Use getVotePartyIncreaseVotesRequired instead
	 */
	public int getVotePartyIncreaseVotesRequiredWithTypo() {
		int increase = getVotePartyIncreaseVotesRequired();
		if (increase > 0) {
			return increase;
		}
		return getVotePartyIncreaseVotesRquired();
	}

	/**
	 * Checks if a vote streak reward is enabled.
	 *
	 * @param type the streak type
	 * @param s    the streak value
	 * @return true if enabled
	 */
	public boolean getVoteStreakRewardEnabled(String type, String s) {
		return getData().getBoolean("VoteStreak." + type + "." + s + ".Enabled");
	}

	/**
	 * Gets the rewards path for a vote streak.
	 *
	 * @param type   the streak type
	 * @param string the streak value
	 * @return the vote streak rewards path
	 */
	public String getVoteStreakRewardsPath(String type, String string) {
		return "VoteStreak." + type + "." + string + ".Rewards";
	}

	/**
	 * Gets all vote streak values for a type.
	 *
	 * @param type the streak type
	 * @return the set of vote streak values
	 */
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

	/**
	 * Gets the rewards path for a weekly award.
	 *
	 * @param path the award path
	 * @return the weekly award rewards path
	 */
	public String getWeeklyAwardRewardsPath(String path) {
		return "WeeklyAwards." + path + ".Rewards";
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("SpecialRewards.yml", true);
	}

	/**
	 * Removes a vote milestone.
	 *
	 * @param votes the milestone votes value
	 */
	public void removeVoteMilestone(String votes) {
		getData().set("VoteMilestones." + votes, null);
		saveData();
	}

	/**
	 * Sets a vote milestone with default values.
	 *
	 * @param intValue the milestone votes value
	 */
	public void setVoteMilestone(int intValue) {
		getData().set("VoteMilestones." + intValue + ".Enabled", true);
		getData().set("VoteMilestones." + intValue + ".Rewards.Messages.Player",
				"&aYou got %milestone% milestone votes!");
		saveData();
	}
}
