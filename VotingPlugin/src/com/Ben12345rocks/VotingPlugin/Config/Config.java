package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.Ben12345rocks.AdvancedCore.Util.Annotation.AnnotationHandler;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataBoolean;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataDouble;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataInt;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataString;
import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class Config.
 */
public class Config extends YMLFile {

	/** The instance. */
	static Config instance = new Config();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static Config getInstance() {
		return instance;
	}

	@ConfigDataString(
			path = "Format.Commands.Vote.ToggleBroadcasts.Enabled", defaultValue = "&cYou will now see vote broadcasts"
	)
	@Getter
	private String formatCommandsVoteToggleBroadcastEnabled;

	@ConfigDataString(
			path = "Format.Commands.Vote.ToggleBroadcasts.Disabled",
			defaultValue = "&cYou will no longer see vote broadcasts"
	)
	@Getter
	private String formatCommandsVoteToggleBroadcastDisabled;

	@ConfigDataBoolean(path = "Commands.VoteRewardFromVoteURL")
	@Getter
	private boolean commandsVoteRewardFromVoteURL = false;

	@ConfigDataString(path = "Format.Commands.Vote.Last.TimeFormat", defaultValue = "%amount% %TimeType%")
	@Getter
	private String formatCommandsVoteLastTimeFormat;

	@ConfigDataString(path = "Format.Commands.Vote.Last.Line", defaultValue = "&3%SiteName%: &6%timeSince% ago")
	@Getter
	private String formatCommandsVoteLastLine;

	@ConfigDataString(path = "Format.Commands.Vote.Last.NeverVoted", defaultValue = "Never voted")
	@Getter
	private String formatCommandsVoteLastNeverVoted;

	@ConfigDataString(path = "Format.Commands.Vote.Last.LastVoted", defaultValue = "%times% ago")
	@Getter
	private String formatCommandsVoteLastLastVoted;

	@ConfigDataString(path = "Format.TimeFormats.Days", defaultValue = "Days")
	@Getter
	private String formatTimeFormatsDays;

	@ConfigDataString(path = "Format.TimeFormats.Day", defaultValue = "Day")
	@Getter
	private String formatTimeFormatsDay;

	@ConfigDataString(path = "Format.TimeFormats.Hours", defaultValue = "Hours")
	@Getter
	private String formatTimeFormatsHours;

	@ConfigDataString(path = "Format.TimeFormats.Hour", defaultValue = "Hour")
	@Getter
	private String formatTimeFormatsHour;

	@ConfigDataString(path = "Format.TimeFormats.Minutes", defaultValue = "Minutes")
	@Getter
	private String formatTimeFormatsMinutes;

	@ConfigDataString(path = "Format.TimeFormats.Minute", defaultValue = "Minute")
	@Getter
	private String formatTimeFormatsMinute;

	@ConfigDataString(path = "Format.TimeFormats.Seconds", defaultValue = "Seconds")
	@Getter
	private String formatTimeFormatsSeconds;

	@ConfigDataString(path = "Format.TimeFormats.Second", defaultValue = "Second")
	@Getter
	private String formatTimeFormatsSecond;

	@ConfigDataBoolean(path = "Commands.DisableVoteRewardGUIs")
	@Getter
	private boolean commandsDisableVoteRewardGUIs;

	@ConfigDataBoolean(path = "AlwaysUpdate")
	@Getter
	private boolean alwaysUpdate = false;

	@ConfigDataBoolean(path = "UpdateWithPlayersOnlineOnly")
	@Getter
	private boolean updateWithPlayersOnlineOnly = false;

	@ConfigDataBoolean(path = "AllowUnjoined")
	@Getter
	private boolean allowUnjoined = false;

	@ConfigDataBoolean(path = "AddTotals")
	@Getter
	private boolean addTotals = true;

	@Getter
	private String allSitesRewardPath = "AllSites";

	@ConfigDataBoolean(path = "AlternateUUIDLookup")
	@Getter
	private boolean alternateUUIDLookup = false;

	@Getter
	private String anySiteRewardsPath = "AnySiteRewards";

	@ConfigDataBoolean(path = "AutoCreateVoteSites")
	@Getter
	private boolean autoCreateVoteSites = true;

	@ConfigDataBoolean(path = "UseBungeecoord")
	@Getter
	private boolean useBungeeCoord = false;

	@ConfigDataBoolean(path = "BungeeBroadcast")
	@Getter
	private boolean bungeeBroadcast = false;

	@ConfigDataBoolean(path = "GUI.VoteURL.AllUrlsButton.RequireAllSitesVoted")
	@Getter
	private boolean guiVoteURLAllUrlsButtonrequireAllSitesVoted = true;

	@ConfigDataBoolean(path = "Format.BroadcastVote")
	@Getter
	private boolean broadcastVotesEnabled = true;

	@ConfigDataBoolean(path = "clearCacheOnUpdate")
	@Getter
	private boolean clearCacheOnUpdate = false;

	@ConfigDataBoolean(path = "ClearCacheOnVote")
	@Getter
	private boolean clearCacheOnVote = false;

	@ConfigDataBoolean(path = "Commands.UseGUI.Best")
	@Getter
	private boolean commandsUseGUIBest = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Last")
	@Getter
	private boolean commandsUseGUILast = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Next")
	@Getter
	private boolean commandsUseGUINext = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Streak")
	@Getter
	private boolean commandsUseGUIStreak = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Today")
	@Getter
	private boolean commandsUseGUIToday = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.TopVoter")
	@Getter
	private boolean commandsUseGUITopVoter = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Total")
	@Getter
	private boolean commandsUseGUITotal = true;

	@ConfigDataBoolean(path = "Commands.UseGUI.Vote")
	@Getter
	private boolean commandsUseGUIVote = true;

	@ConfigDataBoolean(path = "CountFakeVotes")
	@Getter
	private boolean countFakeVotes = true;

	@ConfigDataBoolean(path = "GUI.VoteRewardBackButton")
	@Getter
	private boolean voteRewardBackButton = false;

	@ConfigDataInt(path = "ConvertDelay", defaultValue = 30000)
	@Getter
	private int convertDelay;

	@ConfigDataBoolean(path = "EnableDailyRewards")
	@Getter
	private boolean enableDailyRewards = false;

	@ConfigDataBoolean(path = "DisableUpdateChecking")
	@Getter
	private boolean disableUpdateChecking = false;

	@ConfigDataInt(path = "DelayBetweenUpdates")
	@Getter
	private int delayBetweenUpdates = 3;

	@ConfigDataBoolean(path = "DisableNoServiceSiteMessage")
	@Getter
	private boolean disableNoServiceSiteMessage = false;

	@ConfigDataBoolean(path = "DisableAdvancedTab")
	@Getter
	private boolean disableAdvancedTab = false;

	@Getter
	private String firstVoteRewardsPath = "FirstVote";

	@ConfigDataBoolean(path = "LogVotesToFile")
	@Getter
	private boolean logVotesToFile = false;

	@ConfigDataBoolean(path = "EnableMonthlyAwards")
	@Getter
	private boolean enableMonthlyAwards = false;

	@Getter
	private String votePartyRewardsPath = "VoteParty.Rewards";

	@ConfigDataBoolean(path = "EnableWeeklyAwards")
	@Getter
	private boolean enableWeeklyAwards = false;

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

	@ConfigDataBoolean(path = "LoadCommandAliases")
	@Getter
	private boolean loadCommandAliases = true;

	@ConfigDataBoolean(path = "GUI.VoteTop.UseSkull")
	@Getter
	private boolean guiVoteTopUseSkull = true;

	@ConfigDataString(path = "GUI.VoteTop.PlayerItem.Material")
	@Getter
	private String guiVoteTopPlayerItemMaterial = "PAPER";

	@ConfigDataBoolean(path = "AllowVotePointTransfers")
	@Getter
	private boolean allowVotePointTransfers = false;

	@ConfigDataBoolean(path = "LimitMonthlyVotes")
	@Getter
	private boolean limitMonthlyVotes = false;

	@ConfigDataBoolean(path = "AlwaysCloseInventory")
	@Getter
	private boolean alwaysCloseInventory = false;

	@ConfigDataBoolean(path = "VoteShopHideLimitedReached")
	@Getter
	private boolean voteShopHideLimitedReached = true;

	@ConfigDataString(path = "VoteShopLimitReached")
	@Getter
	private String voteShopLimitReached = "&aYou reached your limit";

	@Getter
	private int formatPageSize = 10;

	@ConfigDataString(path = "Format.Commands.Vote.Today.Line")
	@Getter
	private String formatCommandsVoteTodayLine = "&6%player% : %VoteSite% : %Time%";

	@ConfigDataBoolean(path = "OverrideVersionDisable")
	@Getter
	private boolean overrideVersionDisable = false;

	@ConfigDataBoolean(path = "LastMonthGUI")
	@Getter
	private boolean lastMonthGUI = false;

	@ConfigDataBoolean(path = "AllowUnJoinedCheckServer")
	@Getter
	private boolean allowUnJoinedCheckServer = true;

	/**
	 * Instantiates a new config.
	 */
	public Config() {
		super(new File(Main.plugin.getDataFolder(), "Config.yml"));
	}

	public ConfigurationSection getBackButton() {
		return getData().getConfigurationSection("BackButton");
	}

	/**
	 * Gets the black list.
	 *
	 * @return the black list
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getBlackList() {
		return (ArrayList<String>) getData().getList("BlackList", new ArrayList<String>());
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
	 * Gets the cumulative rewards path
	 *
	 * @param cumulative
	 *            the cumulative
	 * @return the cumulative rewards
	 */
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

	/**
	 * Gets the broad cast msg.
	 *
	 * @return the broad cast msg
	 */
	public String getFormatBroadCastMsg() {
		return getData().getString("Format.BroadcastMsg",
				"&6[&4Broadcast&6] &2Thanks &c%player% &2for voting on %SiteName%");

	}

	/**
	 * Gets the broadcast when online.
	 *
	 * @return the broadcast when online
	 */
	public boolean getFormatBroadcastWhenOnline() {
		return getData().getBoolean("Format.BroadcastWhenOnline");
	}

	/**
	 * Gets the commands vote auto input sites.
	 *
	 * @return the commands vote auto input sites
	 */
	public boolean getFormatCommandsVoteAutoInputSites() {
		return getData().getBoolean("Format.Commands.Vote.AutoInputSites");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteBestLines() {
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Best.Lines", new ArrayList<String>());
	}

	public String getFormatCommandsVoteBestTitle() {
		return getData().getString("Format.Commands.Vote.Best.Title", "&3&l%player% Best Votes");
	}

	/**
	 * Gets the commands vote help line.
	 *
	 * @return the commands vote help line
	 */
	public String getFormatCommandsVoteHelpLine() {
		return getData().getString("Format.Commands.Vote.Help.Line", "&3&l%Command% - &3%HelpMessage%");
	}

	/**
	 * Gets the commands vote help require permission.
	 *
	 * @return the commands vote help require permission
	 */
	public boolean getFormatCommandsVoteHelpRequirePermission() {
		return getData().getBoolean("Format.Commands.Vote.Help.RequirePermission");
	}

	/**
	 * Gets the commands vote help title.
	 *
	 * @return the commands vote help title
	 */
	public String getFormatCommandsVoteHelpTitle() {
		return getData().getString("Format.Commands.Vote.Help.Title", "Voting Player Help");

	}

	/**
	 * Gets the commands vote last title.
	 *
	 * @return the commands vote last title
	 */
	public String getFormatCommandsVoteLastTitle() {
		return getData().getString("Format.Commands.Vote.Last.Title", "&3&l%player% Last Vote Times:");

	}

	/**
	 * Gets the commands vote next info can vote.
	 *
	 * @return the commands vote next info can vote
	 */
	public String getFormatCommandsVoteNextInfoCanVote() {
		return getData().getString("Format.Commands.Vote.Next.Info.CanVote", "Go Vote!");
	}

	/**
	 * Gets the commands vote next info error.
	 *
	 * @return the commands vote next info error
	 */
	public String getFormatCommandsVoteNextInfoError() {
		return getData().getString("Format.Commands.Vote.Next.Info.Error", "");
	}

	/**
	 * Gets the commands vote next info time.
	 *
	 * @return the commands vote next info time
	 */
	public String getFormatCommandsVoteNextInfoTime() {
		return getData().getString("Format.Commands.Vote.Next.Info.TimeUntilVote",
				"&cCould not caculate time until next vote!");
	}

	public String getFormatCommandsVoteNextInfoVoteDelayDaily() {
		return getData().getString("Format.Commands.Vote.Next.Info.VoteDelayDaily",
				"%hours% Hours and %minutes% Minutes");
	}

	/**
	 * Gets the commands vote next layout.
	 *
	 * @return the commands vote next layout
	 */
	public String getFormatCommandsVoteNextLayout() {
		return getData().getString("Format.Commands.Vote.Next.Layout", "&3%SiteName%: &6%info%");
	}

	/**
	 * Gets the commands vote next title.
	 *
	 * @return the commands vote next title
	 */
	public String getFormatCommandsVoteNextTitle() {
		return getData().getString("Format.Commands.Vote.Next.Title", "&3&l%player% Next Votes:");

	}

	/**
	 * Gets the commands vote party.
	 *
	 * @return the commands vote party
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteParty() {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Party", msg);

	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteStreakLines() {
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Streak.Lines", new ArrayList<String>());
	}

	public String getFormatCommandsVoteStreakTitle() {
		return getData().getString("Format.Commands.Vote.Streak.Title", "&3&l%player% Vote Streak");
	}

	/**
	 * Gets the commands vote text.
	 *
	 * @return the commands vote text
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteText() {
		ArrayList<String> str = new ArrayList<String>();
		str.add("&4&lVote for our server!");
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Text", str);
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteTotal() {
		ArrayList<String> list = (ArrayList<String>) getData().getList("Format.Commands.Vote.Total",
				new ArrayList<String>());
		if (list.isEmpty()) {
			list.add("&3&l%player% Total Votes:");
			list.add("&3&lDaily Total: &6&l%DailyTotal%");
			list.add("&3&lWeekly Total: &6&l%WeeklyTotal%");
			list.add("&3&lMonthly Total: &6&l%MonthlyTotal%");
			list.add("&3&lAllTime Total: &6&l%AllTimeTotal%");
		}
		return list;
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteTotalAll() {

		ArrayList<String> list = (ArrayList<String>) getData().getList("Format.Commands.Vote.TotalAll",
				new ArrayList<String>());
		if (list.isEmpty()) {
			list.add("&3&lServer Total Votes:");
			list.add("&3&lDaily Total: &6&l%DailyTotal%");
			list.add("&3&lWeekly Total: &6&l%WeeklyTotal%");
			list.add("&3&lMonthly Total: &6&l%MonthlyTotal%");
			list.add("&3&lAllTime Total: &6&l%AllTimeTotal%");
		}
		return list;
	}

	/**
	 * Gets the commands vote URLS.
	 *
	 * @return the commands vote URLS
	 */
	public String getFormatCommandsVoteURLS() {
		return getData().getString("Format.Commands.Vote.Sites", "&4%num%: &c&l%SiteName% - &c%url%");
	}

	/**
	 * Gets the command vote points.
	 *
	 * @return the command vote points
	 */
	public String getFormatCommandVotePoints() {
		return getData().getString("Format.Commands.Vote.Points", "&a%Player% currently has &a&l%Points%&a Points!");

	}

	/**
	 * Gets the command vote top line.
	 *
	 * @return the command vote top line
	 */
	public String getFormatCommandVoteTopLine() {
		return getData().getString("Format.Commands.Vote.Top.Line", "&c%num%: &6%player%, %votes%");
	}

	/**
	 * Gets the command vote top title.
	 *
	 * @return the command vote top title
	 */
	public String getFormatCommandVoteTopTitle() {
		return getData().getString("Format.Commands.Vote.Top.Title", "&3Top %Top% Voters %page%/%maxpages%");

	}

	/**
	 * Gets the format help line.
	 *
	 * @return the format help line
	 */
	public String getFormatHelpLine() {
		return getData().getString("Format.HelpLine", "&3&l%Command% - &3%HelpMessage%");
	}

	public String getFormatNextPage() {
		return getData().getString("Format.NextPage", "&aNext Page");
	}

	/**
	 * Gets the format no perms.
	 *
	 * @return the format no perms
	 */
	public String getFormatNoPerms() {
		return getData().getString("Format.NoPerms", "&cYou do not have enough permission!");
	}

	public String getFormatNotNumber() {
		return getData().getString("Format.NotNumber", "&cError on &6%arg%&c, number expected!");
	}

	public String getFormatPrevPage() {
		return getData().getString("Format.PrevPage", "&aPrevious Page");
	}

	public String getFormatShopFailedMsg() {
		String msg = getData().getString("Format.ShopFailed", "&cYou do not have %Points% points to purhcase this!");

		return msg;

	}

	public String getFormatShopPurchaseMsg() {
		return getData().getString("Format.ShopPurchase", "&aYou bought the %Identifier% for %Points% Points!");

	}

	/**
	 * Gets the sign top voter sign line 1.
	 *
	 * @return the sign top voter sign line 1
	 */
	public String getFormatSignTopVoterSignLine1() {
		return getData().getString("Format.Signs.TopVoterSign.Line1", "TopVoter: %SiteName%");

	}

	/**
	 * Gets the sign top voter sign line 2.
	 *
	 * @return the sign top voter sign line 2
	 */
	public String getFormatSignTopVoterSignLine2() {
		return getData().getString("Format.Signs.TopVoterSign.Line2", "#%position%");
	}

	/**
	 * Gets the sign top voter sign line 3.
	 *
	 * @return the sign top voter sign line 3
	 */
	public String getFormatSignTopVoterSignLine3() {
		return getData().getString("Format.Signs.TopVoterSign.Line3", "%player%");

	}

	/**
	 * Gets the sign top voter sign line 4.
	 *
	 * @return the sign top voter sign line 4
	 */
	public String getFormatSignTopVoterSignLine4() {
		return getData().getString("Format.Signs.TopVoterSign.Line4", "%votes% Votes");

	}

	/**
	 * Gets the sign top voter sign right click message.
	 *
	 * @return the sign top voter sign right click message
	 */
	public String getFormatSignTopVoterSignRightClickMessage() {
		return getData().getString("Format.Signs.RightClickMessage",
				"&c&l%player% &cis &c&l%position% &cwith &c&l%votes% &cin &c&l%SiteName%");

	}

	/**
	 * Gets the time format.
	 *
	 * @return the time format
	 */
	public String getFormatTimeFormat() {
		return getData().getString("Format.TimeFormat", "EEE, d MMM yyyy HH:mm");

	}

	public String getFormatTopVoterAllTime() {
		return getData().getString("Format.TopVoter.AllTime", "AllTime");
	}

	public String getFormatTopVoterDaily() {
		return getData().getString("Format.TopVoter.Daily", "Daily");
	}

	public String getFormatTopVoterMonthly() {
		return getData().getString("Format.TopVoter.Monthly", "Monthly");
	}

	/**
	 * Gets the top voter reward msg.
	 *
	 * @return the top voter reward msg
	 */
	public String getFormatTopVoterRewardMsg() {
		return getData().getString("Format.TopVoterAwardMsg",
				"&aYou came in %place% in top voters of the month! Here is an award!");

	}

	public String getFormatTopVoterWeekly() {
		return getData().getString("Format.TopVoter.Weekly", "Weekly");
	}

	public String getFormatUserNotExist() {
		return getData().getString("Format.UserNotExist", "&cUser does not exist: %player%");
	}

	/**
	 * Gets the vote help.
	 *
	 * @return the vote help
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatVoteHelp() {
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Help.Lines", new ArrayList<String>());
	}

	public boolean getGiveDefaultPermission() {
		return getData().getBoolean("GiveDefaultPermission", true);
	}

	public ConfigurationSection getGUIVoteBestDayBestItem() {
		return getData().getConfigurationSection("GUI.VoteBest.DayBest.Item");
	}

	public ConfigurationSection getGUIVoteBestMonthBestItem() {
		return getData().getConfigurationSection("GUI.VoteBest.MonthBest.Item");
	}

	public String getGUIVoteBestName() {
		return getData().getString("GUI.VoteBest.Name", "VoteBest: %player%");
	}

	public ConfigurationSection getGUIVoteBestWeekBestItem() {
		return getData().getConfigurationSection("GUI.VoteBest.WeekBest.Item");
	}

	public String getGUIVoteGUIName() {
		return getData().getString("GUI.VoteGUIName", "&cVoteGUI: &c&l%player%");
	}

	public boolean getGUIVoteLastBackButton() {
		return getData().getBoolean("GUI.VoteLast.BackButton");
	}

	public String getGUIVoteLastName() {
		return getData().getString("GUI.VoteLast.Name", "VoteLast: %player%");
	}

	public boolean getGUIVoteNextBackButton() {
		return getData().getBoolean("GUI.VoteNext.BackButton");
	}

	public String getGUIVoteNextName() {
		return getData().getString("GUI.VoteNext.Name", "VoteNext: %player%");
	}

	public String getGUIVoteRewardName() {
		return getData().getString("GUI.VoteRewardName", "VoteReward");
	}

	public boolean getGUIVoteStreakBackButton() {
		return getData().getBoolean("GUI.VoteStreak.BackButton");
	}

	public ConfigurationSection getGUIVoteStreakCurrentDayStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.CurrentDayStreak.Item");
	}

	public ConfigurationSection getGUIVoteStreakCurrentMonthStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.CurrentMonthStreak.Item");
	}

	public ConfigurationSection getGUIVoteStreakCurrentWeekStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.CurrentWeekStreak.Item");
	}

	public ConfigurationSection getGUIVoteStreakHighestDayStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.HighestDayStreak.Item");
	}

	public ConfigurationSection getGUIVoteStreakHighestMonthStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.HighestMonthStreak.Item");
	}

	public ConfigurationSection getGUIVoteStreakHighestWeekStreakItem() {
		return getData().getConfigurationSection("GUI.VoteStreak.HighestWeekStreak.Item");
	}

	public String getGUIVoteStreakName() {
		return getData().getString("GUI.VoteStreak.Name", "VoteStreak");
	}

	public boolean getGUIVoteTodayBackButton() {
		return getData().getBoolean("GUI.VoteToday.BackButton");
	}

	public String getGUIVoteTodayName() {
		return getData().getString("GUI.VoteToday.Name", "VoteToday");
	}

	public boolean getGUIVoteTopBackButton() {
		return getData().getBoolean("GUI.VoteToday.BackButton");
	}

	public String getGUIVoteTopItemLore() {
		return getData().getString("GUI.VoteTop.Item.Lore", "&3&lVotes: &3%votes%");
	}

	public String getGUIVoteTopItemName() {
		return getData().getString("GUI.VoteTop.Item.Name", "&3&l%position%: &3%player%");
	}

	public String getGUIVoteTopName() {
		return getData().getString("GUI.VoteTop.Name", "VoteTop %topvoter%");
	}

	public int getGUIVoteTopSize() {
		return getData().getInt("GUI.VoteTop.Size", 27);
	}

	public ConfigurationSection getGUIVoteTopSwitchItem() {
		return getData().getConfigurationSection("GUI.VoteTop.SwitchItem");
	}

	public ConfigurationSection getGUIVoteTotalAllTimeTotalItem() {
		return getData().getConfigurationSection("GUI.VoteTotal.AllTimeTotal.Item");
	}

	public boolean getGUIVoteTotalBackButton() {
		return getData().getBoolean("GUI.VoteTotal.BackButton");
	}

	public ConfigurationSection getGUIVoteTotalDayTotalItem() {
		return getData().getConfigurationSection("GUI.VoteTotal.DayTotal.Item");
	}

	public ConfigurationSection getGUIVoteTotalItem(TopVoter top) {
		switch (top) {
			case AllTime:
				return getGUIVoteTotalAllTimeTotalItem();
			case Daily:
				return getGUIVoteTotalDayTotalItem();
			case Monthly:
				return getGUIVoteTotalMonthTotalItem();
			case Weekly:
				return getGUIVoteTotalWeekTotalItem();
			default:
				return getGUIVoteTotalAllTimeTotalItem();

		}
	}

	public ConfigurationSection getGUIVoteTotalMonthTotalItem() {
		return getData().getConfigurationSection("GUI.VoteTotal.MonthTotal.Item");
	}

	public String getGUIVoteTotalName() {
		return getData().getString("GUI.VoteTotal.Name", "VoteTotal: %player%");
	}

	public ConfigurationSection getGUIVoteTotalWeekTotalItem() {
		return getData().getConfigurationSection("GUI.VoteTotal.WeekTotal.Item");
	}

	public boolean getGUIVoteURLBackButton() {
		return getData().getBoolean("GUI.VoteURL.BackButton");
	}

	public String getGUIVoteURLName() {
		return getData().getString("GUI.VoteURL.Name", "&cVoteURL");
	}

	public String getGUIVoteURLSiteName() {
		return getData().getString("GUI.VoteURLSite.Name", "VoteSite %site%");
	}

	public String getGUIVoteURLURLText() {
		return getData().getString("GUI.VoteURL.URLText", "%VoteUrl%");
	}

	public int getIdentifierCost(String identifier) {
		return getData().getInt("Shop." + identifier + ".Cost");
	}

	public int getIdentifierLimit(String identifier) {
		return getData().getInt("Shop." + identifier + ".Limit", -1);
	}

	public String getIdentifierRewardsPath(String identifier) {
		return "Shop." + identifier + ".Rewards";

	}

	public Set<String> getIdentifiers() {
		ConfigurationSection shop = getData().getConfigurationSection("Shop");
		if (shop != null) {
			return shop.getKeys(false);
		}
		return new HashSet<String>();
	}

	public ConfigurationSection getIdentifierSection(String identifier) {
		return getData().getConfigurationSection("Shop." + identifier);
	}

	public boolean getLoadTopVoter(TopVoter top) {
		switch (top) {
			case AllTime:
				return getLoadTopVoterAllTime();
			case Daily:
				return getLoadTopVoterDaily();
			case Monthly:
				return getLoadTopVoterMonthly();
			case Weekly:
				return getLoadTopVoterWeekly();
			default:
				return false;
		}
	}

	public boolean getLoadTopVoterAllTime() {
		return getData().getBoolean("LoadTopVoter.AllTime", true);
	}

	public boolean getLoadTopVoterDaily() {
		return getData().getBoolean("LoadTopVoter.Daily");
	}

	public boolean getLoadTopVoterMonthly() {
		return getData().getBoolean("LoadTopVoter.Monthly", true);
	}

	public boolean getLoadTopVoterWeekly() {
		return getData().getBoolean("LoadTopVoter.Weekly");
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

	public int getPointsOnVote() {
		return getData().getInt("PointsOnVote", 1);
	}

	public int getPurgeMin() {
		return getData().getInt("PurgeMin", 90);
	}

	public boolean getPurgeOldData() {
		return getData().getBoolean("PurgeOldData");
	}

	/**
	 * Gets the request API default method.
	 *
	 * @return the request API default method
	 */
	public String getRequestAPIDefaultMethod() {
		return getData().getString("RequestAPI.DefaultMethod", "Anvil");
	}

	/**
	 * Gets the request API disabled methods.
	 *
	 * @return the request API disabled methods
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getRequestAPIDisabledMethods() {
		return (ArrayList<String>) getData().getList("RequestAPI.DisabledMethods", new ArrayList<String>());
	}

	public boolean getResetMilestonesMonthly() {
		return getData().getBoolean("ResetMilestonesMonthly");
	}

	/**
	 * Gets the store top voters daily.
	 *
	 * @return the store top voters daily
	 */
	public boolean getStoreTopVotersDaily() {
		return getData().getBoolean("StoreTopVoters.Daily");
	}

	/**
	 * Gets the store top voters monthly.
	 *
	 * @return the store top voters monthly
	 */
	public boolean getStoreTopVotersMonthly() {
		return getData().getBoolean("StoreTopVoters.Monthly");
	}

	/**
	 * Gets the store top voters weekly.
	 *
	 * @return the store top voters weekly
	 */
	public boolean getStoreTopVotersWeekly() {
		return getData().getBoolean("StoreTopVoters.Weekly");
	}

	public boolean getTopVoterAwardsTies() {
		return getData().getBoolean("TopVoterAwardsTies", true);
	}

	public boolean getTopVoterIgnorePermission() {
		return getData().getBoolean("TopVoterIgnorePermission");
	}

	/**
	 * Gets the vote GUI slot command.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot command
	 */

	public String getVoteGUISlotCommand(String slot) {
		return getData().getString("GUI.VoteGUI." + slot + ".Command", "");
	}

	/**
	 * Gets the vote GUI slot lore.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot lore
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getVoteGUISlotLore(String slot) {
		return (ArrayList<String>) getData().getList("GUI.VoteGUI." + slot + ".Item.Lore", new ArrayList<String>());
	}

	/**
	 * Gets the vote GUI slots.
	 *
	 * @return the vote GUI slots
	 */
	public Set<String> getVoteGUISlots() {
		try {
			return getData().getConfigurationSection("GUI.VoteGUI").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public ConfigurationSection getVoteGUISlotSection(String slot) {
		return getData().getConfigurationSection("GUI.VoteGUI." + slot + ".Item");
	}

	/**
	 * Gets the vote GUI slot slot.
	 *
	 * @param slot
	 *            the slot
	 * @return the vote GUI slot slot
	 */
	public int getVoteGUISlotSlot(String slot) {
		return getData().getInt("GUI.VoteGUI." + slot + ".Slot", -1);
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

	/**
	 * Gets the enabled.
	 *
	 * @return the enabled
	 */
	public boolean getVoteRemindingEnabled() {
		return getData().getBoolean("VoteReminding.Enabled");
	}

	/**
	 * Gets the remind delay.
	 *
	 * @return the remind delay
	 */
	public int getVoteRemindingRemindDelay() {
		return getData().getInt("VoteReminding.RemindDelay", 30);
	}

	/**
	 * Gets the remind on login.
	 *
	 * @return the remind on login
	 */
	public boolean getVoteRemindingRemindOnLogin() {
		return getData().getBoolean("VoteReminding.RemindOnLogin");
	}

	/**
	 * Gets the remind only once.
	 *
	 * @return the remind only once
	 */
	public boolean getVoteRemindingRemindOnlyOnce() {
		return getData().getBoolean("VoteReminding.RemindOnlyOnce");
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	public String getVoteRemindingRewardsPath() {
		return "VoteReminding.Rewards";
	}

	public boolean getVoteShopBackButton() {
		return getData().getBoolean("VoteShopBackButton", true);
	}

	public boolean getVoteShopEnabled() {
		return getData().getBoolean("VoteShopEnabled", true);
	}

	public String getVoteShopName() {
		return getData().getString("GUI.VoteShopName", "VoteShop");
	}

	public boolean getVoteShopNotBuyable(String shop) {
		return getData().getBoolean("Shop." + shop + ".NotBuyable", false);
	}

	public String getVoteShopPermission(String ident) {
		return getData().getString("Shop." + ident + ".Permission", "");
	}

	/**
	 * Gets the vote site items.
	 *
	 * @param site
	 *            the site
	 * @return the vote site items
	 */
	public Set<String> getVoteSiteItems(String site) {
		String siteName = site.replace(".", "-");
		try {
			return getData().getConfigurationSection("GUI.VoteReward." + siteName + ".Items").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public ConfigurationSection getVoteSiteItemsSection(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getConfigurationSection("GUI.VoteReward." + siteName + ".Items." + item);
	}

	/**
	 * Gets the vote site items slot.
	 *
	 * @param site
	 *            the site
	 * @param item
	 *            the item
	 * @return the vote site items slot
	 */
	public int getVoteSiteItemsSlot(String site, String item) {
		String siteName = site.replace(".", "-");
		return getData().getInt("GUI.VoteReward." + siteName + ".Items." + item + ".Slot");
	}

	/**
	 * Gets the votes required.
	 *
	 * @return the votes required
	 */
	public int getVotesRequired() {
		return getData().getInt("VotesRequired");
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
			return new HashSet<String>();
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public String getVoteTopDefault() {
		return getData().getString("VoteTopDefault", "Monthly");
	}

	public ConfigurationSection getVoteURLAlreadyVotedAllUrlsButtonItemSection() {
		if (getData().isConfigurationSection("GUI.VoteURL.AllUrlsButton.AlreadyVotedItem")) {
			return getData().getConfigurationSection("GUI.VoteURL.AllUrlsButton.AlreadyVotedItem");
		} else {
			return getData().getConfigurationSection("GUI.VoteURL.AlreadyVotedItem");
		}
	}

	public ConfigurationSection getVoteURLAlreadyVotedItemSection() {
		return getData().getConfigurationSection("GUI.VoteURL.AlreadyVotedItem");
	}

	public ConfigurationSection getVoteURLCanVoteAllUrlsButtonItemSection() {
		if (getData().isConfigurationSection("GUI.VoteURL.AllUrlsButton.CanVoteItem")) {
			return getData().getConfigurationSection("GUI.VoteURL.AllUrlsButton.CanVoteItem");
		} else {
			return getData().getConfigurationSection("GUI.VoteURL.CanVoteItem");
		}
	}

	public ConfigurationSection getVoteURLCanVoteItemSection() {
		return getData().getConfigurationSection("GUI.VoteURL.CanVoteItem");
	}

	/**
	 * Gets the vote URL next vote.
	 *
	 * @return the vote URL next vote
	 */
	public String getVoteURLNextVote() {
		return getData().getString("GUI.VoteURL.NextVote", "&cCan Vote In: %Info%");

	}

	/**
	 * Gets the vote URL see URL.
	 *
	 * @return the vote URL see URL
	 */
	public String getVoteURLSeeURL() {
		return getData().getString("GUI.VoteURL.SeeURL", "&cClick to see URL");
	}

	/**
	 * Gets the vote URL site name.
	 *
	 * @return the vote URL site name
	 */
	public String getVoteURLSiteName() {
		return getData().getString("GUI.VoteURL.SiteName", "&c%Name%");
	}

	/**
	 * Gets the vote URL view all urls button enabled.
	 *
	 * @return the vote URL view all urls button enabled
	 */
	public boolean getVoteURLViewAllUrlsButtonEnabled() {
		return getData().getBoolean("GUI.VoteURL.ViewAllUrlsButtonEnabled");
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

	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("Config.yml", true);
	}

}
