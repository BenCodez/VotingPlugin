package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.file.YMLFile;
import com.bencodez.simpleapi.file.annotation.AnnotationHandler;
import com.bencodez.simpleapi.file.annotation.ConfigDataBoolean;
import com.bencodez.simpleapi.file.annotation.ConfigDataInt;
import com.bencodez.simpleapi.file.annotation.ConfigDataKeys;
import com.bencodez.simpleapi.file.annotation.ConfigDataListString;
import com.bencodez.simpleapi.file.annotation.ConfigDataLong;
import com.bencodez.simpleapi.file.annotation.ConfigDataString;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.placeholders.PlaceholderCacheLevel;
import com.bencodez.votingplugin.topvoter.TopVoter;

import lombok.Getter;

// TODO: Auto-generated Javadoc
/**
 * The Class Config.
 */
public class Config extends YMLFile {

	@ConfigDataBoolean(path = "AddCustomCommands")
	@Getter
	private boolean addCustomCommands = false;

	@ConfigDataBoolean(path = "AddTotals")
	@Getter
	private boolean addTotals = true;

	@ConfigDataBoolean(path = "AddTotalsOffline")
	@Getter
	private boolean addTotalsOffline = true;

	@ConfigDataBoolean(path = "LoadInteralExpansion")
	@Getter
	private boolean loadInteralExpansion = true;

	@ConfigDataBoolean(path = "PerSiteCoolDownEvents")
	@Getter
	private boolean perSiteCoolDownEvents = false;

	@ConfigDataInt(path = "LimitVotePoints")
	@Getter
	private int limitVotePoints = -1;

	@ConfigDataBoolean(path = "QueueVotesDuringTimeChange")
	@Getter
	private boolean queueVotesDuringTimeChange = false;

	@ConfigDataBoolean(path = "AdvancedServiceSiteHandling")
	@Getter
	private boolean advancedServiceSiteHandling = false;

	@ConfigDataBoolean(path = "AllowUnjoined")
	@Getter
	private boolean allowUnjoined = false;

	@ConfigDataBoolean(path = "PreloadSkulls")
	@Getter
	private boolean preloadSkulls = false;

	@ConfigDataBoolean(path = "DiscordSRV.Enabled")
	@Getter
	private boolean discordSRVEnabled = false;

	@ConfigDataBoolean(path = "DiscordSRV.TopVoter.Enabled")
	@Getter
	private boolean discordSRVTopVoterEnabled = false;

	@ConfigDataLong(path = "DiscordSRV.TopVoter.Channel")
	@Getter
	private long discordSRVTopVoterChannel = 0;

	@ConfigDataString(path = "DiscordSRV.TopVoter.Title")
	@Getter
	private String discordSRVTopVoterTitle = "&3Top Voters of the Month";

	@ConfigDataString(path = "DiscordSRV.TopVoter.RankDisplay")
	@Getter
	private String discordSRVTopVoterRankDisplay = "&c%rank%: &6%player% - %votes% Votes";

	@ConfigDataBoolean(path = "AllowUnJoinedCheckServer")
	@Getter
	private boolean allowUnJoinedCheckServer = true;

	@ConfigDataBoolean(path = "AllowVotePointTransfers")
	@Getter
	private boolean allowVotePointTransfers = false;

	@ConfigDataBoolean(path = "AlternateUUIDLookup")
	@Getter
	private boolean alternateUUIDLookup = false;

	@ConfigDataBoolean(path = "AlwaysCloseInventory")
	@Getter
	private boolean alwaysCloseInventory = false;

	@ConfigDataBoolean(path = "ExtraVoteShopCheck")
	@Getter
	private boolean extraVoteShopCheck = true;

	@ConfigDataBoolean(path = "TrackShopPurchases")
	@Getter
	private boolean trackShopPurchases = false;

	@ConfigDataBoolean(path = "AlwaysUpdate")
	@Getter
	private boolean alwaysUpdate = false;

	@ConfigDataBoolean(path = "AutoCreateVoteSites")
	@Getter
	private boolean autoCreateVoteSites = true;

	@ConfigDataBoolean(path = "Format.BroadcastVote")
	@Getter
	private boolean broadcastVotesEnabled = true;

	@ConfigDataBoolean(path = "CountFakeVotes")
	@Getter
	private boolean countFakeVotes = true;

	@ConfigDataInt(path = "DelayBetweenUpdates")
	@Getter
	private int delayBetweenUpdates = 3;

	@ConfigDataBoolean(path = "DisableAdvancedTab")
	@Getter
	private boolean disableAdvancedTab = false;

	@ConfigDataBoolean(path = "DisableCoolDownCheck")
	@Getter
	private boolean disableCoolDownCheck = false;

	@ConfigDataListString(path = "DisabledCommands")
	@Getter
	private ArrayList<String> disabledCommands = new ArrayList<>();

	@ConfigDataListString(path = "DisabledDefaultPermissions")
	@Getter
	private ArrayList<String> disabledDefaultPermissions = new ArrayList<>();

	@ConfigDataListString(path = "CachedPlaceholders")
	@Getter
	private ArrayList<String> cachedPlaceholders = new ArrayList<>();

	@ConfigDataBoolean(path = "DisableInteractEvent")
	@Getter
	private boolean disableInteractEvent = false;

	@ConfigDataBoolean(path = "DisableNoServiceSiteMessage")
	@Getter
	private boolean disableNoServiceSiteMessage = false;

	@ConfigDataBoolean(path = "DisableUpdateChecking")
	@Getter
	private boolean disableUpdateChecking = false;

	@ConfigDataBoolean(path = "ExtraAllSitesCheck")
	@Getter
	private boolean extraAllSitesCheck = false;

	@ConfigDataBoolean(path = "CloseInventoryOnVote")
	@Getter
	private boolean closeInventoryOnVote = true;

	@ConfigDataBoolean(path = "ExtraBackgroundUpdate")
	@Getter
	private boolean extraBackgroundUpdate = false;

	@ConfigDataString(path = "Format.AlternateBroadcast.Broadcast")
	@Getter
	private String formatAlternateBroadcastBroadcast = "&6[&4Broadcast&6] &2%numberofplayers% voted in the last half hour! /vote";

	@ConfigDataInt(path = "Format.AlternateBroadcast.Delay")
	@Getter
	private int formatAlternateBroadcastDelay = 30;

	@ConfigDataBoolean(path = "Format.AlternateBroadcast.Enabled")
	@Getter
	private boolean formatAlternateBroadcastEnabled = false;

	@ConfigDataBoolean(path = "Format.OnlyOneOfflineBroadcast")
	@Getter
	private boolean formatOnlyOneOfflineBroadcast = false;

	@ConfigDataString(path = "Format.OfflineBroadcast")
	@Getter
	private String formatOfflineBroadcast = "&6[&4Broadcast&6] &2Thanks &c%player% &2for voting on %numberofvotes% times!";

	@ConfigDataBoolean(path = "Format.Commands.Vote.ForceLinks")
	@Getter
	private boolean formatCommandsVoteForceLinks = true;

	@ConfigDataString(path = "Format.Commands.Vote.Last.LastVoted")
	@Getter
	private String formatCommandsVoteLastLastVoted = "%times% ago";

	@ConfigDataString(path = "Format.Commands.Vote.GivePoints.NotEnoughPoints")
	@Getter
	private String formatCommandsVoteGivePointsNotEnoughPoints = "&cNot enough points";

	@ConfigDataString(path = "Format.Commands.Vote.GivePoints.NotJoinedServer")
	@Getter
	private String formatCommandsVoteGivePointsNotJoinedServer = "&c%player% has not joined the server";

	@ConfigDataString(path = "Format.Commands.Vote.GivePoints.NumberLowerThanZero")
	@Getter
	private String formatCommandsVoteGivePointsNumberLowerThanZero = "&cNumber of points needs to be greater than 0";

	@ConfigDataString(path = "Format.Commands.Vote.GivePoints.TransferFrom")
	@Getter
	private String formatCommandsVoteGivePointsTransferFrom = "&c%transfer% points given to %touser%";

	@ConfigDataString(path = "Format.Commands.Vote.GivePoints.TransferTo")
	@Getter
	private String formatCommandsVoteGivePointsTransferTo = "&cYou received %transfer points from %fromuser%";

	@ConfigDataString(path = "Format.Commands.Vote.Last.Line", defaultValue = "&3%SiteName%: &6%timeSince% ago")
	@Getter
	private String formatCommandsVoteLastLine;

	@ConfigDataString(path = "Format.Commands.Vote.Last.NeverVoted", defaultValue = "Never voted")
	@Getter
	private String formatCommandsVoteLastNeverVoted;

	@ConfigDataBoolean(path = "Format.Commands.Vote.Last.IncludeSeconds")
	@Getter
	private boolean formatCommandsVoteLastIncludeSeconds = true;

	@ConfigDataString(path = "Format.Commands.Vote.Last.TimeFormat", defaultValue = "%amount% %TimeType%")
	@Getter
	private String formatCommandsVoteLastTimeFormat;

	@ConfigDataString(path = "Format.Commands.Vote.Today.Line")
	@Getter
	private String formatCommandsVoteTodayLine = "&6%player% : %VoteSite% : %Time%";

	@ConfigDataListString(path = "Format.Commands.Vote.Today.Title")
	@Getter
	private ArrayList<String> formatCommandsVoteTodayTitle = ArrayUtils
			.convert(new String[] { "&cToday's Votes %page%/%maxpage%", "&cPlayerName : VoteSite : Time" });

	@ConfigDataString(path = "Format.Commands.Vote.ToggleBroadcasts.Disabled")
	@Getter
	private String formatCommandsVoteToggleBroadcastDisabled = "&cYou will no longer see vote broadcasts";

	@ConfigDataString(path = "Format.Commands.Vote.ToggleBroadcasts.Enabled")
	@Getter
	private String formatCommandsVoteToggleBroadcastEnabled = "&cYou will now see vote broadcasts";

	@ConfigDataString(path = "Format.Commands.Vote.ToggleReminders.Disabled")
	@Getter
	private String formatCommandsVoteToggleRemindersDisabled = "&cVote reminders disabled";

	@ConfigDataString(path = "Format.Commands.Vote.ToggleReminders.Enabled")
	@Getter
	private String formatCommandsVoteToggleRemindersEnabled = "&cVote reminders enabled";

	@Getter
	private int formatPageSize = 10;

	@ConfigDataString(path = "Format.TimeFormats.Day", defaultValue = "Day")
	@Getter
	private String formatTimeFormatsDay;

	@ConfigDataString(path = "Format.TimeFormats.Days", defaultValue = "Days")
	@Getter
	private String formatTimeFormatsDays;

	@ConfigDataString(path = "Format.TimeFormats.Hour", defaultValue = "Hour")
	@Getter
	private String formatTimeFormatsHour;

	@ConfigDataString(path = "Format.TimeFormats.Hours", defaultValue = "Hours")
	@Getter
	private String formatTimeFormatsHours;

	@ConfigDataString(path = "Format.TimeFormats.Minute", defaultValue = "Minute")
	@Getter
	private String formatTimeFormatsMinute;

	@ConfigDataString(path = "Format.TimeFormats.Minutes", defaultValue = "Minutes")
	@Getter
	private String formatTimeFormatsMinutes;

	@ConfigDataString(path = "Format.TimeFormats.Second", defaultValue = "Second")
	@Getter
	private String formatTimeFormatsSecond;

	@ConfigDataString(path = "Format.TimeFormats.Seconds", defaultValue = "Seconds")
	@Getter
	private String formatTimeFormatsSeconds;

	@ConfigDataBoolean(path = "LimitMonthlyVotes")
	@Getter
	private boolean limitMonthlyVotes = false;

	@ConfigDataBoolean(path = "LoadCommandAliases")
	@Getter
	private boolean loadCommandAliases = true;

	@ConfigDataBoolean(path = "LogVotesToFile")
	@Getter
	private boolean logVotesToFile = false;

	@ConfigDataInt(path = "MaxiumNumberOfTopVotersToLoad")
	@Getter
	private int MaxiumNumberOfTopVotersToLoad = 1000;

	@ConfigDataBoolean(path = "OverrideVersionDisable")
	@Getter
	private boolean overrideVersionDisable = false;

	@ConfigDataBoolean(path = "PreventRepeatMilestones")
	@Getter
	private boolean preventRepeatMilestones = false;

	@ConfigDataBoolean(path = "OfflineVotesLimit.Enabled")
	@Getter
	private boolean offlineVotesLimitEnabled = false;

	@ConfigDataInt(path = "OfflineVotesLimit.Amount")
	@Getter
	private int offlineVotesLimitAmount = 5;

	@ConfigDataBoolean(path = "UpdateWithPlayersOnlineOnly")
	@Getter
	private boolean updateWithPlayersOnlineOnly = false;

	@ConfigDataBoolean(path = "UseHighestTotals")
	@Getter
	private boolean useHighestTotals = true;

	@ConfigDataBoolean(path = "UseJavascriptPlaceholders")
	@Getter
	private boolean useJavascriptPlaceholders = false;

	@ConfigDataBoolean(path = "UsePrimaryAccountForPlaceholders")
	@Getter
	private boolean usePrimaryAccountForPlaceholders = false;

	@ConfigDataBoolean(path = "UseVoteGUIMainCommand")
	@Getter
	private boolean useVoteGUIMainCommand = false;

	@ConfigDataBoolean(path = "UseVoteStreaks")
	@Getter
	private boolean useVoteStreaks = true;

	@ConfigDataListString(path = "VotingBroadcastBlacklist")
	@Getter
	private ArrayList<String> votingBroadcastBlacklist = new ArrayList<>();

	@ConfigDataKeys(path = "CustomCommands")
	@Getter
	private Set<String> customCommands = new HashSet<>();

	@ConfigDataKeys(path = "CustomPlaceholderReturns")
	@Getter
	private Set<String> customPlaceholderReturns = new HashSet<>();

	@ConfigDataString(path = "Format.BroadcastMsg")
	@Getter
	private String formatBroadCastMsg = "&6[&4Broadcast&6] &2Thanks &c%player% &2for voting on %SiteName%";

	@ConfigDataBoolean(path = "Format.BroadcastWhenOnline")
	@Getter
	private boolean formatBroadcastWhenOnline = false;

	@ConfigDataBoolean(path = "Format.Commands.Vote.AutoInputSites")
	@Getter
	private boolean formatCommandsVoteAutoInputSites = true;

	@ConfigDataBoolean(path = "Format.Commands.Vote.OnlyShowSitesToVote")
	@Getter
	private boolean formatCommandsVoteOnlyShowSitesToVote = false;

	@ConfigDataListString(path = "Format.Commands.Vote.Best.Lines")
	@Getter
	private ArrayList<String> formatCommandsVoteBestLines = new ArrayList<>();

	@ConfigDataString(path = "Format.Commands.Vote.Best.Title")
	@Getter
	private String formatCommandsVoteBestTitle = "&3&l%player% Best Votes";

	@ConfigDataString(path = "Format.Commands.Vote.Help.HoverColor")
	@Getter
	private String formatCommandsVoteHelpHoverColor = "AQUA";

	@ConfigDataString(path = "Format.Commands.Vote.Help.Line")
	@Getter
	private String formatCommandsVoteHelpLine = "&6%Command% - &6%HelpMessage%";

	@ConfigDataBoolean(path = "Format.Commands.Vote.Help.RequirePermission")
	@Getter
	private boolean formatCommandsVoteHelpRequirePermission = false;

	@ConfigDataString(path = "Format.Commands.Vote.Help.Title")
	@Getter
	private String formatCommandsVoteHelpTitle = "&6Voting Player Help";

	@ConfigDataString(path = "Format.Commands.Vote.Last.Title")
	@Getter
	private String formatCommandsVoteLastTitle = "&3&l%player% Last Vote Times:";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Info.CanVote")
	@Getter
	private String formatCommandsVoteNextInfoCanVote = "Go Vote!";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Info.Error")
	@Getter
	private String formatCommandsVoteNextInfoError = "";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Info.TimeUntilVote")
	@Getter
	private String formatCommandsVoteNextInfoTime = "&cCould not caculate time until next vote!";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Info.VoteDelayDaily")
	@Getter
	private String formatCommandsVoteNextInfoVoteDelayDaily = "%hours% Hours and %minutes% Minutes";

	@ConfigDataString(path = "PlaceholderCacheLevel", options = { "AUTO", "SPECIFIC", "NONE" })
	@Getter
	private String placeholderCacheLevelString = "AUTO";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Layout")
	@Getter
	private String formatCommandsVoteNextLayout = "&3%SiteName%: &6%info%";

	@ConfigDataString(path = "Format.Commands.Vote.Next.Title")
	@Getter
	private String formatCommandsVoteNextTitle = "&3&l%player% Next Votes:";

	@ConfigDataString(path = "Format.Commands.Vote.Streak.Title")
	@Getter
	private String formatCommandsVoteStreakTitle = "&3&l%player% Vote Streak";

	@ConfigDataString(path = "Format.Commands.Vote.Sites")
	@Getter
	private String formatCommandsVoteURLS = "&4%num%: &c&l%SiteName% - &c%url%";

	@ConfigDataString(path = "Format.Commands.Vote.Points")
	@Getter
	private String formatCommandsVotePoints = "&a%Player% currently has &a&l%Points%&a Points!";

	@ConfigDataString(path = "Format.Commands.Vote.Top.Line")
	@Getter
	private String formatCommandsVoteTopLine = "&c%num%: &6%player%, %votes%";

	@ConfigDataString(path = "Format.Commands.Vote.Top.Title")
	@Getter
	private String formatCommandVoteTopTitle = "&3Top %Top% Voters %page%/%maxpages%";

	@ConfigDataString(path = "Format.NextPage")
	@Getter
	private String formatNextPage = "&aNext Page";

	@ConfigDataString(path = "Format.NoPerms")
	@Getter
	private String formatNoPerms = "&cYou do not have enough permission!";

	@ConfigDataString(path = "Format.NotNumber")
	@Getter
	private String formatNotNumber = "&cError on &6%arg%&c, number expected!";

	@ConfigDataString(path = "Format.PrevPage")
	@Getter
	private String formatPrevPage = "&aPrevious Page";

	@ConfigDataString(path = "Format.ShopFailed")
	@Getter
	private String formatShopFailedMsg = "&cYou do not have %Points% points to purhcase this!";

	@ConfigDataString(path = "Format.ShopNotPurchasable")
	@Getter
	private String formatShopNotPurchasable = "&cThis item is not buyable!";

	@ConfigDataString(path = "Format.ShopPurchase")
	@Getter
	private String formatShopPurchaseMsg = "&aYou bought the %Identifier% for %Points% Points!";

	@ConfigDataString(path = "Format.Signs.TopVoterSign.Line1")
	@Getter
	private String formatSignTopVoterSignLine1 = "TopVoter: %SiteName%";

	@ConfigDataString(path = "Format.Signs.TopVoterSign.Line2")
	@Getter
	private String formatSignTopVoterSignLine2 = "#%position%";

	@ConfigDataString(path = "Format.Signs.TopVoterSign.Line3")
	@Getter
	private String formatSignTopVoterSignLine3 = "%player%";

	@ConfigDataString(path = "Format.Signs.TopVoterSign.Line4")
	@Getter
	private String formatSignTopVoterSignLine4 = "%votes% Votes";

	@ConfigDataString(path = "Format.Signs.RightClickMessage")
	@Getter
	private String formatSignTopVoterSignRightClickMessage = "&c&l%player% &cis &c&l%position% &cwith &c&l%votes% &cin &c&l%SiteName%";

	@ConfigDataString(path = "Format.TimeFormat")
	@Getter
	private String formatTimeFormat = "EEE, d MMM yyyy HH:mm";

	@ConfigDataString(path = "Format.TopVoter.AllTime")
	@Getter
	private String formatTopVoterAllTime = "AllTime";

	@ConfigDataString(path = "Format.TopVoter.Daily")
	@Getter
	private String formatTopVoterDaily = "Daily";

	@ConfigDataString(path = "Format.TopVoter.Monthly")
	@Getter
	private String formatTopVoterMonthly = "Monthly";

	@ConfigDataString(path = "Format.TopVoterAwardMsg")
	@Getter
	private String formatTopVoterRewardMsg = "&aYou came in %place% in top voters of the month! Here is an award!";

	@ConfigDataString(path = "Format.TopVoter.Weekly")
	@Getter
	private String formatTopVoterWeekly = "Weekly";

	@ConfigDataString(path = "Format.UserNotExist")
	@Getter
	private String formatUserNotExist = "&cUser does not exist: %player%";

	@ConfigDataString(path = "Format.InvalidCommand.Vote")
	@Getter
	private String formatInvalidCommandVote = "&4No valid arguments, see /vote help!";
	@ConfigDataString(path = "Format.InvalidCommand.AdminVote")
	@Getter
	private String formatInvalidCommandAdminVote = "&4No valid arguments, see /adminvote help!";
	@ConfigDataBoolean(path = "GiveDefaultPermission")
	@Getter
	private boolean giveDefaultPermission = true;

	@ConfigDataBoolean(path = "LoadTopVoter.AllTime")
	@Getter
	private boolean loadTopVoterAllTime = true;

	@ConfigDataBoolean(path = "LoadTopVoter.Monthly")
	@Getter
	private boolean loadTopVoterMonthly = true;

	@ConfigDataBoolean(path = "LoadTopVoter.Daily")
	@Getter
	private boolean loadTopVoterDaily = false;

	@ConfigDataBoolean(path = "LoadTopVoter.Weekly")
	@Getter
	private boolean loadTopVoterWeekly = false;

	@ConfigDataInt(path = "PointsOnVote")
	@Getter
	private int pointsOnVote = 1;

	@ConfigDataBoolean(path = "PurgeNoDataOnStartup")
	@Getter
	private boolean purgeNoDataOnStartup = false;

	@ConfigDataBoolean(path = "StoreMonthTotalsWithDate")
	@Getter
	private boolean storeMonthTotalsWithDate = false;

	@ConfigDataBoolean(path = "UseMonthDateTotalsAsPrimaryTotal")
	@Getter
	private boolean useMonthDateTotalsAsPrimaryTotal = false;

	@ConfigDataString(path = "RequestAPI.DefaultMethod")
	@Getter
	private String requestAPIDefaultMethod = "ANVIL";

	@ConfigDataListString(path = "RequestAPI.DisabledMethods")
	@Getter
	private ArrayList<String> requestAPIDisabledMethods = new ArrayList<>();

	@ConfigDataBoolean(path = "StoreTopVoters.Daily")
	@Getter
	private boolean storeTopVotersDaily = false;

	@ConfigDataBoolean(path = "StoreTopVoters.Weekly")
	@Getter
	private boolean storeTopVotersWeekly = false;

	@ConfigDataBoolean(path = "TopVoterAwardsTies")
	@Getter
	private boolean topVoterAwardsTies = true;

	@ConfigDataBoolean(path = "TopVoterIgnorePermission")
	@Getter
	private boolean topVoterIgnorePermission = false;

	@ConfigDataBoolean(path = "VoteReminding.Enabled")
	@Getter
	private boolean voteRemindingEnabled = true;

	@ConfigDataInt(path = "VoteReminding.RemindDelay")
	@Getter
	private int voteRemindingRemindDelay = 30;

	@ConfigDataBoolean(path = "VoteReminding.RemindOnLogin")
	@Getter
	private boolean voteRemindingRemindOnLogin = false;

	@ConfigDataBoolean(path = "VoteReminding.RemindOnlyOnce")
	@Getter
	private boolean voteRemindingRemindOnlyOnce = false;

	@ConfigDataString(path = "VoteTopDefault")
	@Getter
	private String voteTopDefault = "Monthly";

	public Config(VotingPluginMain plugin) {
		super(plugin, new File(plugin.getDataFolder(), "Config.yml"));
	}

	/**
	 * Gets the black list.
	 *
	 * @return the black list
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getBlackList() {
		return (ArrayList<String>) getData().getList("BlackList", new ArrayList<>());
	}

	public ConfigurationSection getCustomCommands(String ident) {
		return getData().getConfigurationSection("CustomCommands." + ident);
	}

	public Set<String> getCustomPlaceholderReturns(String placeholder) {
		Set<String> str = getData().getConfigurationSection("CustomPlaceholderReturns." + placeholder).getKeys(false);
		if (str == null) {
			str = new HashSet<>();
		}
		return str;
	}

	public String getCustomPlaceholderReturns(String placeholder, String returnString) {
		return getData().getString("CustomPlaceholderReturns." + placeholder + "." + returnString, "");
	}

	/**
	 * Gets the commands vote party.
	 *
	 * @return the commands vote party
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteParty() {
		ArrayList<String> msg = new ArrayList<>();
		msg.add("&cCurrently at &6%Votes%&c, &6%NeededVotes% &cmore votes to go to reach &6%VotesRequired%");
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Party", msg);

	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteStreakLines() {
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Streak.Lines", new ArrayList<>());
	}

	/**
	 * Gets the commands vote text.
	 *
	 * @return the commands vote text
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteText() {
		ArrayList<String> str = new ArrayList<>();
		str.add("&4&lVote for our server!");
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Text", str);
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatCommandsVoteTotal() {
		ArrayList<String> list = (ArrayList<String>) getData().getList("Format.Commands.Vote.Total", new ArrayList<>());
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
				new ArrayList<>());
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
	 * Gets the vote help.
	 *
	 * @return the vote help
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getFormatVoteHelp() {
		return (ArrayList<String>) getData().getList("Format.Commands.Vote.Help.Lines", new ArrayList<>());
	}

	public boolean getLoadTopVoter(TopVoter top) {
		switch (top) {
		case AllTime:
			return isLoadTopVoterAllTime();
		case Daily:
			return isLoadTopVoterDaily();
		case Monthly:
			return isLoadTopVoterMonthly();
		case Weekly:
			return isLoadTopVoterWeekly();
		default:
			return false;
		}
	}

	public PlaceholderCacheLevel getPlaceholderCacheLevel() {
		return PlaceholderCacheLevel.getCache(getPlaceholderCacheLevelString());
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	public String getVoteRemindingRewardsPath() {
		return "VoteReminding.Rewards";
	}

	@Override
	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		getPlugin().saveResource("Config.yml", true);
	}

}
