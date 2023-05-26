package com.bencodez.votingplugin.config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.yml.YMLFile;
import com.bencodez.advancedcore.api.yml.annotation.AnnotationHandler;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataBoolean;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataInt;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataListString;
import com.bencodez.advancedcore.api.yml.annotation.ConfigDataString;
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

	@ConfigDataBoolean(path = "PerSiteCoolDownEvents")
	@Getter
	private boolean perSiteCoolDownEvents = false;

	@ConfigDataBoolean(path = "QueueVotesDuringTimeChange")
	@Getter
	private boolean queueVotesDuringTimeChange = false;

	@ConfigDataBoolean(path = "AllowUnjoined")
	@Getter
	private boolean allowUnjoined = false;

	@ConfigDataBoolean(path = "PreloadSkulls")
	@Getter
	private boolean preloadSkulls = false;

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
	private ArrayList<String> disabledCommands = new ArrayList<String>();

	@ConfigDataListString(path = "DisabledDefaultPermissions")
	@Getter
	private ArrayList<String> disabledDefaultPermissions = new ArrayList<String>();

	@ConfigDataListString(path = "CachedPlaceholders")
	@Getter
	private ArrayList<String> cachedPlaceholders = new ArrayList<String>();

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

	@ConfigDataString(path = "Format.Commands.Vote.Last.TimeFormat", defaultValue = "%amount% %TimeType%")
	@Getter
	private String formatCommandsVoteLastTimeFormat;

	@ConfigDataString(path = "Format.Commands.Vote.Today.Line")
	@Getter
	private String formatCommandsVoteTodayLine = "&6%player% : %VoteSite% : %Time%";

	@ConfigDataListString(path = "Format.Commands.Vote.Today.Title")
	@Getter
	private ArrayList<String> formatCommandsVoteTodayTitle = ArrayUtils.getInstance()
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
	private ArrayList<String> votingBroadcastBlacklist = new ArrayList<String>();

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
		return (ArrayList<String>) getData().getList("BlackList", new ArrayList<String>());
	}

	public Set<String> getCustomCommands() {
		Set<String> str = getData().getConfigurationSection("CustomCommands").getKeys(false);
		if (str == null) {
			str = new HashSet<String>();
		}
		return str;
	}

	public Set<String> getCustomPlaceholderReturns() {
		Set<String> str = getData().getConfigurationSection("CustomPlaceholderReturns").getKeys(false);
		if (str == null) {
			str = new HashSet<String>();
		}
		return str;
	}

	public Set<String> getCustomPlaceholderReturns(String placeholder) {
		Set<String> str = getData().getConfigurationSection("CustomPlaceholderReturns." + placeholder).getKeys(false);
		if (str == null) {
			str = new HashSet<String>();
		}
		return str;
	}

	public String getCustomPlaceholderReturns(String placeholder, String returnString) {
		return getData().getString("CustomPlaceholderReturns." + placeholder + "." + returnString, "");
	}

	public ConfigurationSection getCustomCommands(String ident) {
		return getData().getConfigurationSection("CustomCommands." + ident);
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

	public String getFormatCommandsVoteHelpHoverColor() {
		return getData().getString("Format.Commands.Vote.Help.HoverColor", "AQUA");
	}

	/**
	 * Gets the commands vote help line.
	 *
	 * @return the commands vote help line
	 */
	public String getFormatCommandsVoteHelpLine() {
		return getData().getString("Format.Commands.Vote.Help.Line", "&6%Command% - &6%HelpMessage%");
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
		return getData().getString("Format.Commands.Vote.Help.Title", "&6Voting Player Help");

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

	public PlaceholderCacheLevel getPlaceholderCacheLevel() {
		return PlaceholderCacheLevel.getCache(getData().getString("PlaceholderCacheLevel", "AUTO"));
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
		return getData().getString("Format.ShopFailed", "&cYou do not have %Points% points to purhcase this!");
	}

	public String getFormatShopNotPurchasable() {
		return getData().getString("Format.ShopNotPurchasable", "&cThis item is not buyable!");
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
		return getData().getBoolean("LoadTopVoter.Daily", false);
	}

	public boolean getLoadTopVoterMonthly() {
		return getData().getBoolean("LoadTopVoter.Monthly", true);
	}

	public boolean getLoadTopVoterWeekly() {
		return getData().getBoolean("LoadTopVoter.Weekly", false);
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

	/**
	 * Gets the store top voters daily.
	 *
	 * @return the store top voters daily
	 */
	public boolean getStoreTopVotersDaily() {
		return getData().getBoolean("StoreTopVoters.Daily");
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

	public String getVoteTopDefault() {
		return getData().getString("VoteTopDefault", "Monthly");
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
