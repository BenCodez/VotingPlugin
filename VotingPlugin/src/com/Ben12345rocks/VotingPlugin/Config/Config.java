package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.configuration.ConfigurationSection;

import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

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

	/**
	 * Instantiates a new config.
	 */
	public Config() {
		super(new File(Main.plugin.getDataFolder(), "Config.yml"));
	}

	public String getMySqlHost() {
		return getData().getString("MySQL.Host", "");
	}

	public int getMySqlPort() {
		return getData().getInt("MySQL.Port");
	}
	
	public boolean getMySqlPreloadTable() {
		return getData().getBoolean("MySQL.PreLoadTable");
	}

	public int getMySqlMaxConnections() {
		return getData().getInt("MySQL.MaxConnections", 1);
	}

	public String getMySqlDatabase() {
		return getData().getString("MySQL.Database", "");
	}

	public String getMySqlUsername() {
		return getData().getString("MySQL.Username", "");
	}

	public String getMySqlPassword() {
		return getData().getString("MySQL.Password", "");
	}

	/**
	 * Allow un joined.
	 *
	 * @return true, if successful
	 */
	public boolean allowUnJoined() {
		return getData().getBoolean("AllowUnjoined");
	}

	public int getDelayBetweenUpdates() {
		return getData().getInt("DelayBetweenUpdates", 3);
	}

	/**
	 * Gets the all sites reward.
	 *
	 * @return the all sites reward
	 */
	public String getAllSitesRewardPath() {
		return "AllSites";
	}

	/**
	 * Gets the rewards.
	 *
	 * @return the rewards
	 */
	public String getAnySiteRewardsPath() {
		return "AnySiteRewards";

	}

	public boolean getLoadTopVoterMonthly() {
		return getData().getBoolean("LoadTopVoter.Monthly", true);
	}

	public boolean getLoadTopVoterWeekly() {
		return getData().getBoolean("LoadTopVoter.Weekly");
	}

	public boolean getLoadTopVoterDaily() {
		return getData().getBoolean("LoadTopVoter.Daily");
	}

	/**
	 * Gets the auto create vote sites.
	 *
	 * @return the auto create vote sites
	 */
	public boolean getAutoCreateVoteSites() {
		return getData().getBoolean("AutoCreateVoteSites");
	}
	
	public boolean getDisableNoServiceSiteMessage() {
		return getData().getBoolean("DisableNoServiceSiteMessage");
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
	 * Gets the broad cast votes enabled.
	 *
	 * @return the broad cast votes enabled
	 */
	public boolean getBroadCastVotesEnabled() {
		return getData().getBoolean("BroadcastVote");
	}

	public boolean getCommandsUseGUILast() {
		return getData().getBoolean("Commands.UseGUI.Last", true);
	}

	public boolean getCommandsUseGUINext() {
		return getData().getBoolean("Commands.UseGUI.Next", true);
	}

	public boolean getCommandsUseGUIToday() {
		return getData().getBoolean("Commands.UseGUI.Today", true);
	}

	public boolean getCommandsUseGUITopVoter() {
		return getData().getBoolean("Commands.UseGUI.TopVoter", true);
	}

	public boolean getCommandsUseGUITotal() {
		return getData().getBoolean("Commands.UseGUI.Total", true);
	}

	public boolean getCommandsUseGUIVote() {
		return getData().getBoolean("Commands.UseGUI.Vote", true);
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

	/**
	 * Gets the daily award rewards path
	 *
	 * @param pos
	 *            the pos
	 * @return the daily award rewards
	 */
	public String getDailyAwardRewardsPath(int pos) {
		return "DailyAwards." + pos + ".Rewards";
	}

	/**
	 * Gets the daily awards enabled.
	 *
	 * @return the daily awards enabled
	 */
	public boolean getDailyAwardsEnabled() {
		return getData().getBoolean("EnableDailyAwards");
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
	 * Gets the debug enabled.
	 *
	 * @return the debug enabled
	 */
	public boolean getDebugEnabled() {
		return getData().getBoolean("Debug");
	}

	/**
	 * Gets the debug info ingame.
	 *
	 * @return the debug info ingame
	 */
	public boolean getDebugInfoIngame() {
		return getData().getBoolean("DebugInfoIngame");
	}

	/**
	 * Gets the first vote rewards.
	 *
	 * @return the first vote rewards
	 */
	public String getFirstVoteRewardsPath() {
		return "FirstVote";
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
	 * Gets the commands vote last line.
	 *
	 * @return the commands vote last line
	 */
	public String getFormatCommandsVoteLastLine() {
		return getData().getString("Format.Commands.Vote.Last.Line", "&3%SiteName%: &6%time%");
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

	/**
	 * Gets the commands vote total all line.
	 *
	 * @return the commands vote total all line
	 */
	public String getFormatCommandsVoteTotalAllLine() {
		return getData().getString("Format.Commands.Vote.TotalAll.Line", "&3%SiteName% &6%Total%");

	}

	/**
	 * Gets the commands vote total all title.
	 *
	 * @return the commands vote total all title
	 */
	public String getFormatCommandsVoteTotalAllTitle() {
		return getData().getString("Format.Commands.Vote.TotalAll.Title", "&3&lAll Votes Total:");

	}

	/**
	 * Gets the commands vote total all total.
	 *
	 * @return the commands vote total all total
	 */
	public String getFormatCommandsVoteTotalAllTotal() {
		return getData().getString("Format.Commands.Vote.TotalAll.Total", "&3&lTotal: &6&l%Totals%");
	}

	/**
	 * Gets the commands vote total line.
	 *
	 * @return the commands vote total line
	 */
	public String getFormatCommandsVoteTotalLine() {
		return getData().getString("Format.Commands.Vote.Total.Line", "&3%SiteName%: &6%Total%");
	}

	/**
	 * Gets the commands vote total title.
	 *
	 * @return the commands vote total title
	 */
	public String getFormatCommandsVoteTotalTitle() {
		return getData().getString("Format.Commands.Vote.Total.Title", "&3&l%player% Total Votes:");
	}

	/**
	 * Gets the commands vote total total.
	 *
	 * @return the commands vote total total
	 */
	public String getFormatCommandsVoteTotalTotal() {
		return getData().getString("Format.Commands.Vote.Total.Total", "&3&lTotal: &6&l%Totals%");
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

	/**
	 * Gets the page size.
	 *
	 * @return the page size
	 */
	public int getFormatPageSize() {
		return 10;
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

	/**
	 * Gets the top voter reward msg.
	 *
	 * @return the top voter reward msg
	 */
	public String getFormatTopVoterRewardMsg() {
		return getData().getString("Format.TopVoterAwardMsg",
				"&aYou came in %place% in top voters of the month! Here is an award!");

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

	public int getIdentifierCost(String identifier) {
		return getData().getInt("Shop." + identifier + ".Cost");
	}

	public String getIdentifierFromSlot(int slot) {
		for (String identifier : getIdentifiers()) {
			if (getIdentifierSlot(identifier) == slot) {
				return identifier;
			}
		}
		return null;
	}

	public int getIdentifierItemAmount(String identifier) {
		return getData().getInt("Shop." + identifier + ".Item.Amount");
	}

	public String getIdentifierRewardsPath(String identifier) {
		return "Shop." + identifier + ".Rewards";

	}

	public Set<String> getIdentifiers() {
		return getData().getConfigurationSection("Shop").getKeys(false);
	}

	public ConfigurationSection getIdentifierSection(String identifier) {
		return getData().getConfigurationSection("Shop." + identifier);
	}

	public int getIdentifierSlot(String identifier) {
		return getData().getInt("Shop." + identifier + ".Slot");
	}

	/**
	 * Gets the log debug to file.
	 *
	 * @return the log debug to file
	 */
	public boolean getLogDebugToFile() {
		return getData().getBoolean("LogDebugToFile", true);
	}

	/**
	 * Gets the log votes to file.
	 *
	 * @return the log votes to file
	 */
	public boolean getLogVotesToFile() {
		return getData().getBoolean("LogVotesToFile");
	}

	public boolean getMilestoneResetMonthly(int milestones) {
		return getData().getBoolean("MileStones." + milestones + ".ResetMonthly");
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

	/**
	 * Gets the monthly award rewards.
	 *
	 * @param pos
	 *            the pos
	 * @return the monthly award rewards
	 */
	public String getMonthlyAwardRewardsPath(int pos) {
		return "MonthlyAwards." + pos + ".Rewards";
	}

	/**
	 * Gets the monthly awards enabled.
	 *
	 * @return the monthly awards enabled
	 */
	public boolean getMonthlyAwardsEnabled() {
		return getData().getBoolean("EnableMonthlyAwards");
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
	 * Gets the send scoreboards.
	 *
	 * @return the send scoreboards
	 */
	public boolean getSendScoreboards() {
		return getData().getBoolean("SendScoreboards");
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

	public int getUserVotesRequired() {
		return getData().getInt("VoteParty.UserVotesRequired");
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
		return (ArrayList<String>) getData().getList("GUI.VoteGUI." + slot + ".Lore", new ArrayList<String>());
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
		return getData().getInt("GUI.VoteGUI." + slot + ".Slot");
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

	public boolean getVotePartyResetEachDay() {
		return getData().getBoolean("VoteParty.ResetEachDay");
	}

	/**
	 * Gets the vote party rewards.
	 *
	 * @return the vote party rewards
	 */
	public String getVotePartyRewardsPath() {
		return "VoteParty.Rewards";
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

	public ConfigurationSection getVoteSiteItemSection(String site) {
		String siteName = site.replace(".", "-");
		return getData().getConfigurationSection("GUI.VoteReward." + siteName);
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

	public ConfigurationSection getVoteURLAlreadyVotedItemSection() {
		return getData().getConfigurationSection("GUI.VoteURL.AlreadyVotedItem");
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

	/**
	 * Gets the weekly award rewards.
	 *
	 * @param pos
	 *            the pos
	 * @return the weekly award rewards
	 */
	public String getWeeklyAwardRewardsPath(int pos) {
		return "WeeklyAwards." + pos + ".Rewards";
	}

	/**
	 * Gets the weekly awards enabled.
	 *
	 * @return the weekly awards enabled
	 */
	public boolean getWeeklyAwardsEnabled() {
		return getData().getBoolean("EnableWeeklyAwards");
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

	@Override
	public void onFileCreation() {
		plugin.saveResource("Config.yml", true);
	}

	/**
	 * Sets the allow un joined.
	 *
	 * @param value
	 *            the new allow un joined
	 */
	public void setAllowUnJoined(boolean value) {
		getData().set("AllowUnjoined", value);
		saveData();
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
	 * Sets the broadcast vote enabled.
	 *
	 * @param value
	 *            the new broadcast vote enabled
	 */
	public void setBroadcastVoteEnabled(boolean value) {
		getData().set("BroadcastVote", value);
		saveData();
	}

	/**
	 * Sets the debug enabled.
	 *
	 * @param value
	 *            the new debug enabled
	 */
	public void setDebugEnabled(boolean value) {
		getData().set("Debug", value);
		saveData();
	}

	/**
	 * Sets the debug info ingame.
	 *
	 * @param value
	 *            the new debug info ingame
	 */
	public void setDebugInfoIngame(boolean value) {
		getData().set("DebugInfoIngame", value);
		saveData();
	}

	/**
	 * Sets the top voter awards enabled.
	 *
	 * @param value
	 *            the new top voter awards enabled
	 */
	public void setTopVoterAwardsEnabled(boolean value) {
		getData().set("TopVoterAwards", value);
		saveData();
	}

	public void setVoteRemindingEnabled(boolean value) {
		getData().set("VoteReminding.Enabled", value);
		saveData();
	}

	public void setVoteRemindingRemindDelay(int value) {
		getData().set("VoteReminding.RemindDelay", value);
		saveData();
	}

	public void setVoteRemindingRemindOnLogin(boolean value) {
		getData().set("VoteReminding.RemindOnLogin", value);
		saveData();
	}

	public void setVoteRemindingRemindOnlyOnce(boolean value) {
		getData().set("VoteReminding.RemindOnlyOnce", value);
		saveData();
	}

	public void setVoteRemindingRewards(ArrayList<String> value) {
		getData().set("VoteReminding.Rewards", value);
		saveData();
	}

	public String getDataStorage() {
		return getData().getString("DataStorage", "FLAT");
	}

	public boolean getCheckOnWorldChange() {
		return getData().getBoolean("CheckOnWorldChange");
	}

	public boolean getPreloadUsers() {
		return getData().getBoolean("PreLoadUsers");
	}

	public boolean getLoadTopVoterAllTime() {
		return getData().getBoolean("LoadTopVoter.AllTime", true);
	}

	public String getVoteTopDefault() {
		return getData().getString("VoteTopDefault", "Monthly");
	}

}
