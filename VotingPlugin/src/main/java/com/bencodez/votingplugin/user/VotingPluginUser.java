package com.bencodez.votingplugin.user;

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.stream.Collectors;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.data.DataValue;
import com.bencodez.simpleapi.sql.data.DataValueInt;
import com.bencodez.simpleapi.time.ParsedDuration;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.events.PlayerReceivePointsEvent;
import com.bencodez.votingplugin.events.PlayerSpecialRewardEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.events.SpecialRewardType;
import com.bencodez.votingplugin.proxy.BungeeMessageData;
import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;
import com.bencodez.votingplugin.votesites.VoteSite;

/**
 * The Class VotingPluginUser. This class represents a user in the VotingPlugin
 * system. It extends the AdvancedCoreUser class and provides additional
 * functionality specific to the VotingPlugin.
 */
public class VotingPluginUser extends com.bencodez.advancedcore.api.user.AdvancedCoreUser {

	/** The plugin instance. */
	private VotingPluginMain plugin;

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin the plugin instance
	 * @param user   the AdvancedCoreUser instance
	 */
	public VotingPluginUser(VotingPluginMain plugin, AdvancedCoreUser user) {
		super(plugin, user);
		this.plugin = plugin;
	}

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin the plugin instance
	 * @param player the player instance
	 * @deprecated Use {@link #VotingPluginUser(VotingPluginMain, AdvancedCoreUser)}
	 *             instead.
	 */
	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, Player player) {
		super(plugin, player);
		this.plugin = plugin;
	}

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin     the plugin instance
	 * @param playerName the player name
	 * @deprecated Use {@link #VotingPluginUser(VotingPluginMain, AdvancedCoreUser)}
	 *             instead.
	 */
	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, String playerName) {
		super(plugin, playerName);
		this.plugin = plugin;
	}

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin the plugin instance
	 * @param uuid   the UUID of the player
	 * @deprecated Use {@link #VotingPluginUser(VotingPluginMain, AdvancedCoreUser)}
	 *             instead.
	 */
	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid) {
		super(plugin, uuid);
		this.plugin = plugin;
	}

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin   the plugin instance
	 * @param uuid     the UUID of the player
	 * @param loadName whether to load the player name
	 * @deprecated Use {@link #VotingPluginUser(VotingPluginMain, AdvancedCoreUser)}
	 *             instead.
	 */
	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid, boolean loadName) {
		super(plugin, uuid, loadName);
		this.plugin = plugin;
	}

	/**
	 * Instantiates a new VotingPluginUser.
	 *
	 * @param plugin     the plugin instance
	 * @param uuid       the UUID of the player
	 * @param playerName the player name
	 * @deprecated Use {@link #VotingPluginUser(VotingPluginMain, AdvancedCoreUser)}
	 *             instead.
	 */
	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid, String playerName) {
		super(plugin, uuid, playerName);
		this.plugin = plugin;
	}

	/**
	 * Adds one to the all-time total votes.
	 */
	public void addAllTimeTotal() {
		setAllTimeTotal(getAllTimeTotal() + 1);
	}

	/**
	 * Adds one to the daily vote streak.
	 */
	public void addDayVoteStreak() {
		setDayVoteStreak(getDayVoteStreak() + 1);
	}

	/**
	 * Adds one to the monthly total votes.
	 */
	public void addMonthTotal() {
		setMonthTotal(getMonthTotal() + 1);
	}

	/**
	 * Adds one to the monthly vote streak.
	 */
	public void addMonthVoteStreak() {
		setMonthVoteStreak(getMonthVoteStreak() + 1);
	}

	/**
	 * Adds an offline vote for the specified vote site.
	 *
	 * @param voteSiteName the name of the vote site
	 */
	public void addOfflineVote(String voteSiteName) {
		ArrayList<String> offlineVotes = getOfflineVotes();
		offlineVotes.add(voteSiteName);
		setOfflineVotes(offlineVotes);
	}

	/**
	 * Adds points to the user based on the configuration.
	 */
	public void addPoints() {
		int points = plugin.getConfigFile().getPointsOnVote();
		if (points != 0) {
			addPoints(points);
		}
		if (plugin.getConfigFile().getLimitVotePoints() > 0) {
			if (getPoints() > plugin.getConfigFile().getLimitVotePoints()) {
				setPoints(plugin.getConfigFile().getLimitVotePoints());
			}
		}
	}

	/**
	 * Adds the specified number of points to the user.
	 *
	 * @param value the number of points to add
	 * @return the current total points
	 */
	public int addPoints(int value) {
		return addPoints(value, false);
	}

	/**
	 * Adds the specified number of points to the user, optionally asynchronously.
	 *
	 * @param value the number of points to add
	 * @param async whether to add the points asynchronously
	 * @return the current total points
	 */
	public synchronized int addPoints(int value, boolean async) {
		PlayerReceivePointsEvent event = new PlayerReceivePointsEvent(this, value);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return getPoints();
		}
		int newTotal = getPoints() + event.getPoints();
		setPoints(newTotal, async);
		return newTotal;
	}

	/**
	 * Adds one to the total votes.
	 */
	public void addTotal() {
		addMonthTotal();
		addAllTimeTotal();
	}

	/**
	 * Adds one to the daily total votes.
	 */
	public void addTotalDaily() {
		setDailyTotal(getDailyTotal() + 1);
	}

	/**
	 * Adds one to the weekly total votes.
	 */
	public void addTotalWeekly() {
		setWeeklyTotal(getWeeklyTotal() + 1);
	}

	/**
	 * Adds one to the weekly vote streak.
	 */
	public void addWeekVoteStreak() {
		setWeekVoteStreak(getWeekVoteStreak() + 1);
	}

	/**
	 * Handles a bungee vote.
	 *
	 * @param service   the service name
	 * @param text      the bungee message data
	 * @param setTotals whether to set the totals
	 */
	public void bungeeVote(String service, BungeeMessageData text, boolean setTotals) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Bungee vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSiteManager().getVoteSite(service, true),
					getPlayerName(), service, true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setVotingPluginUser(this);
			plugin.getServer().getPluginManager().callEvent(voteEvent);
		}
	}

	/**
	 * Handles an online bungee vote.
	 *
	 * @param service   the service name
	 * @param text      the bungee message data
	 * @param setTotals whether to set the totals
	 */
	public void bungeeVoteOnline(String service, BungeeMessageData text, boolean setTotals) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Bungee online vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSiteManager().getVoteSite(service, true),
					getPlayerName(), service, true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setVotingPluginUser(this);
			plugin.getServer().getPluginManager().callEvent(voteEvent);
		}
	}

	/**
	 * Handles a plugin messaging bungee vote.
	 *
	 * @param service   the service name
	 * @param time      the vote time
	 * @param text      the bungee message data
	 * @param setTotals whether to set the totals
	 * @param wasOnline whether the player was online
	 * @param broadcast whether to broadcast the vote
	 * @param num       the vote number
	 */
	public void bungeeVotePluginMessaging(String service, long time, BungeeMessageData text, boolean setTotals,
			boolean wasOnline, boolean broadcast, int num) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Pluginmessaging vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSiteManager().getVoteSite(service, true),
					getPlayerName(), service, true);
			voteEvent.setBungee(true);
			voteEvent.setVotingPluginUser(this);
			voteEvent.setForceBungee(true);
			voteEvent.setTime(time);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setWasOnline(wasOnline);
			voteEvent.setBroadcast(broadcast);
			voteEvent.setVoteNumber(num);
			plugin.getServer().getPluginManager().callEvent(voteEvent);
		}
	}

	/**
	 * Checks if the user can vote on all sites.
	 *
	 * @return true, if the user can vote on all sites
	 */
	public boolean canVoteAll() {
		for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!voteSite.isHidden()) {
				boolean canVote = canVoteSite(voteSite);
				if (!canVote) {
					return false;
				}
			}
		}
		return true;
	}

	/**
	 * Checks if the user can vote on any site.
	 *
	 * @return true, if the user can vote on any site
	 */
	public boolean canVoteAny() {
		for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!voteSite.isIgnoreCanVote() && !voteSite.isHidden()) {
				boolean canVote = canVoteSite(voteSite);
				if (canVote) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Checks if the user can vote on the specified site.
	 *
	 * @param voteSite the vote site
	 * @return true, if the user can vote on the site
	 */
	public boolean canVoteSite(VoteSite voteSite) {
		long time = getTime(voteSite);
		if (time == 0) {
			return true;
		}
		try {
			LocalDateTime now = plugin.getTimeChecker().getTime();
			LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
					.plusHours(plugin.getOptions().getTimeHourOffSet());

			if (!voteSite.isVoteDelayDaily()) {
				ParsedDuration voteDelay = voteSite.getVoteDelay();

				// Preserve old behavior: if delay is 0, you can never vote again (unless daily
				// reset mode)
				if (voteDelay == null || voteDelay.isEmpty() || voteDelay.getMillis() == 0L) {
					return false;
				}

				LocalDateTime nextVote = lastVote.plus(Duration.ofMillis(voteDelay.getMillis()));
				return now.isAfter(nextVote);
			}

			// Daily reset logic unchanged
			LocalDateTime resetTime = lastVote.withHour(voteSite.getVoteDelayDailyHour()).withMinute(0).withSecond(0);
			LocalDateTime resetTimeTomorrow = resetTime.plusHours(24);

			if (lastVote.isBefore(resetTime)) {
				return now.isAfter(resetTime);
			} else {
				return now.isAfter(resetTimeTomorrow);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	/**
	 * Checks if the user has voted on all sites.
	 *
	 * @return true, if the user has voted on all sites
	 */
	public boolean checkAllVotes() {
		VotingPluginUser user = this;

		ArrayList<Integer> months = new ArrayList<>();
		ArrayList<Integer> days = new ArrayList<>();

		for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (voteSite.isEnabled() && !voteSite.isHidden()) {
				long time = user.getTime(voteSite);
				if (time == 0) {
					return false;
				}
				months.add(MiscUtils.getInstance().getMonthFromMili(time));
				days.add(MiscUtils.getInstance().getDayFromMili(time));
			}
		}

		// check months
		for (Integer month : months) {
			if (!months.get(0).equals(month)) {
				return false;
			}
		}

		// check days
		for (Integer day : days) {
			if (!days.get(0).equals(day)) {
				return false;
			}
		}

		return true;
	}

	/**
	 * Checks if the user has voted on almost all sites.
	 *
	 * @return true, if the user has voted on almost all sites
	 */
	public boolean checkAlmostAllVotes() {
		if (getSitesNotVotedOn() <= 1) {
			return true;
		}
		return false;
	}

	/**
	 * Checks the day vote streak and updates it if necessary.
	 *
	 * @param forceBungee whether to force bungee
	 */
	public void checkDayVoteStreak(boolean forceBungee) {
		if (!voteStreakUpdatedToday(LocalDateTime.now())) {
			if (!plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage() || hasPercentageTotal(
					TopVoter.Daily, plugin.getSpecialRewardsConfig().getVoteStreakRequirementDay(), null)) {
				plugin.extraDebug("Adding day vote streak to " + getUUID() + " "
						+ plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage() + " "
						+ hasPercentageTotal(TopVoter.Daily,
								plugin.getSpecialRewardsConfig().getVoteStreakRequirementDay(), null));
				addDayVoteStreak();
				plugin.getSpecialRewards().checkVoteStreak(null, this, "Day", forceBungee);
				setDayVoteStreakLastUpdate(System.currentTimeMillis());
			}
		}
	}

	/**
	 * Clears the offline votes.
	 */
	public void clearOfflineVotes() {
		setOfflineVotes(new ArrayList<>());
		setOfflineRewards(new ArrayList<>());
	}

	/**
	 * Clears the total votes for all top voter categories.
	 */
	public void clearTotals() {
		for (TopVoter top : TopVoter.values()) {
			resetTotals(top);
		}
	}

	/**
	 * Gets the all-time total votes.
	 *
	 * @return the all-time total votes
	 * @deprecated Use getTotal(TopVoter.AllTime) when able instead
	 */
	@Deprecated
	public int getAllTimeTotal() {
		return getTotal(TopVoter.AllTime);
	}

	/**
	 * Gets the best day vote streak.
	 *
	 * @return the best day vote streak
	 */
	public int getBestDayVoteStreak() {
		return getData().getInt("BestDayVoteStreak");
	}

	/**
	 * Gets the best month vote streak.
	 *
	 * @return the best month vote streak
	 */
	public int getBestMonthVoteStreak() {
		return getData().getInt("BestMonthVoteStreak");
	}

	/**
	 * Gets the best week vote streak.
	 *
	 * @return the best week vote streak
	 */
	public int getBestWeekVoteStreak() {
		return getData().getInt("BestWeekVoteStreak");
	}

	/**
	 * Checks if the cooldown check is enabled.
	 *
	 * @return true, if the cooldown check is enabled
	 */
	public boolean getCoolDownCheck() {
		return getData().getBoolean(getCoolDownCheckPath());
	}

	/**
	 * Gets the path for the cooldown check.
	 *
	 * @return the cooldown check path
	 */
	public String getCoolDownCheckPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage();
		}
		return "CoolDownCheck";
	}

	/**
	 * Checks if the cooldown check is enabled for a specific vote site.
	 *
	 * @param site the vote site
	 * @return true, if the cooldown check is enabled for the site
	 */
	public boolean getCoolDownCheckSite(VoteSite site) {
		HashMap<String, Boolean> coolDownChecks = getCoolDownCheckSiteList();
		if (coolDownChecks.containsKey(site.getKey())) {
			return coolDownChecks.get(site.getKey()).booleanValue();
		}
		return false;
	}

	/**
	 * Gets the list of cooldown checks for all vote sites.
	 *
	 * @return the list of cooldown checks for all vote sites
	 */
	public HashMap<String, Boolean> getCoolDownCheckSiteList() {
		HashMap<String, Boolean> coolDownChecks = new HashMap<>();
		ArrayList<String> coolDownCheck = getData().getStringList(getCoolDownCheckSitePath());
		for (String str : coolDownCheck) {
			String[] data = str.split("//");
			if (data.length > 1 && plugin.getVoteSiteManager().hasVoteSite(data[0])) {
				VoteSite site = plugin.getVoteSiteManager().getVoteSite(data[0], true);
				if (site != null) {
					Boolean b = Boolean.valueOf(data[1]);
					coolDownChecks.put(site.getKey(), b);
				}
			}
		}
		return coolDownChecks;
	}

	/**
	 * Gets the path for the cooldown check site list.
	 *
	 * @return the cooldown check site list path
	 */
	public String getCoolDownCheckSitePath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServerNameStorage() + "_Sites";
		}
		return "CoolDownCheck" + "_Sites";
	}

	/**
	 * Gets the daily total votes.
	 *
	 * @return the daily total votes
	 * @deprecated Use getTotal(TopVoter.Daily) instead
	 */
	@Deprecated
	public int getDailyTotal() {
		return getTotal(TopVoter.Daily);
	}

	/**
	 * Gets the day vote streak.
	 *
	 * @return the day vote streak
	 */
	public int getDayVoteStreak() {
		return getData().getInt("DayVoteStreak");
	}

	/**
	 * Gets the last update time for the day vote streak.
	 *
	 * @return the last update time for the day vote streak
	 */
	public long getDayVoteStreakLastUpdate() {
		String str = getData().getString("DayVoteStreakLastUpdate");
		if (str == null || str.isEmpty() || str.equals("null")) {
			return 0;
		}
		try {
			return Long.parseLong(str);
		} catch (NumberFormatException e) {
			return 0;
		}
	}

	/**
	 * Checks if the broadcast is disabled.
	 *
	 * @return true if the broadcast is disabled, false otherwise
	 */
	public boolean getDisableBroadcast() {
		return getUserData().getBoolean("DisableBroadcast");
	}

	/**
	 * Gets the day when the user has gotten all sites.
	 *
	 * @return the day when the user has gotten all sites
	 */
	public int getGottenAllSitesDay() {
		return getData().getInt(plugin.getVotingPluginUserManager().getGottenAllSitesDayPath(), 0);
	}

	/**
	 * Gets the day when the user has gotten almost all sites.
	 *
	 * @return the day when the user has gotten almost all sites
	 */
	public int getGottenAlmostAllSitesDay() {
		return getData().getInt(plugin.getVotingPluginUserManager().getGottenAlmostAllSitesDayPath(), 0);
	}

	/**
	 * Gets the highest daily total votes.
	 *
	 * @return the highest daily total votes
	 */
	public int getHighestDailyTotal() {
		return getData().getInt("HighestDailyTotal");
	}

	/**
	 * Gets the highest monthly total votes.
	 *
	 * @return the highest monthly total votes
	 */
	public int getHighestMonthlyTotal() {
		return getData().getInt("HighestMonthlyTotal");
	}

	/**
	 * Gets the highest weekly total votes.
	 *
	 * @return the highest weekly total votes
	 */
	public int getHighestWeeklyTotal() {
		return getData().getInt("HighestWeeklyTotal");
	}

	/**
	 * Gets the total votes for the last month.
	 *
	 * @return the total votes for the last month
	 */
	public int getLastMonthTotal() {
		return getData().getInt("LastMonthTotal");
	}

	/**
	 * Gets the last votes for each vote site.
	 *
	 * @return a map of vote sites and the last vote time
	 */
	public HashMap<VoteSite, Long> getLastVotes() {
		HashMap<VoteSite, Long> lastVotes = new HashMap<>();
		ArrayList<String> LastVotesList = getUserData().getStringList("LastVotes");
		for (String str : LastVotesList) {
			String[] data = str.split("//");
			if (data.length > 1 && plugin.getVoteSiteManager().hasVoteSite(data[0])) {
				VoteSite site = plugin.getVoteSiteManager().getVoteSite(data[0], true);
				if (site != null) {
					long time = 0;
					try {
						time = Long.parseLong(data[1]);
					} catch (NumberFormatException e) {
						time = 0;
						plugin.debug("Not long: " + data[1]);
					}
					lastVotes.put(site, time);
				}
			}
		}
		return lastVotes;
	}

	/**
	 * Gets the time of the last vote.
	 *
	 * @return the time of the last vote
	 */
	public Long getLastVoteTime() {
		Long time = Long.valueOf(0);
		for (Long value : getLastVotes().values()) {
			if (value.longValue() > time) {
				time = value;
			}
		}
		return time;
	}

	/**
	 * Gets the last vote time for a specific vote site.
	 *
	 * @param voteSite the vote site
	 * @return the last vote time for the vote site
	 */
	public long getLastVoteTimer(VoteSite voteSite) {
		HashMap<VoteSite, Long> times = getLastVotes();
		if (times.containsKey(voteSite)) {
			return times.get(voteSite).longValue();
		}
		return 0;
	}

	/**
	 * Gets the last vote times sorted in descending order.
	 *
	 * @return a map of vote sites and the last vote times sorted in descending
	 *         order
	 */
	public HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		LinkedHashMap<VoteSite, Long> times = new LinkedHashMap<>();

		for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			times.put(voteSite, getTime(voteSite));
		}
		LinkedHashMap<VoteSite, Long> sorted = new LinkedHashMap<>(
				times.entrySet().stream().sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
						.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
		return sorted;
	}

	/**
	 * Gets the total votes for the month.
	 *
	 * @return the total votes for the month
	 * @deprecated Use getTotal(TopVoter.Monthly) instead
	 */
	@Deprecated
	public int getMonthTotal() {
		return getTotal(TopVoter.Monthly);
	}

	/**
	 * Gets the month vote streak.
	 *
	 * @return the month vote streak
	 */
	public int getMonthVoteStreak() {
		return getData().getInt("MonthVoteStreak");
	}

	/**
	 * Gets the next time all sites are available for voting.
	 *
	 * @return the next time all sites are available for voting
	 */
	public long getNextTimeAllSitesAvailable() {
		long longest = 0;
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			long seconds = voteNextDurationTime(site);
			if (seconds > longest) {
				longest = seconds;
			}
		}

		return longest;
	}

	/**
	 * Gets the next time the first site is available for voting.
	 *
	 * @return the next time the first site is available for voting
	 */
	public long getNextTimeFirstSiteAvailable() {
		long shortest = 0;
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!canVoteSite(site)) {
				long seconds = voteNextDurationTime(site);
				if (shortest == 0 || seconds < shortest) {
					shortest = seconds;
				}
			}
		}

		return shortest;
	}

	/**
	 * Gets the number of offline votes for the specified vote site.
	 *
	 * @param site the vote site
	 * @return the number of offline votes for the specified vote site
	 */
	public int getNumberOfOfflineVotes(VoteSite site) {
		ArrayList<String> offlineVotes = getOfflineVotes();
		int num = 0;
		for (String str : offlineVotes) {
			if (str.equals(site.getKey())) {
				num++;
			}
		}
		return num;
	}

	/**
	 * Gets the list of offline votes.
	 *
	 * @return the list of offline votes
	 */
	public ArrayList<String> getOfflineVotes() {
		return getUserData().getStringList("OfflineVotes");
	}

	/**
	 * Gets the points of the user.
	 *
	 * @return the points of the user
	 */
	public int getPoints() {
		return getUserData().getInt(getPointsPath());
	}

	/**
	 * Gets the path for the points.
	 *
	 * @return the points path
	 */
	public String getPointsPath() {
		if (plugin.getBungeeSettings().isPerServerPoints()) {
			return plugin.getBungeeSettings().getServerNameStorage() + "_Points";
		}
		return "Points";
	}

	/**
	 * Gets the number of sites not voted on.
	 *
	 * @return the number of sites not voted on
	 */
	public int getSitesNotVotedOn() {
		int amount = 0;
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!site.isHidden()) {
				if (site.getPermissionToView().isEmpty() || hasPermission(site.getPermissionToView(), false)) {
					if (canVoteSite(site)) {
						amount++;
					}
				}
			}
		}
		return amount;
	}

	public int getTotalNumberOfSites() {
		int amount = 0;
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!site.isHidden()) {
				if (site.getPermissionToView().isEmpty() || hasPermission(site.getPermissionToView(), false)) {
					amount++;
				}
			}
		}
		return amount;
	}

	public int getSitesVotedOn() {
		int amount = 0;
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!canVoteSite(site)) {
				amount++;
			}
		}
		return amount;
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite the vote site
	 * @return the time
	 */
	public long getTime(VoteSite voteSite) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		if (lastVotes.containsKey(voteSite)) {
			return lastVotes.get(voteSite);
		}
		return 0;
	}

	/**
	 * Gets the top voter player.
	 *
	 * @return the top voter player
	 */
	public TopVoterPlayer getTopVoterPlayer() {
		return new TopVoterPlayer(UUID.fromString(getUUID()), getPlayerName(), getLastOnline());
	}

	/**
	 * Gets the total votes for the specified top voter category.
	 *
	 * @param top the top voter category
	 * @return the total votes for the specified top voter category
	 */
	public int getTotal(TopVoter top) {
		switch (top) {
		case AllTime:
			return getUserData().getInt("AllTimeTotal");
		case Daily:
			return getUserData().getInt("DailyTotal");
		case Monthly:
			if (plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()) {
				return getData().getInt(plugin.getVotingPluginUserManager().getMonthTotalsWithDatePath());
			}
			return getData().getInt("MonthTotal");
		case Weekly:
			return getUserData().getInt("WeeklyTotal");
		default:
			break;
		}
		return 0;
	}

	/**
	 * Gets the total votes for the specified top voter category at a specific time.
	 *
	 * @param top    the top voter category
	 * @param atTime the specific time
	 * @return the total votes for the specified top voter category at the specific
	 *         time
	 */
	public int getTotal(TopVoter top, LocalDateTime atTime) {
		switch (top) {
		case AllTime:
			return getUserData().getInt("AllTimeTotal");
		case Daily:
			return getUserData().getInt("DailyTotal");
		case Monthly:
			if (plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()) {
				return getData().getInt(plugin.getVotingPluginUserManager().getMonthTotalsWithDatePath(atTime));
			}
			return getData().getInt("MonthTotal");
		case Weekly:
			return getUserData().getInt("WeeklyTotal");
		default:
			break;
		}
		return 0;
	}

	/**
	 * Gets the number of votes for the vote party.
	 *
	 * @return the number of votes for the vote party
	 */
	public int getVotePartyVotes() {
		return getUserData().getInt("VotePartyVotes");
	}

	/**
	 * Gets the vote shop identifier limit.
	 *
	 * @param identifier the identifier for the vote shop
	 * @return the vote shop identifier limit
	 */
	public int getVoteShopIdentifierLimit(String identifier) {
		return getData().getInt("VoteShopLimit" + identifier);
	}

	/**
	 * Gets the weekly total votes.
	 *
	 * @return the weekly total votes
	 * @deprecated Use getTotal(TopVoter.Weekly) instead
	 */
	@Deprecated
	public int getWeeklyTotal() {
		return getTotal(TopVoter.Weekly);
	}

	/**
	 * Gets the week vote streak.
	 *
	 * @return the week vote streak
	 */
	public int getWeekVoteStreak() {
		return getData().getInt("WeekVoteStreak");
	}

	/**
	 * Gives the daily top voter award.
	 *
	 * @param place the place of the top voter
	 * @param path  the path to the reward configuration
	 */
	public void giveDailyTopVoterAward(int place, String path) {
		SpecialRewardType type = SpecialRewardType.TOPVOTER;
		type.setType("Daily");
		type.setAmount(1);
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(this, type, null);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getDailyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
				.withPlaceHolder("topvoter", "Daily").withPlaceHolder("votes", "" + getTotal(TopVoter.Daily))
				.setOnline(isOnline()).send(this);
	}

	/**
	 * Gives the monthly top voter award.
	 *
	 * @param place the place of the top voter
	 * @param path  the path to the reward configuration
	 */
	public void giveMonthlyTopVoterAward(int place, String path) {
		SpecialRewardType type = SpecialRewardType.TOPVOTER;
		type.setType("Monthly");
		type.setAmount(1);
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(this, type, null);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getMonthlyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
				.withPlaceHolder("topvoter", "Monthly").withPlaceHolder("votes", "" + getTotal(TopVoter.Monthly))
				.setOnline(isOnline()).send(this);
	}

	/**
	 * Gives the weekly top voter award.
	 *
	 * @param place the place of the top voter
	 * @param path  the path to the reward configuration
	 */
	public void giveWeeklyTopVoterAward(int place, String path) {
		SpecialRewardType type = SpecialRewardType.TOPVOTER;
		type.setType("Weekly");
		type.setAmount(1);
		PlayerSpecialRewardEvent event = new PlayerSpecialRewardEvent(this, type, null);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getWeeklyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
				.withPlaceHolder("topvoter", "Weekly").withPlaceHolder("votes", "" + getTotal(TopVoter.Weekly))
				.setOnline(isOnline()).send(this);
	}

	/**
	 * Gets how many unique vote sites this user has voted on today.
	 *
	 * Uses existing LastVotes data (no storage). A site counts if its last-vote
	 * timestamp falls on "today" using VotingPlugin's time offset.
	 *
	 * @return number of unique sites voted on today
	 */
	public long getUniqueVoteSitesToday() {
		LocalDateTime now = plugin.getTimeChecker().getTime();
		LocalDate today = now.toLocalDate();

		long count = 0;

		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (site == null || !site.isEnabled() || site.isHidden()) {
				continue;
			}

			long time = getTime(site);
			if (time <= 0) {
				continue;
			}

			// Match the same offset handling as canVoteSite()
			LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
					.plusHours(plugin.getOptions().getTimeHourOffSet());

			if (lastVote.toLocalDate().equals(today)) {
				count++;
			}
		}

		return count;
	}

	/**
	 * Checks if the user has a percentage of the total votes.
	 *
	 * @param top        the top voter category
	 * @param percentage the percentage of the total votes
	 * @param time       the specific time
	 * @return true if the user has the percentage of the total votes, false
	 *         otherwise
	 */
	public boolean hasPercentageTotal(TopVoter top, double percentage, LocalDateTime time) {
		int total = getTotal(top, time);
		switch (top) {
		case Daily:
			return (double) total / (double) plugin.getVoteSiteManager().getVoteSitesEnabled().size()
					* 100 > percentage;
		case Monthly:
			return total / ((double) plugin.getVoteSiteManager().getVoteSitesEnabled().size()
					* time.getMonth().length(false)) * 100 > percentage;
		case Weekly:
			return total / ((double) plugin.getVoteSiteManager().getVoteSitesEnabled().size() * 7) * 100 > percentage;
		default:
			return false;
		}
	}

	/**
	 * Checks if the user is reminded.
	 *
	 * @return true if the user is reminded, false otherwise
	 */
	public boolean isReminded() {
		return getUserData().getBoolean("Reminded");
	}

	/**
	 * Checks if the user is ignored for top voter.
	 *
	 * @return true if the user is ignored for top voter, false otherwise
	 */
	public boolean isTopVoterIgnore() {
		return getUserData().getBoolean("TopVoterIgnore");
	}

	/**
	 * Sends a login message to the user.
	 */
	public void loginMessage() {
		if (plugin.getConfigFile().isVoteRemindingRemindOnLogin()) {
			plugin.getVoteReminding().runRemindLogin(this);
		}
	}

	/**
	 * Gives login rewards to the user.
	 */
	public void loginRewards() {
		if (plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(), "LoginRewards")) {
			new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), "LoginRewards").send(this);
		}
	}

	/**
	 * Gives logout rewards to the user.
	 */
	public void logoutRewards() {
		if (plugin.getRewardHandler().hasRewards(plugin.getSpecialRewardsConfig().getData(), "LogoutRewards")) {
			new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), "LogoutRewards").send(this);
		}
	}

	/**
	 * Merges the provided data with the current data.
	 *
	 * @param toAdd the data to add
	 */
	public void mergeData(HashMap<String, DataValue> toAdd) {
		HashMap<String, DataValue> currentData = getData().getValues();
		HashMap<String, DataValue> newData = new HashMap<>();

		for (TopVoter top : TopVoter.values()) {
			if (toAdd.containsKey(top.getColumnName()) && currentData.containsKey(top.getColumnName())) {
				newData.put(top.getColumnName(), new DataValueInt(
						currentData.get(top.getColumnName()).getInt() + toAdd.get(top.getColumnName()).getInt()));
			}
		}

		if (newData.size() > 0) {
			getData().setValues(newData);
		}
	}

	/**
	 * Broadcasts an offline vote.
	 *
	 * @param user          the user
	 * @param checkBungee   whether to check bungee
	 * @param numberOfVotes the number of votes
	 */
	public void offlineBroadcast(VotingPluginUser user, boolean checkBungee, int numberOfVotes) {
		if (plugin.getConfigFile().isFormatAlternateBroadcastEnabled()) {
			return;
		}
		if (!user.isVanished()) {
			String playerName = user.getPlayerName();
			if (plugin.getConfigFile().getVotingBroadcastBlacklist().contains(playerName)) {
				plugin.getLogger().info("Not broadcasting for " + playerName + ", in blacklist");
				return;
			}
			if (checkBungee && plugin.getBungeeSettings().isBungeeBroadcast()
					&& plugin.getBungeeSettings().isUseBungeecoord()) {
				if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.SOCKETS)) {
					plugin.getBungeeHandler().sendData("BroadcastOffline", "" + numberOfVotes, user.getPlayerName());
				} else if (plugin.getBungeeHandler().getMethod().equals(BungeeMethod.MYSQL)
						|| plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
					String uuid = user.getUUID();

					if (Bukkit.getOnlinePlayers().size() > 0) {
						plugin.getPluginMessaging().sendPluginMessage("VoteBroadcastOffline", uuid, "" + numberOfVotes);
					}
				}

			} else {
				String bc = MessageAPI.colorize(plugin.getConfigFile().getFormatOfflineBroadcast());
				HashMap<String, String> placeholders = new HashMap<>();
				placeholders.put("player", playerName);
				placeholders.put("nickname",
						(user.getPlayer() != null) ? user.getPlayer().getDisplayName() : user.getPlayerName());
				placeholders.put("numberofvotes", "" + numberOfVotes);
				bc = PlaceholderUtils.replacePlaceHolder(bc, placeholders);
				bc = PlaceholderUtils.replacePlaceHolders(user.getOfflinePlayer(), bc);
				ArrayList<Player> players = new ArrayList<>();
				for (Player p : Bukkit.getOnlinePlayers()) {
					if (!plugin.getVotingPluginUserManager().getVotingPluginUser(p).getDisableBroadcast()) {
						players.add(p);
					}
				}

				MiscUtils.getInstance().broadcast(bc, players);
			}
		} else {
			plugin.debug(user.getPlayerName() + " is vanished, not broadcasting");
		}
	}

	/**
	 * Processes offline votes.
	 */
	public void offVote() {
		if (!plugin.getOptions().isProcessRewards()) {
			plugin.debug("Processing rewards is disabled");
			return;
		}

		Player player = getPlayer();
		if (!plugin.getOptions().isOnlineMode()) {
			player = Bukkit.getPlayer(getPlayerName());
		}
		if (player == null) {
			return;
		}

		plugin.extraDebug("Checking offline votes for " + player.getName() + "/" + getUUID());

		// Update top voter ignore flag if needed.
		boolean currentTopVoterIgnore = player.hasPermission("VotingPlugin.TopVoter.Ignore");
		if (isTopVoterIgnore() != currentTopVoterIgnore) {
			setTopVoterIgnore(currentTopVoterIgnore);
		}

		ArrayList<String> offlineVotes = getOfflineVotes();
		if (offlineVotes.isEmpty()) {
			return;
		}

		// Send vote effects and clear persistent offline votes.
		sendVoteEffects(false);
		setOfflineVotes(new ArrayList<>());

		boolean offlineBroadcastEnabled = plugin.getConfigFile().isFormatOnlyOneOfflineBroadcast();

		// If broadcast is enabled, do it using the local vote count.
		if (offlineBroadcastEnabled) {
			offlineBroadcast(this, plugin.getBungeeSettings().isUseBungeecoord(), offlineVotes.size());
		}

		// Process each offline vote.
		for (String voteSiteName : offlineVotes) {
			if (plugin.getVoteSiteManager().hasVoteSite(voteSiteName)) {
				plugin.debug("Giving offline site reward: " + voteSiteName);
				playerVote(plugin.getVoteSiteManager().getVoteSite(voteSiteName, true), false, !offlineBroadcastEnabled,
						false);
			} else {
				plugin.debug("Site doesn't exist: " + voteSiteName);
			}
		}
	}

	/**
	 * Processes a player vote.
	 *
	 * @param voteSite  the vote site
	 * @param online    whether the player is online
	 * @param broadcast whether to broadcast the vote
	 * @param bungee    whether to use bungee
	 */
	public void playerVote(VoteSite voteSite, boolean online, boolean broadcast, boolean bungee) {
		if (plugin.getConfigFile().isFormatBroadcastWhenOnline() && plugin.getConfigFile().isBroadcastVotesEnabled()
				&& broadcast && !plugin.getBungeeSettings().isDisableBroadcast()) {
			voteSite.broadcastVote(this);
		}
		voteSite.giveRewards(this, online, bungee);
	}

	/**
	 * Removes points from the user.
	 *
	 * @param points the number of points to remove
	 * @return true if the points were removed, false otherwise
	 */
	public boolean removePoints(int points) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points);
			return true;
		}
		return false;
	}

	/**
	 * Removes points from the user asynchronously.
	 *
	 * @param points the number of points to remove
	 * @param async  whether to remove the points asynchronously
	 * @return true if the points were removed, false otherwise
	 */
	public boolean removePoints(int points, boolean async) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points, async);
			return true;
		}
		return false;
	}

	/**
	 * Resets the last voted time for all vote sites.
	 */
	public void resetLastVoted() {
		HashMap<VoteSite, Long> map = getLastVotes();
		for (Entry<VoteSite, Long> e : map.entrySet()) {
			e.setValue(0l);
		}
		setLastVotes(map);
	}

	/**
	 * Resets the last voted time for a specific vote site.
	 *
	 * @param site the vote site
	 */
	public void resetLastVoted(VoteSite site) {
		HashMap<VoteSite, Long> map = getLastVotes();
		map.put(site, 0l);
		setLastVotes(map);
	}

	/**
	 * Resets the total votes for a specific top voter category.
	 *
	 * @param topVoter the top voter category
	 */
	public void resetTotals(TopVoter topVoter) {
		setTotal(topVoter, 0);
	}

	/**
	 * Sends vote effects to the user.
	 *
	 * @param online whether the user is online
	 */
	public void sendVoteEffects(boolean online) {
		plugin.getRewardHandler().giveReward(this, plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getAnySiteRewardsPath(), new RewardOptions().setOnline(online));
	}

	/**
	 * Sets the all-time total votes.
	 *
	 * @param allTimeTotal the all-time total votes
	 * @deprecated Use setTotal(TopVoter.AllTime, allTimeTotal) instead
	 */
	@Deprecated
	public void setAllTimeTotal(int allTimeTotal) {
		setTotal(TopVoter.AllTime, allTimeTotal);
	}

	/**
	 * Sets the best day vote streak.
	 *
	 * @param streak the best day vote streak
	 */
	public void setBestDayVoteStreak(int streak) {
		getData().setInt("BestDayVoteStreak", streak);
	}

	/**
	 * Sets the best month vote streak.
	 *
	 * @param streak the best month vote streak
	 */
	public void setBestMonthVoteStreak(int streak) {
		getData().setInt("BestMonthVoteStreak", streak);
	}

	/**
	 * Sets the best week vote streak.
	 *
	 * @param streak the best week vote streak
	 */
	public void setBestWeekVoteStreak(int streak) {
		getData().setInt("BestWeekVoteStreak", streak);
	}

	/**
	 * Sets the cooldown check.
	 *
	 * @param coolDownCheck whether the cooldown check is enabled
	 */
	public void setCoolDownCheck(boolean coolDownCheck) {
		getData().setBoolean(getCoolDownCheckPath(), coolDownCheck);
	}

	/**
	 * Sets the cooldown check for all vote sites.
	 *
	 * @param coolDownChecks the cooldown checks for all vote sites
	 */
	public void setCoolDownCheckSite(HashMap<String, Boolean> coolDownChecks) {
		ArrayList<String> data = new ArrayList<>();
		for (Entry<String, Boolean> entry : coolDownChecks.entrySet()) {
			String str = entry.getKey() + "//" + entry.getValue().toString();
			data.add(str);
		}
		getUserData().setStringList(getCoolDownCheckSitePath(), data);
	}

	/**
	 * Sets the cooldown check for a specific vote site.
	 *
	 * @param site  the vote site
	 * @param value whether the cooldown check is enabled
	 */
	public void setCoolDownCheckSite(VoteSite site, boolean value) {
		HashMap<String, Boolean> coolDownChecks = getCoolDownCheckSiteList();
		coolDownChecks.put(site.getKey(), Boolean.valueOf(value));
		setCoolDownCheckSite(coolDownChecks);
	}

	/**
	 * Sets the daily total votes.
	 *
	 * @param total the daily total votes
	 * @deprecated Use setTotal(TopVoter.Daily, total) instead
	 */
	@Deprecated
	public void setDailyTotal(int total) {
		setTotal(TopVoter.Daily, total);
	}

	/**
	 * Sets the day vote streak.
	 *
	 * @param streak the day vote streak
	 */
	public void setDayVoteStreak(int streak) {
		getData().setInt("DayVoteStreak", streak);
		if (getBestDayVoteStreak() < streak) {
			setBestDayVoteStreak(streak);
		}
	}

	/**
	 * Sets the last update time for the day vote streak.
	 *
	 * @param time the last update time for the day vote streak
	 */
	public void setDayVoteStreakLastUpdate(long time) {
		getData().setString("DayVoteStreakLastUpdate", "" + time);
	}

	/**
	 * Sets whether the broadcast is disabled.
	 *
	 * @param value true to disable the broadcast, false otherwise
	 */
	public void setDisableBroadcast(boolean value) {
		getUserData().setBoolean("DisableBroadcast", value);
	}

	/**
	 * Sets the day when the user has gotten all sites.
	 *
	 * @param day the day when the user has gotten all sites
	 */
	public void setGottenAllSitesDay(int day) {
		getData().setInt(plugin.getVotingPluginUserManager().getGottenAllSitesDayPath(), day);
	}

	/**
	 * Sets the day when the user has gotten almost all sites.
	 *
	 * @param day the day when the user has gotten almost all sites
	 */
	public void setGottenAlmostAllSitesDay(int day) {
		getData().setInt(plugin.getVotingPluginUserManager().getGottenAlmostAllSitesDayPath(), day);
	}

	/**
	 * Sets the highest daily total votes.
	 *
	 * @param total the highest daily total votes
	 */
	public void setHighestDailyTotal(int total) {
		getData().setInt("HighestDailyTotal", total);
	}

	/**
	 * Sets the highest monthly total votes.
	 *
	 * @param total the highest monthly total votes
	 */
	public void setHighestMonthlyTotal(int total) {
		getData().setInt("HighestMonthlyTotal", total);
	}

	/**
	 * Sets the highest weekly total votes.
	 *
	 * @param total the highest weekly total votes
	 */
	public void setHighestWeeklyTotal(int total) {
		getData().setInt("HighestWeeklyTotal", total);
	}

	/**
	 * Sets the total votes for the last month.
	 *
	 * @param total the total votes for the last month
	 */
	public void setLastMonthTotal(int total) {
		getData().setInt("LastMonthTotal", total);
	}

	/**
	 * Sets the last votes for each vote site.
	 *
	 * @param lastVotes a map of vote sites and the last vote time
	 */
	public void setLastVotes(HashMap<VoteSite, Long> lastVotes) {
		ArrayList<String> data = new ArrayList<>();
		for (Entry<VoteSite, Long> entry : lastVotes.entrySet()) {
			String str = entry.getKey().getKey() + "//" + entry.getValue().longValue();
			data.add(str);
		}
		getUserData().setStringList("LastVotes", data);
	}

	/**
	 * Sets the total votes for the month.
	 *
	 * @param total the total votes for the month
	 * @deprecated Use setTotal(TopVoter.Monthly, total) instead
	 */
	@Deprecated
	public void setMonthTotal(int total) {
		setTotal(TopVoter.Monthly, total);
	}

	/**
	 * Sets the month vote streak.
	 *
	 * @param streak the month vote streak
	 */
	public void setMonthVoteStreak(int streak) {
		getData().setInt("MonthVoteStreak", streak);
		if (getBestMonthVoteStreak() < streak) {
			setBestMonthVoteStreak(streak);
		}
	}

	/**
	 * Sets the list of offline votes.
	 *
	 * @param offlineVotes the list of offline votes
	 */
	public void setOfflineVotes(ArrayList<String> offlineVotes) {
		getUserData().setStringList("OfflineVotes", offlineVotes);
	}

	/**
	 * Sets the points of the user.
	 *
	 * @param value the number of points
	 */
	public void setPoints(int value) {
		getUserData().setInt(getPointsPath(), value, false);
	}

	/**
	 * Sets the points of the user asynchronously.
	 *
	 * @param value the number of points
	 * @param async whether to set the points asynchronously
	 */
	public void setPoints(int value, boolean async) {
		getUserData().setInt(getPointsPath(), value, false, async);
	}

	/**
	 * Sets whether the user is reminded.
	 *
	 * @param reminded true if the user is reminded, false otherwise
	 */
	public void setReminded(boolean reminded) {
		getUserData().setString("Reminded", "" + reminded);
	}

	/**
	 * Sets the current time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 */
	public void setTime(VoteSite voteSite) {
		setTime(voteSite, LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}

	/**
	 * Sets the specified time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @param time     the time to set
	 */
	public void setTime(VoteSite voteSite, Long time) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		if (lastVotes != null && lastVotes.containsKey(voteSite)) {
			if (lastVotes.get(voteSite).longValue() == time.longValue()) {
				plugin.debug("Not setting last vote time for " + voteSite.getKey() + ", already set to " + time);
				return;
			}
		}
		lastVotes.put(voteSite, time);
		setLastVotes(lastVotes);
	}

	/**
	 * Sets whether the user is ignored for top voter.
	 *
	 * @param topVoterIgnore true to ignore the user for top voter, false otherwise
	 */
	public void setTopVoterIgnore(boolean topVoterIgnore) {
		getUserData().setString("TopVoterIgnore", "" + topVoterIgnore);
	}

	/**
	 * Sets the total votes for the specified top voter category.
	 *
	 * @param top   the top voter category
	 * @param value the total votes to set
	 */
	public void setTotal(TopVoter top, int value) {
		switch (top) {
		case AllTime:
			getUserData().setInt("AllTimeTotal", value);
			break;
		case Daily:
			getUserData().setInt("DailyTotal", value);
			break;
		case Monthly:
			if (plugin.getConfigFile().isLimitMonthlyVotes()) {
				LocalDateTime time = plugin.getTimeChecker().getTime();
				int days = time.getDayOfMonth();
				if (value >= days * plugin.getVoteSiteManager().getVoteSitesEnabled().size()) {
					value = days * plugin.getVoteSiteManager().getVoteSitesEnabled().size();
				}
			}
			getData().setInt("MonthTotal", value);
			if (plugin.getConfigFile().isStoreMonthTotalsWithDate()) {
				getData().setInt(plugin.getVotingPluginUserManager().getMonthTotalsWithDatePath(), value);
			}
			break;
		case Weekly:
			getUserData().setInt("WeeklyTotal", value);
			break;
		default:
			break;
		}
	}

	/**
	 * Sets the number of votes for the vote party.
	 *
	 * @param value the number of votes to set
	 */
	public void setVotePartyVotes(int value) {
		getUserData().setInt("VotePartyVotes", value);
	}

	/**
	 * Sets the vote shop identifier limit.
	 *
	 * @param identifier the identifier for the vote shop
	 * @param value      the limit to set
	 */
	public void setVoteShopIdentifierLimit(String identifier, int value) {
		getData().setInt("VoteShopLimit" + identifier, value);
	}

	/**
	 * Sets the weekly total votes.
	 *
	 * @param total the weekly total votes
	 * @deprecated Use setTotal(TopVoter.Weekly, total) instead
	 */
	@Deprecated
	public void setWeeklyTotal(int total) {
		setTotal(TopVoter.Weekly, total);
	}

	/**
	 * Sets the week vote streak.
	 *
	 * @param streak the week vote streak
	 */
	public void setWeekVoteStreak(int streak) {
		getData().setInt("WeekVoteStreak", streak);
		if (getBestWeekVoteStreak() < streak) {
			setBestWeekVoteStreak(streak);
		}
	}

	/**
	 * Checks if the user should be reminded.
	 *
	 * @return true if the user should be reminded, false otherwise
	 */
	public boolean shouldBeReminded() {
		Player player = getPlayer();
		if (player != null) {
			if (player.hasPermission("VotingPlugin.NoRemind")) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Gets the last vote date for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @return the last vote date as a string
	 * @deprecated Use getTime(VoteSite) instead
	 */
	@Deprecated
	public String voteCommandLastDate(VoteSite voteSite) {
		long time = getTime(voteSite);
		if (time > 0) {
			Date date = new Date(time);
			String timeString = new SimpleDateFormat(plugin.getConfigFile().getFormatTimeFormat()).format(date);
			if (MessageAPI.containsIgnorecase(timeString, "YamlConfiguration")) {
				plugin.getLogger().warning("Detected issue parsing time, check time format");
			}
			return timeString;
		}
		return "";
	}

	/**
	 * Gets the duration since the last vote for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @return the duration since the last vote as a string
	 */
	public String voteCommandLastDuration(VoteSite voteSite) {
		long time = getTime(voteSite);
		if (time > 0) {
			LocalDateTime now = LocalDateTime.now();
			LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault());

			Duration dur = Duration.between(lastVote, now);

			long diffSecond = dur.getSeconds();
			int diffDays = (int) (diffSecond / 60 / 60 / 24);
			int diffHours = (int) (diffSecond / 60 / 60 - diffDays * 24);
			int diffMinutes = (int) (diffSecond / 60 - diffHours * 60 - diffDays * 24 * 60);
			int diffSeconds = (int) (diffSecond - diffMinutes * 60 - diffHours * 60 * 60 - diffDays * 24 * 60 * 60);

			String info = "";
			if (diffDays == 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsDay()), "amount", "" + diffDays);
				info += " ";
			} else if (diffDays > 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsDays()), "amount", "" + diffDays);
				info += " ";
			}

			if (diffHours == 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsHour()), "amount", "" + diffHours);
				info += " ";
			} else if (diffHours > 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsHours()), "amount", "" + diffHours);
				info += " ";
			}

			if (diffMinutes == 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsMinute()), "amount", "" + diffMinutes);
				info += " ";
			} else if (diffMinutes > 1) {
				info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsMinutes()), "amount", "" + diffMinutes);
				info += " ";
			}

			if (plugin.getConfigFile().isFormatCommandsVoteLastIncludeSeconds()) {
				if (diffSeconds == 1) {
					info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
							plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
							plugin.getConfigFile().getFormatTimeFormatsSecond()), "amount", "" + diffSeconds);
				} else {
					info += PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
							plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
							plugin.getConfigFile().getFormatTimeFormatsSeconds()), "amount", "" + diffSeconds);
				}
			}

			info = PlaceholderUtils.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteLastLastVoted(),
					"times", info);

			return info;
		}
		return plugin.getConfigFile().getFormatCommandsVoteLastNeverVoted();
	}

	/**
	 * Gets the last vote date and duration for the specified vote site for the GUI.
	 *
	 * @param voteSite the vote site
	 * @return the last vote date and duration as a string for the GUI
	 */
	public String voteCommandLastGUILine(VoteSite voteSite) {
		String timeString = voteCommandLastDate(voteSite);
		String timeSince = voteCommandLastDuration(voteSite);

		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return PlaceholderUtils.replacePlaceHolder(plugin.getGui().getChestVoteLastLine(), placeholders);
	}

	/**
	 * Gets the last vote date and duration for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @return the last vote date and duration as a string
	 */
	public String voteCommandLastLine(VoteSite voteSite) {
		String timeString = voteCommandLastDate(voteSite);
		String timeSince = voteCommandLastDuration(voteSite);

		HashMap<String, String> placeholders = new HashMap<>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return PlaceholderUtils.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteLastLine(),
				placeholders);
	}

	/**
	 * Gets the next available vote time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @return the next available vote time as a string
	 */
	public String voteCommandNextInfo(VoteSite voteSite) {
		return voteCommandNextInfo(voteSite, getTime(voteSite));
	}

	/**
	 * Gets the next available vote time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @param time     the current time
	 * @return the next available vote time as a string
	 */
	public String voteCommandNextInfo(VoteSite voteSite, long time) {
		String info = new String();

		long nextTime = voteNextDurationTime(voteSite, time);
		if (nextTime == 0) {
			info = plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
		} else {
			int diffHours = (int) (nextTime / (60 * 60));
			long diffMinutes = nextTime / 60 - diffHours * 60;

			if (diffHours < 0) {
				diffHours = diffHours * -1;
			}
			if (diffHours >= 24) {
				diffHours = diffHours - 24;
			}
			if (diffMinutes < 0) {
				diffMinutes = diffMinutes * -1;
			}

			String timeMsg = plugin.getConfigFile().getFormatCommandsVoteNextInfoVoteDelayDaily();
			timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
			timeMsg = MessageAPI.replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
			info = timeMsg;
		}

		return info;
	}

	/**
	 * Gets the next available vote duration time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @return the next available vote duration time in seconds
	 */
	public long voteNextDurationTime(VoteSite voteSite) {
		return voteNextDurationTime(voteSite, getTime(voteSite));
	}

	/**
	 * Gets the next available vote duration time for the specified vote site.
	 *
	 * @param voteSite the vote site
	 * @param time     the last vote time (epoch millis)
	 * @return the next available vote duration time in seconds
	 */
	public long voteNextDurationTime(VoteSite voteSite, long time) {
		LocalDateTime now = plugin.getTimeChecker().getTime();

		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(plugin.getOptions().getTimeHourOffSet());

		if (!voteSite.isVoteDelayDaily()) {
			ParsedDuration voteDelay = voteSite.getVoteDelay();

			if (time == 0 || voteDelay == null || voteDelay.isEmpty()) {
				return 0;
			}

			// Ignore months, use fixed duration only
			LocalDateTime nextVote = lastVote.plus(Duration.ofMillis(voteDelay.getMillis()));

			if (now.isAfter(nextVote)) {
				return 0;
			}

			return Duration.between(now, nextVote).getSeconds();
		}

		// Daily reset logic (unchanged)
		LocalDateTime resetTime = lastVote.withHour(voteSite.getVoteDelayDailyHour()).withMinute(0).withSecond(0);

		LocalDateTime resetTimeTomorrow = resetTime.plusHours(24);

		if (lastVote.isBefore(resetTime)) {
			if (now.isBefore(resetTime)) {
				return Duration.between(now, resetTime).getSeconds();
			}
		} else {
			if (now.isBefore(resetTimeTomorrow)) {
				return Duration.between(now, resetTimeTomorrow).getSeconds();
			}
		}

		return 0;
	}

	/**
	 * Checks if the vote streak was updated today.
	 *
	 * @param time the current time
	 * @return true if the vote streak was updated today, false otherwise
	 */
	public boolean voteStreakUpdatedToday(LocalDateTime time) {
		return MiscUtils.getInstance().getTime(getDayVoteStreakLastUpdate()).getDayOfYear() == time.getDayOfYear();
	}

	public String getVoteStreakState(String columnName) {
		return getData().getString(columnName);
	}

	public void setVoteStreakState(String columnName, String value) {
		getData().setString(columnName, value);
	}

}
