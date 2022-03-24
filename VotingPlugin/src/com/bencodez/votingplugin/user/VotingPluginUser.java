package com.bencodez.votingplugin.user;

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.YearMonth;
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

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.MiscUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.user.AdvancedCoreUser;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.bungee.BungeeMessageData;
import com.bencodez.votingplugin.bungee.BungeeMethod;
import com.bencodez.votingplugin.events.PlayerReceivePointsEvent;
import com.bencodez.votingplugin.events.PlayerVoteEvent;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.topvoter.TopVoterPlayer;

// TODO: Auto-generated Javadoc
/**
 * The Class User.
 */
public class VotingPluginUser extends com.bencodez.advancedcore.api.user.AdvancedCoreUser {

	/** The plugin. */
	private VotingPluginMain plugin;

	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, Player player) {
		super(plugin, player);
		this.plugin = plugin;
	}

	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, String playerName) {
		super(plugin, playerName);
		this.plugin = plugin;
	}

	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid) {
		super(plugin, uuid);
		this.plugin = plugin;
	}

	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid, boolean loadName) {
		super(plugin, uuid, loadName);
		this.plugin = plugin;
	}

	@Deprecated
	public VotingPluginUser(VotingPluginMain plugin, UUID uuid, String playerName) {
		super(plugin, uuid, playerName);
		this.plugin = plugin;
	}

	public VotingPluginUser(VotingPluginMain plugin, AdvancedCoreUser user) {
		super(plugin, user);
		this.plugin = plugin;
	}

	public void addAllTimeTotal() {
		setAllTimeTotal(getAllTimeTotal() + 1);
	}

	public void addDayVoteStreak() {
		setDayVoteStreak(getDayVoteStreak() + 1);
	}

	public void addMonthTotal() {
		setMonthTotal(getMonthTotal() + 1);
	}

	public void addMonthVoteStreak() {
		setMonthVoteStreak(getMonthVoteStreak() + 1);
	}

	public void addOfflineVote(String voteSiteName) {
		ArrayList<String> offlineVotes = getOfflineVotes();
		offlineVotes.add(voteSiteName);
		setOfflineVotes(offlineVotes);
	}

	/**
	 * Adds the points.
	 */
	public void addPoints() {
		int points = plugin.getConfigFile().getPointsOnVote();
		if (points != 0) {
			addPoints(points);
		}
	}

	/**
	 * Adds the points.
	 *
	 * @param value the value
	 * @return current total points
	 */
	public int addPoints(int value) {
		PlayerReceivePointsEvent event = new PlayerReceivePointsEvent(this, value);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return getPoints();
		}
		int newTotal = getPoints() + event.getPoints();
		setPoints(newTotal);
		return newTotal;
	}

	/**
	 * Adds the total.
	 *
	 */
	public void addTotal() {
		setMilestoneCount(getMilestoneCount() + 1);
		addMonthTotal();
		addAllTimeTotal();
	}

	/**
	 * Adds the total daily.
	 *
	 *
	 */
	public void addTotalDaily() {
		setDailyTotal(getDailyTotal() + 1);
	}

	/**
	 * Adds the total weekly.
	 *
	 *
	 */
	public void addTotalWeekly() {
		setWeeklyTotal(getWeeklyTotal() + 1);
	}

	public void addWeekVoteStreak() {
		setWeekVoteStreak(getWeekVoteStreak() + 1);
	}

	public void bungeeVote(String service, BungeeMessageData text, boolean setTotals) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Bungee vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service, true), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setVotingPluginUser(this);
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				}
			});
		}
	}

	public void bungeeVoteOnline(String service, BungeeMessageData text, boolean setTotals) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Bungee online vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service, true), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setVotingPluginUser(this);
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				}
			});
		}
	}

	public void bungeeVotePluginMessaging(String service, long time, BungeeMessageData text, boolean setTotals,
			boolean wasOnline, boolean broadcast) {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			plugin.debug("Pluginmessaging vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service, true), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setVotingPluginUser(this);
			voteEvent.setForceBungee(true);
			voteEvent.setTime(time);
			voteEvent.setAddTotals(setTotals);
			voteEvent.setBungeeTextTotals(text);
			voteEvent.setWasOnline(wasOnline);
			voteEvent.setBroadcast(broadcast);
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				}
			});
		}
	}

	/**
	 * Can vote all.
	 *
	 * @return true, if successful
	 */
	public boolean canVoteAll() {
		for (VoteSite voteSite : plugin.getVoteSites()) {
			if (!voteSite.isHidden()) {
				boolean canVote = canVoteSite(voteSite);
				if (!canVote) {
					return false;
				}
			}
		}
		return true;
	}

	public boolean canVoteAny() {
		for (VoteSite voteSite : plugin.getVoteSites()) {
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
	 * Can vote site.
	 *
	 * @param voteSite the vote site
	 * @return true, if successful
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
				double votedelay = voteSite.getVoteDelay();
				double voteDelayMin = voteSite.getVoteDelayMin();

				if (votedelay == 0 && voteDelayMin == 0) {
					return false;
				}

				LocalDateTime nextvote = lastVote.plusHours((long) votedelay).plusMinutes((long) voteDelayMin);

				return now.isAfter(nextvote);
			} else {
				if (now.getDayOfYear() != lastVote.getDayOfYear() || now.getYear() != lastVote.getYear()) {
					return true;
				}
				return false;
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;

	}

	/**
	 * Check all votes.
	 *
	 * @return true, if successful
	 */
	public boolean checkAllVotes() {
		VotingPluginUser user = this;

		ArrayList<Integer> months = new ArrayList<Integer>();
		ArrayList<Integer> days = new ArrayList<Integer>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			if (voteSite.isEnabled() && !voteSite.isHidden()) {
				long time = user.getTime(voteSite);
				if (time != 0) {
					months.add(MiscUtils.getInstance().getMonthFromMili(time));
					days.add(MiscUtils.getInstance().getDayFromMili(time));
				} else {
					return false;
				}
			}
		}

		// check months
		for (int i = 0; i < months.size(); i++) {
			if (!months.get(0).equals(months.get(i))) {
				return false;
			}
		}

		// check days
		for (int i = 0; i < days.size(); i++) {
			if (!days.get(0).equals(days.get(i))) {
				return false;
			}
		}

		return true;
	}

	public void checkDayVoteStreak(boolean forceBungee) {
		if (!voteStreakUpdatedToday(LocalDateTime.now())) {
			if (!plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage() || hasPercentageTotal(
					TopVoter.Daily, plugin.getSpecialRewardsConfig().getVoteStreakRequirementDay(), null)) {
				addDayVoteStreak();
				plugin.getSpecialRewards().checkVoteStreak(this, "Day", forceBungee);
				setDayVoteStreakLastUpdate(System.currentTimeMillis());
			}
		}
	}

	public void clearOfflineVotes() {
		setOfflineVotes(new ArrayList<String>());
		setOfflineRewards(new ArrayList<String>());
	}

	public void clearTotals() {
		for (TopVoter top : TopVoter.values()) {
			resetTotals(top);
		}
	}

	@Deprecated
	public int getAllTimeTotal() {
		return getUserData().getInt("AllTimeTotal", isCacheData(), isWaitForCache());
	}

	public int getBestDayVoteStreak() {
		return getData().getInt("BestDayVoteStreak", isCacheData(), isWaitForCache());
	}

	public int getBestMonthVoteStreak() {
		return getData().getInt("BestMonthVoteStreak", isCacheData(), isWaitForCache());
	}

	public int getBestWeekVoteStreak() {
		return getData().getInt("BestWeekVoteStreak", isCacheData(), isWaitForCache());
	}

	@Deprecated
	public int getDailyTotal() {
		return getUserData().getInt("DailyTotal", isCacheData(), isWaitForCache());
	}

	public int getDayVoteStreak() {
		return getData().getInt("DayVoteStreak", isCacheData(), isWaitForCache());
	}

	public long getDayVoteStreakLastUpdate() {
		String str = getData().getString("DayVoteStreakLastUpdate", isCacheData(), isWaitForCache());
		if (str == null || str.isEmpty() || str.equals("null")) {
			return 0;
		}
		return Long.parseLong(str);
	}

	public boolean getDisableBroadcast() {
		return getUserData().getBoolean("DisableBroadcast", isCacheData(), isWaitForCache());
	}

	public int getGottenAllSitesDay() {
		return getData().getInt(plugin.getVotingPluginUserManager().getGottenAllSitesDayPath(), 0, isWaitForCache());
	}

	public HashMap<String, Boolean> getHasGottenMilestone() {
		HashMap<String, Boolean> hasGottenMilestone = new HashMap<String, Boolean>();
		ArrayList<String> milestoneList = getUserData().getStringList(getGottenMilestonesPath());
		for (String str : milestoneList) {
			String[] data = str.split("//");
			if (data.length > 1) {
				boolean gotten = Boolean.parseBoolean(data[1]);
				hasGottenMilestone.put(data[0], gotten);
			}
		}
		return hasGottenMilestone;
	}

	public int getHighestDailyTotal() {
		return getData().getInt("HighestDailyTotal", isCacheData(), isWaitForCache());
	}

	public int getHighestMonthlyTotal() {
		return getData().getInt("HighestMonthlyTotal", isCacheData(), isWaitForCache());
	}

	public int getHighestWeeklyTotal() {
		return getData().getInt("HighestWeeklyTotal", isCacheData(), isWaitForCache());
	}

	public int getLastMonthTotal() {
		return getData().getInt("LastMonthTotal", isCacheData(), isWaitForCache());
	}

	public HashMap<VoteSite, Long> getLastVotes() {
		HashMap<VoteSite, Long> lastVotes = new HashMap<VoteSite, Long>();
		ArrayList<String> LastVotesList = getUserData().getStringList("LastVotes");
		for (String str : LastVotesList) {
			String[] data = str.split("//");
			if (data.length > 1 && plugin.hasVoteSite(data[0])) {
				VoteSite site = plugin.getVoteSite(data[0], true);
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
	 * Gets the last vote times sorted.
	 *
	 * @return the last vote times sorted
	 */
	public HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		LinkedHashMap<VoteSite, Long> times = new LinkedHashMap<VoteSite, Long>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			times.put(voteSite, getTime(voteSite));
		}
		LinkedHashMap<VoteSite, Long> sorted = new LinkedHashMap<>(
				times.entrySet().stream().sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
						.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue)));
		return sorted;
	}

	public int getMilestoneCount() {
		return getData().getInt("MilestoneCount", getAllTimeTotal(), isCacheData(), isWaitForCache());
	}

	@Deprecated
	public int getMonthTotal() {
		return getData().getInt("MonthTotal", isCacheData(), isWaitForCache());
	}

	public int getMonthVoteStreak() {
		return getData().getInt("MonthVoteStreak", isCacheData(), isWaitForCache());
	}

	public ArrayList<String> getOfflineVotes() {
		return getUserData().getStringList("OfflineVotes", isCacheData(), isWaitForCache());
	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public int getPoints() {
		return getUserData().getInt(getPointsPath(), isCacheData(), isWaitForCache());
	}

	public String getPointsPath() {
		if (plugin.getBungeeSettings().isPerServerPoints()) {
			return plugin.getBungeeSettings().getServerNameStorage() + "_Points";
		} else {
			return "Points";
		}
	}

	public java.util.UUID getPrimaryAccount() {
		String s = getData().getString("PrimaryAccount", isCacheData(), isWaitForCache());
		if (s != null && !s.isEmpty()) {
			return java.util.UUID.fromString(s);
		}
		return null;
	}

	public int getSitesVotedOn() {
		int amount = 0;
		for (VoteSite site : plugin.getVoteSites()) {
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

	public int getTotal(TopVoter top) {
		switch (top) {
		case AllTime:
			return getUserData().getInt("AllTimeTotal", isCacheData(), isWaitForCache());
		case Daily:
			return getUserData().getInt("DailyTotal", isCacheData(), isWaitForCache());
		case Monthly:
			return getData().getInt("MonthTotal", isCacheData(), isWaitForCache());
		case Weekly:
			return getUserData().getInt("WeeklyTotal", isCacheData(), isWaitForCache());
		default:
			break;
		}
		return 0;
	}

	public int getVotePartyVotes() {
		return getUserData().getInt("VotePartyVotes", isCacheData(), isWaitForCache());
	}

	public int getVoteShopIdentifierLimit(String identifier) {
		return getData().getInt("VoteShopLimit" + identifier, isCacheData(), isWaitForCache());
	}

	@Deprecated
	public int getWeeklyTotal() {
		return getUserData().getInt("WeeklyTotal", isCacheData(), isWaitForCache());
	}

	public int getWeekVoteStreak() {
		return getData().getInt("WeekVoteStreak", isCacheData(), isWaitForCache());
	}

	public void giveDailyTopVoterAward(int place, String path) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getDailyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
						.withPlaceHolder("topvoter", "Daily").withPlaceHolder("votes", "" + getTotal(TopVoter.Daily))
						.setOnline(isOnline()).send(this);
	}

	public void giveMonthlyTopVoterAward(int place, String path) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getMonthlyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
						.withPlaceHolder("topvoter", "Monthly")
						.withPlaceHolder("votes", "" + getTotal(TopVoter.Monthly)).setOnline(isOnline()).send(this);
	}

	public void giveWeeklyTopVoterAward(int place, String path) {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getWeeklyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
						.withPlaceHolder("topvoter", "Weekly").withPlaceHolder("votes", "" + getTotal(TopVoter.Weekly))
						.setOnline(isOnline()).send(this);
	}

	/**
	 * Checks for gotten first vote.
	 *
	 * @return true if user got the first vote reward
	 */
	public boolean hasGottenFirstVote() {
		if (plugin.getBungeeSettings().isUseBungeecoord()
				&& plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			return getTotal(TopVoter.AllTime) > 1;
		}
		return getTotal(TopVoter.AllTime) != 0;
	}

	public boolean hasGottenFirstVoteToday() {
		if (plugin.getBungeeSettings().isUseBungeecoord()
				&& plugin.getBungeeHandler().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			return getTotal(TopVoter.Daily) > 1;
		}
		return getTotal(TopVoter.Daily) != 0;
	}

	/**
	 * Checks for gotten milestone.
	 *
	 * @param votesRequired the votes required
	 * @return true, if successful
	 */
	public boolean hasGottenMilestone(int votesRequired) {
		HashMap<String, Boolean> hasGottenMilestone = getHasGottenMilestone();
		if (hasGottenMilestone.containsKey("" + votesRequired)) {
			return hasGottenMilestone.get("" + votesRequired);
		}
		return false;
	}

	public boolean hasPercentageTotal(TopVoter top, double percentage, LocalDateTime time) {
		int total = getTotal(top);
		switch (top) {
		case Daily:
			return total / plugin.getVoteSites().size() * 100 > percentage;
		case Monthly:
			return total / (plugin.getVoteSites().size() * time.getMonth().length(false)) * 100 > percentage;
		case Weekly:
			return total / (plugin.getVoteSites().size() * 7) * 100 > percentage;
		default:
			return false;
		}
	}

	public boolean hasPrimaryAccount() {
		return getPrimaryAccount() != null;
	}

	public boolean isReminded() {
		return getUserData().getBoolean("Reminded");
	}

	public boolean isTopVoterIgnore() {
		return getUserData().getBoolean("TopVoterIgnore");
	}

	/**
	 * Login message.
	 */
	public void loginMessage() {
		if (plugin.getConfigFile().getVoteRemindingRemindOnLogin()) {
			plugin.getVoteReminding().runRemindLogin(this);
		}
	}

	public void loginRewards() {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), "LoginRewards").send(this);
	}

	public void logoutRewards() {
		new RewardBuilder(plugin.getSpecialRewardsConfig().getData(), "LogoutRewards").send(this);
	}

	/**
	 * Off vote.
	 */
	public void offVote() {
		if (!plugin.getOptions().isProcessRewards()) {
			plugin.debug("Processing rewards is disabled");
			return;
		}

		Player player = getPlayer();
		if (player != null) {
			plugin.extraDebug("Checking offline vote site votes for " + player.getName());

			boolean topVoterIngorePerm = player.hasPermission("VotingPlugin.TopVoter.Ignore");
			if (isTopVoterIgnore() != topVoterIngorePerm) {
				setTopVoterIgnore(topVoterIngorePerm);
			}
			ArrayList<String> offlineVotes = getOfflineVotes();
			// plugin.debug(ArrayUtils.getInstance().makeStringList(offlineVotes));
			if (offlineVotes.size() > 0) {
				sendVoteEffects(false);
				setOfflineVotes(new ArrayList<String>());
			}

			for (int i = 0; i < offlineVotes.size(); i++) {
				if (plugin.hasVoteSite(offlineVotes.get(i))) {
					plugin.debug("Giving offline site reward: " + offlineVotes.get(i));
					playerVote(plugin.getVoteSite(offlineVotes.get(i), true), false, true, false);
				} else {
					plugin.debug("Site doesn't exist: " + offlineVotes.get(i));
				}
			}
		}
	}

	public void playerVote(VoteSite voteSite, boolean online, boolean broadcast, boolean bungee) {
		if (plugin.getConfigFile().getFormatBroadcastWhenOnline() && plugin.getConfigFile().isBroadcastVotesEnabled()
				&& broadcast && !plugin.getBungeeSettings().isDisableBroadcast()) {
			voteSite.broadcastVote(this);
		}
		voteSite.giveRewards(this, online, bungee);
	}

	/**
	 * Removes the points.
	 *
	 * @param points the points
	 * @return true, if successful
	 */
	public boolean removePoints(int points) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points);
			return true;
		}
		return false;
	}

	public void resetTotals(TopVoter topVoter) {
		setTotal(topVoter, 0);
	}

	/**
	 * Send vote effects.
	 *
	 * @param online the online
	 */
	public void sendVoteEffects(boolean online) {
		RewardHandler.getInstance().giveReward(this, plugin.getSpecialRewardsConfig().getData(),
				plugin.getSpecialRewardsConfig().getAnySiteRewardsPath(), new RewardOptions().setOnline(online));
	}

	@Deprecated
	public void setAllTimeTotal(int allTimeTotal) {
		setTotal(TopVoter.AllTime, allTimeTotal);
	}

	public void setBestDayVoteStreak(int streak) {
		getData().setInt("BestDayVoteStreak", streak);
	}

	public void setBestMonthVoteStreak(int streak) {
		getData().setInt("BestMonthVoteStreak", streak);
	}

	public void setBestWeekVoteStreak(int streak) {
		getData().setInt("BestWeekVoteStreak", streak);
	}

	@Deprecated
	public void setDailyTotal(int total) {
		setTotal(TopVoter.Daily, total);
	}

	public void setDayVoteStreak(int streak) {
		getData().setInt("DayVoteStreak", streak);
		if (getBestDayVoteStreak() < streak) {
			setBestDayVoteStreak(streak);
		}
	}

	public void setDayVoteStreakLastUpdate(long time) {
		getData().setString("DayVoteStreakLastUpdate", "" + time);
	}

	public void setDisableBroadcast(boolean value) {
		getUserData().setBoolean("DisableBroadcast", value);
	}

	public void setGottenAllSitesDay(int day) {
		getData().setInt(plugin.getVotingPluginUserManager().getGottenAllSitesDayPath(), day);
	}

	public void setHasGotteMilestone(int votesRequired, boolean b) {
		HashMap<String, Boolean> hasGottenMilestone = getHasGottenMilestone();
		hasGottenMilestone.put("" + votesRequired, b);
		setHasGottenMilestone(hasGottenMilestone);
	}

	public void setHasGottenMilestone(HashMap<String, Boolean> hasGottenMilestone) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<String, Boolean> entry : hasGottenMilestone.entrySet()) {
			String str = entry.getKey() + "//" + entry.getValue().booleanValue();
			data.add(str);
		}
		getUserData().setStringList(getGottenMilestonesPath(), data);
	}

	public String getGottenMilestonesPath() {
		if (plugin.getBungeeSettings().isPerServerMilestones()) {
			return plugin.getOptions().getServer() + "_" + "GottenMilestones";
		}
		return "GottenMileStones";
	}

	public void setHighestDailyTotal(int total) {
		getData().setInt("HighestDailyTotal", total);
	}

	public void setHighestMonthlyTotal(int total) {
		getData().setInt("HighestMonthlyTotal", total);
	}

	public void setHighestWeeklyTotal(int total) {
		getData().setInt("HighestWeeklyTotal", total);
	}

	public void setLastMonthTotal(int total) {
		getData().setInt("LastMonthTotal", total);
	}

	public void setLastVotes(HashMap<VoteSite, Long> lastVotes) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Long> entry : lastVotes.entrySet()) {
			String str = entry.getKey().getKey() + "//" + entry.getValue().longValue();
			data.add(str);
		}
		getUserData().setStringList("LastVotes", data);
	}

	public void setMilestoneCount(int value) {
		getData().setInt("MilestoneCount", value);
	}

	public String getCoolDownCheckPath() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			return "CoolDownCheck_" + plugin.getBungeeSettings().getServer();
		} else {
			return "CoolDownCheck";
		}
	}

	public void setCoolDownCheck(boolean coolDownCheck) {
		getData().setBoolean(getCoolDownCheckPath(), coolDownCheck);
	}

	public boolean getCoolDownCheck() {
		return getData().getBoolean(getCoolDownCheckPath(), isCacheData(), isWaitForCache());
	}

	@Deprecated
	public void setMonthTotal(int total) {
		setTotal(TopVoter.Monthly, total);
	}

	public void setMonthVoteStreak(int streak) {
		getData().setInt("MonthVoteStreak", streak);
		if (getBestMonthVoteStreak() < streak) {
			setBestMonthVoteStreak(streak);
		}
	}

	public void setOfflineVotes(ArrayList<String> offlineVotes) {
		getUserData().setStringList("OfflineVotes", offlineVotes);
	}

	/**
	 * Sets the points.
	 *
	 * @param value the new points
	 */
	public void setPoints(int value) {
		getUserData().setInt(getPointsPath(), value, !plugin.getBungeeSettings().isUseBungeecoord());
	}

	public void setPrimaryAccount(java.util.UUID uuid) {
		if (uuid != null) {
			getData().setString("PrimaryAccount", uuid.toString());
		} else {
			getData().setString("PrimaryAccount", "");
		}
	}

	public void setReminded(boolean reminded) {
		getUserData().setString("Reminded", "" + reminded);
	}

	public void setTime(VoteSite voteSite) {
		setTime(voteSite, LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}

	public void setTime(VoteSite voteSite, Long time) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		lastVotes.put(voteSite, time);
		setLastVotes(lastVotes);
	}

	public void setTopVoterIgnore(boolean topVoterIgnore) {
		getUserData().setString("TopVoterIgnore", "" + topVoterIgnore);
	}

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
				int days = YearMonth.of(time.getYear(), time.getMonth()).lengthOfMonth();
				if (value >= days * plugin.getVoteSites().size()) {
					value = days * plugin.getVoteSites().size();
				}
			}
			getData().setInt("MonthTotal", value);
			break;
		case Weekly:
			getUserData().setInt("WeeklyTotal", value);
			break;
		default:
			break;

		}
	}

	public void setVotePartyVotes(int value) {
		getUserData().setInt("VotePartyVotes", value);
	}

	public void setVoteShopIdentifierLimit(String identifier, int value) {
		getData().setInt("VoteShopLimit" + identifier, value);
	}

	@Deprecated
	public void setWeeklyTotal(int total) {
		setTotal(TopVoter.Weekly, total);
	}

	public void setWeekVoteStreak(int streak) {
		getData().setInt("WeekVoteStreak", streak);
		if (getBestWeekVoteStreak() < streak) {
			setBestWeekVoteStreak(streak);
		}
	}

	public boolean shouldBeReminded() {
		Player player = getPlayer();
		if (player != null) {
			if (player.hasPermission("VotingPlugin.NoRemind")) {
				return false;
			}
		}
		return true;
	}

	@Deprecated
	public String voteCommandLastDate(VoteSite voteSite) {
		long time = getTime(voteSite);
		if (time > 0) {
			Date date = new Date(time);
			String timeString = new SimpleDateFormat(plugin.getConfigFile().getFormatTimeFormat()).format(date);
			if (StringParser.getInstance().containsIgnorecase(timeString, "YamlConfiguration")) {
				plugin.getLogger().warning("Detected issue parsing time, check time format");
			}
			return timeString;
		}
		return "";
	}

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
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								plugin.getConfigFile().getFormatTimeFormatsDay()), "amount", "" + diffDays);
				info += " ";
			} else if (diffDays > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								plugin.getConfigFile().getFormatTimeFormatsDays()), "amount", "" + diffDays);
				info += " ";
			}

			if (diffHours == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								plugin.getConfigFile().getFormatTimeFormatsHour()), "amount", "" + diffHours);
				info += " ";
			} else if (diffHours > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								plugin.getConfigFile().getFormatTimeFormatsHours()), "amount", "" + diffHours);
				info += " ";
			}

			if (diffMinutes == 1) {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsMinute()), "amount", "" + diffMinutes);
				info += " ";
			} else if (diffMinutes > 1) {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsMinutes()), "amount", "" + diffMinutes);
				info += " ";
			}

			if (diffSeconds == 1) {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsSecond()), "amount", "" + diffSeconds);
			} else {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						plugin.getConfigFile().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						plugin.getConfigFile().getFormatTimeFormatsSeconds()), "amount", "" + diffSeconds);
			}

			info = StringParser.getInstance()
					.replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteLastLastVoted(), "times", info);

			return info;
		}
		return plugin.getConfigFile().getFormatCommandsVoteLastNeverVoted();
	}

	public String voteCommandLastGUILine(VoteSite voteSite) {
		String timeString = voteCommandLastDate(voteSite);
		String timeSince = voteCommandLastDuration(voteSite);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return StringParser.getInstance().replacePlaceHolder(plugin.getGui().getChestVoteLastLine(), placeholders);
	}

	public String voteCommandLastLine(VoteSite voteSite) {
		String timeString = voteCommandLastDate(voteSite);
		String timeSince = voteCommandLastDuration(voteSite);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return StringParser.getInstance().replacePlaceHolder(plugin.getConfigFile().getFormatCommandsVoteLastLine(),
				placeholders);
	}

	public String voteCommandNextInfo(VoteSite voteSite) {
		String info = new String();

		long time = getTime(voteSite);
		LocalDateTime now = plugin.getTimeChecker().getTime();

		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(plugin.getOptions().getTimeHourOffSet());

		if (!voteSite.isVoteDelayDaily()) {
			double votedelay = voteSite.getVoteDelay();
			if (votedelay == 0 && voteSite.getVoteDelayMin() == 0) {
				String errorMsg = plugin.getConfigFile().getFormatCommandsVoteNextInfoError();
				info = errorMsg;
			} else {

				LocalDateTime nextvote = lastVote.plusHours((long) votedelay)
						.plusMinutes((long) voteSite.getVoteDelayMin());

				if (time == 0 || now.isAfter(nextvote)) {
					info = plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				} else {
					Duration dur = Duration.between(now, nextvote);

					long diffHours = dur.getSeconds() / (60 * 60);
					long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

					String timeMsg = plugin.getConfigFile().getFormatCommandsVoteNextInfoTime();
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Long.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;

				}
			}
		} else {
			LocalDateTime offsetoclockyesterday = plugin.getTimeChecker().getTime().plusDays(-1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktoday = plugin.getTimeChecker().getTime().withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktomorrow = plugin.getTimeChecker().getTime().plusDays(1).withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());

			if (!now.isBefore(offsetoclocktoday)) {
				if (!lastVote.isBefore(offsetoclocktoday)) {
					Duration dur = Duration.between(now, offsetoclocktomorrow);
					int diffHours = (int) (dur.getSeconds() / (60 * 60));
					long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

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
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Integer.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					info = plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				}
			} else {
				if (!lastVote.isBefore(offsetoclockyesterday)) {
					Duration dur = Duration.between(now, offsetoclocktoday);
					int diffHours = (int) (dur.getSeconds() / (60 * 60));
					long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

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
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Integer.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					info = plugin.getConfigFile().getFormatCommandsVoteNextInfoCanVote();
				}
			}
		}
		return info;
	}

	public long voteNextDurationTime(VoteSite voteSite) {
		long time = getTime(voteSite);
		LocalDateTime now = plugin.getTimeChecker().getTime();

		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(plugin.getOptions().getTimeHourOffSet());

		if (!voteSite.isVoteDelayDaily()) {
			double votedelay = voteSite.getVoteDelay();
			if (votedelay == 0 && voteSite.getVoteDelayMin() == 0) {
				return 0;
			} else {
				LocalDateTime nextvote = lastVote.plusHours((long) votedelay)
						.plusMinutes((long) voteSite.getVoteDelayMin());

				if (time == 0 || now.isAfter(nextvote)) {
					return 0;
				} else {
					Duration dur = Duration.between(now, nextvote);
					return dur.getSeconds();
				}
			}
		} else {
			LocalDateTime offsetoclockyesterday = plugin.getTimeChecker().getTime().plusDays(-1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktoday = plugin.getTimeChecker().getTime().withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktomorrow = plugin.getTimeChecker().getTime().plusDays(1).withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());

			if (!now.isBefore(offsetoclocktoday)) {
				if (!lastVote.isBefore(offsetoclocktoday)) {
					Duration dur = Duration.between(now, offsetoclocktomorrow);
					return dur.getSeconds();
				} else {
					return 0;
				}
			} else {
				if (!lastVote.isBefore(offsetoclockyesterday)) {
					Duration dur = Duration.between(now, offsetoclocktoday);
					return dur.getSeconds();
				} else {
					return 0;
				}
			}
		}
	}

	public boolean voteStreakUpdatedToday(LocalDateTime time) {
		return MiscUtils.getInstance().getTime(getDayVoteStreakLastUpdate()).getDayOfYear() == time.getDayOfYear();
	}

	public int getSitesNotVotedOn() {
		int amount = 0;
		for (VoteSite site : plugin.getVoteSites()) {
			if (!site.isHidden()) {
				if (canVoteSite(site)) {
					amount++;
				}
			}
		}
		return amount;
	}

	public TopVoterPlayer getTopVoterPlayer() {
		return new TopVoterPlayer(UUID.fromString(getUUID()), getPlayerName());
	}

	public long getNextTimeAllSitesAvailable() {
		long longest = 0;
		for (VoteSite site : plugin.getVoteSites()) {
			long seconds = voteNextDurationTime(site);
			if (seconds > longest) {
				longest = seconds;
			}
		}

		return longest;
	}

}
