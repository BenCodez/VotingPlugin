package com.Ben12345rocks.VotingPlugin.Objects;

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
import java.util.stream.Collectors;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.AdvancedCore.TimeChecker.TimeChecker;
import com.Ben12345rocks.AdvancedCore.UserManager.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.VotingPlugin.BungeeHandler;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.BungeeSettings;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.SpecialRewardsConfig;
import com.Ben12345rocks.VotingPlugin.Events.PlayerReceivePointsEvent;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteCoolDownEndEvent;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
import com.Ben12345rocks.VotingPlugin.SpecialRewards.SpecialRewards;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.Ben12345rocks.VotingPlugin.VoteReminding.VoteReminding;
import com.Ben12345rocks.VotingPlugin.bungee.BungeeMethod;

// TODO: Auto-generated Javadoc
/**
 * The Class User.
 */
public class User extends com.Ben12345rocks.AdvancedCore.UserManager.User {

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Instantiates a new user.
	 *
	 * @param player the player
	 */
	@Deprecated
	public User(Player player) {
		super(plugin, player);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param playerName the player name
	 */
	@Deprecated
	public User(String playerName) {
		super(plugin, playerName);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid the uuid
	 */
	@Deprecated
	public User(UUID uuid) {
		super(plugin, uuid);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid     the uuid
	 * @param loadName the load name
	 */
	@Deprecated
	public User(UUID uuid, boolean loadName) {
		super(plugin, uuid, loadName);
	}

	public void setPrimaryAccount(java.util.UUID uuid) {
		if (uuid != null) {
			getData().setString("PrimaryAccount", uuid.toString());
		} else {
			getData().setString("PrimaryAccount", "");
		}
	}

	public java.util.UUID getPrimaryAccount() {
		String s = getData().getString("PrimaryAccount", true);
		if (s != null && !s.isEmpty()) {
			return java.util.UUID.fromString(s);
		}
		return null;
	}

	public boolean hasPrimaryAccount() {
		return getPrimaryAccount() != null;
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

	public void addOfflineOtherReward(String reward) {
		ArrayList<String> offlineOtherRewards = getOfflineOtherRewards();
		offlineOtherRewards.add(reward);
		setOfflineOtherRewards(offlineOtherRewards);
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
		int points = Config.getInstance().getPointsOnVote();
		if (points != 0) {
			addPoints(points);
		}
	}

	/**
	 * Adds the points.
	 *
	 * @param value the value
	 */
	public void addPoints(int value) {
		PlayerReceivePointsEvent event = new PlayerReceivePointsEvent(this, value);
		Bukkit.getPluginManager().callEvent(event);

		if (event.isCancelled()) {
			return;
		}
		setPoints(getPoints() + event.getPoints());
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

	public void bungeeVote(String service) {
		if (BungeeSettings.getInstance().isUseBungeecoord()) {
			Main.plugin.debug("Bungee vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				}
			});
		}
	}

	public void bungeeVoteOnline(String service) {
		if (BungeeSettings.getInstance().isUseBungeecoord()) {
			Main.plugin.debug("Bungee online vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(false);
			Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

				@Override
				public void run() {
					plugin.getServer().getPluginManager().callEvent(voteEvent);
				}
			});
		}
	}

	public void loginRewards() {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(), "LoginRewards").send(this);
	}

	public void logoutRewards() {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(), "LogoutRewards").send(this);
	}

	public void bungeeVotePluginMessaging(String service, long time, String bungeeTextTotals) {
		if (BungeeSettings.getInstance().isUseBungeecoord()) {
			Main.plugin.debug("Pluginmessaging vote for " + getPlayerName() + " on " + service);

			PlayerVoteEvent voteEvent = new PlayerVoteEvent(plugin.getVoteSite(service), getPlayerName(), service,
					true);
			voteEvent.setBungee(true);
			voteEvent.setForceBungee(true);
			voteEvent.setTime(time);
			voteEvent.setAddTotals(false);
			voteEvent.setBungeeTextTotals(bungeeTextTotals);
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
			boolean canVote = canVoteSite(voteSite);
			if (!canVote) {
				return false;
			}
		}
		return true;
	}

	public boolean canVoteAny() {
		for (VoteSite voteSite : plugin.getVoteSites()) {
			boolean canVote = canVoteSite(voteSite);
			if (canVote) {
				return true;
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
			LocalDateTime now = TimeChecker.getInstance().getTime();
			LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
					.plusHours(Main.plugin.getOptions().getTimeHourOffSet());

			if (!voteSite.isVoteDelayDaily()) {
				double votedelay = voteSite.getVoteDelay();

				if (votedelay == 0) {
					return false;
				}

				LocalDateTime nextvote = lastVote.plusHours((long) votedelay)
						.plusMinutes((long) voteSite.getVoteDelayMin());

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
		User user = this;

		ArrayList<Integer> months = new ArrayList<Integer>();
		ArrayList<Integer> days = new ArrayList<Integer>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			long time = user.getTime(voteSite);
			if (time != 0) {
				months.add(MiscUtils.getInstance().getMonthFromMili(time));
				days.add(MiscUtils.getInstance().getDayFromMili(time));
			} else {
				return false;
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

	public void checkCoolDownEvents() {
		for (VoteSite site : plugin.getVoteSites()) {
			if (canVoteSite(site) != getLastCoolDownCheck(site)) {
				plugin.debug(getPlayerName() + " vote cooldown ended: " + site.getKey());
				PlayerVoteCoolDownEndEvent event = new PlayerVoteCoolDownEndEvent(this, site);
				plugin.getServer().getPluginManager().callEvent(event);
				setLastVoteCoolDownCheck(true, site);
			}
		}
	}

	public void checkDayVoteStreak() {
		if (!voteStreakUpdatedToday(LocalDateTime.now())) {
			if (!SpecialRewardsConfig.getInstance().isVoteStreakRequirementUsePercentage() || hasPercentageTotal(
					TopVoter.Daily, SpecialRewardsConfig.getInstance().getVoteStreakRequirementDay(), null)) {
				addDayVoteStreak();
				SpecialRewards.getInstance().checkVoteStreak(this, "Day");
				setDayVoteStreakLastUpdate(System.currentTimeMillis());
			}
		}
	}

	public void clearOfflineRewards() {
		setOfflineVotes(new ArrayList<String>());
		setOfflineRewards(new ArrayList<String>());
		setOfflineOtherRewards(new ArrayList<String>());
	}

	public void clearTotals() {
		for (TopVoter top : TopVoter.values()) {
			resetTotals(top);
		}
	}

	@Deprecated
	public int getAllTimeTotal() {
		return getUserData().getInt("AllTimeTotal");
	}

	public int getBestDayVoteStreak() {
		return getData().getInt("BestDayVoteStreak", isWaitForCache());
	}

	public int getBestMonthVoteStreak() {
		return getData().getInt("BestMonthVoteStreak", isWaitForCache());
	}

	public int getBestWeekVoteStreak() {
		return getData().getInt("BestWeekVoteStreak", isWaitForCache());
	}

	@Deprecated
	public int getDailyTotal() {
		return getUserData().getInt("DailyTotal", isWaitForCache());
	}

	public int getDayVoteStreak() {
		return getData().getInt("DayVoteStreak", isWaitForCache());
	}

	public long getDayVoteStreakLastUpdate() {
		String str = getData().getString("DayVoteStreakLastUpdate", isWaitForCache());
		if (str.isEmpty()) {
			return 0;
		}
		return Long.parseLong(str);
	}

	public boolean getDisableBroadcast() {
		return getUserData().getBoolean("DisableBroadcast");
	}

	public HashMap<String, Boolean> getHasGottenMilestone() {
		HashMap<String, Boolean> hasGottenMilestone = new HashMap<String, Boolean>();
		ArrayList<String> milestoneList = getUserData().getStringList("GottenMileStones");
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
		return getData().getInt("HighestDailyTotal", isWaitForCache());
	}

	public int getHighestMonthlyTotal() {
		return getData().getInt("HighestMonthlyTotal", isWaitForCache());
	}

	public int getHighestWeeklyTotal() {
		return getData().getInt("HighestWeeklyTotal", isWaitForCache());
	}

	public boolean getLastCoolDownCheck(VoteSite site) {
		HashMap<VoteSite, Boolean> array = getLastCoolDownCheckArray();
		if (array.containsKey(site)) {
			return array.get(site).booleanValue();
		}
		return true;
	}

	public HashMap<VoteSite, Boolean> getLastCoolDownCheckArray() {
		HashMap<VoteSite, Boolean> lastVotesCheck = new HashMap<VoteSite, Boolean>();
		ArrayList<String> LastVotesCheckList = getUserData().getStringList("LastVoteCoolDownCheck");
		for (String str : LastVotesCheckList) {
			String[] data = str.split("//");
			if (data.length > 1 && plugin.hasVoteSite(data[0])) {
				VoteSite site = plugin.getVoteSite(data[0]);
				if (site != null) {
					Boolean value = Boolean.FALSE;
					try {
						value = Boolean.valueOf(data[1]);
					} catch (NumberFormatException e) {
						plugin.debug("Not value: " + data[1]);
					}
					lastVotesCheck.put(site, value);
				}
			}
		}
		return lastVotesCheck;
	}

	public int getLastMonthTotal() {
		return getData().getInt("LastMonthTotal", isWaitForCache());
	}

	public HashMap<VoteSite, Long> getLastVotes() {
		HashMap<VoteSite, Long> lastVotes = new HashMap<VoteSite, Long>();
		ArrayList<String> LastVotesList = getUserData().getStringList("LastVotes");
		for (String str : LastVotesList) {
			String[] data = str.split("//");
			if (data.length > 1 && plugin.hasVoteSite(data[0])) {
				VoteSite site = plugin.getVoteSite(data[0]);
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
		return getData().getInt("MilestoneCount", getAllTimeTotal(), isWaitForCache());
	}

	@Deprecated
	public int getMonthTotal() {
		return getData().getInt("MonthTotal");
	}

	public int getMonthVoteStreak() {
		return getData().getInt("MonthVoteStreak", isWaitForCache());
	}

	public ArrayList<String> getOfflineOtherRewards() {
		return getUserData().getStringList("OfflineOtherRewards");
	}

	public ArrayList<String> getOfflineVotes() {
		return getUserData().getStringList("OfflineVotes");
	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public int getPoints() {
		return getUserData().getInt("Points", isWaitForCache());
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
			return getUserData().getInt("AllTimeTotal", isWaitForCache());
		case Daily:
			return getUserData().getInt("DailyTotal", isWaitForCache());
		case Monthly:
			return getData().getInt("MonthTotal", isWaitForCache());
		case Weekly:
			return getUserData().getInt("WeeklyTotal", isWaitForCache());
		default:
			break;
		}
		return 0;
	}

	public int getVotePartyVotes() {
		return getUserData().getInt("VotePartyVotes", isWaitForCache());
	}

	/*
	 * public Integer hasLastCumulative(int votesRequired) { HashMap<Integer,
	 * Integer> lastCumative = getLastCumulatives(); if
	 * (lastCumative.containsKey(votesRequired)) { return
	 * lastCumative.get(votesRequired); } return 0; } public HashMap<Integer,
	 * Integer> getLastCumulatives() { HashMap<Integer, Integer> lastCumulative =
	 * new HashMap<Integer, Integer>(); ArrayList<String> milestoneList =
	 * getUserData().getStringList("LastCumulative"); for (String str :
	 * milestoneList) { String[] data = str.split("//"); if (data.length > 1) { try
	 * { lastCumulative.put(Integer.parseInt(data[0]), Integer.parseInt(data[1])); }
	 * catch (Exception e) { e.printStackTrace(); } } } return lastCumulative; }
	 * public void setLastCumulatives(HashMap<Integer, Integer> lastCumulative) {
	 * ArrayList<String> data = new ArrayList<String>(); for (Entry<Integer,
	 * Integer> entry : lastCumulative.entrySet()) { String str = entry.getKey() +
	 * "//" + entry.getValue(); data.add(str); }
	 * getUserData().setStringList("LastCumulative", data); } public void
	 * setLastCumulative(int votesRequired, int value) { HashMap<Integer, Integer>
	 * lastCumulative = getLastCumulatives(); lastCumulative.put(votesRequired,
	 * value); setLastCumulatives(lastCumulative); }
	 */

	public int getVoteShopIdentifierLimit(String identifier) {
		return getData().getInt("VoteShopLimit" + identifier, isWaitForCache());
	}

	@Deprecated
	public int getWeeklyTotal() {
		return getUserData().getInt("WeeklyTotal");
	}

	public int getWeekVoteStreak() {
		return getData().getInt("WeekVoteStreak", isWaitForCache());
	}

	public void giveDailyTopVoterAward(int place, String path) {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getDailyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
						.withPlaceHolder("topvoter", "Daily").withPlaceHolder("votes", "" + getTotal(TopVoter.Daily))
						.setOnline(isOnline()).send(this);
	}

	public void giveMonthlyTopVoterAward(int place, String path) {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getMonthlyAwardRewardsPath(path))
						.withPlaceHolder("place", "" + place).withPlaceHolder("topvoter", "Monthly")
						.withPlaceHolder("votes", "" + getTotal(TopVoter.Monthly)).setOnline(isOnline()).send(this);
	}

	public void giveOfflineOtherRewards() {
		// old reward api, will be removed in the future
		ArrayList<String> offlineRewards = getOfflineOtherRewards();
		for (String str : offlineRewards) {
			if (str.equalsIgnoreCase("FirstVote")) {
				SpecialRewards.getInstance().giveFirstVoteRewards(this, false);
			} else if (str.equalsIgnoreCase("AllSites")) {
				SpecialRewards.getInstance().giveAllSitesRewards(this, false);
			} else if (str.equalsIgnoreCase("VoteParty")) {
				VoteParty.getInstance().giveReward(this, false);
			} else if (str.contains("Cumulative")) {
				String st = str.substring("Cumulative".length());
				if (StringParser.getInstance().isInt(st)) {
					int votesRequired = Integer.parseInt(st);
					if (votesRequired != 0) {
						if (SpecialRewardsConfig.getInstance().getCumulativeRewardEnabled(votesRequired)) {
							SpecialRewards.getInstance().giveCumulativeVoteReward(this, false, votesRequired);
						}
					}
				}
			} else if (str.contains("MileStone")) {
				String st = str.substring("MileStone".length());
				if (StringParser.getInstance().isInt(st)) {
					int votesRequired = Integer.parseInt(st);
					if (votesRequired > 0) {
						if (SpecialRewardsConfig.getInstance().getMilestoneRewardEnabled(votesRequired)) {
							SpecialRewards.getInstance().giveMilestoneVoteReward(this, true, votesRequired);
						}
					}
				}
			} else if (str.contains("VoteStreak")) {
				String[] args = str.split("_");
				if (args.length > 2) {
					String type = args[1];
					String st = args[2];

					if (SpecialRewardsConfig.getInstance().getVoteStreakRewardEnabled(type, st)) {
						SpecialRewards.getInstance().giveVoteStreakReward(this, false, type, "" + st, -1);
					}

				}
			} else {
				plugin.debug("Reward handle for " + str + " does not exist!");
			}

		}
		if (!offlineRewards.isEmpty()) {
			setOfflineOtherRewards(new ArrayList<String>());
		}
	}

	public void giveWeeklyTopVoterAward(int place, String path) {
		new RewardBuilder(SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getWeeklyAwardRewardsPath(path)).withPlaceHolder("place", "" + place)
						.withPlaceHolder("topvoter", "Weekly").withPlaceHolder("votes", "" + getTotal(TopVoter.Weekly))
						.setOnline(isOnline()).send(this);
	}

	/**
	 * Checks for gotten first vote.
	 *
	 * @return true if user got the first vote reward
	 */
	public boolean hasGottenFirstVote() {
		if (BungeeSettings.getInstance().isUseBungeecoord()
				&& BungeeHandler.getInstance().getMethod().equals(BungeeMethod.PLUGINMESSAGING)) {
			return getTotal(TopVoter.AllTime) > 1;
		}
		return getTotal(TopVoter.AllTime) != 0;
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
		if (Config.getInstance().getVoteRemindingRemindOnLogin()) {
			VoteReminding.getInstance().runRemindLogin(this);
		}
	}

	public String voteCommandNextInfo(VoteSite voteSite) {
		String info = new String();

		long time = getTime(voteSite);
		LocalDateTime now = TimeChecker.getInstance().getTime();
		;
		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(Main.plugin.getOptions().getTimeHourOffSet());

		if (!voteSite.isVoteDelayDaily()) {
			double votedelay = voteSite.getVoteDelay();
			if (votedelay == 0 && voteSite.getVoteDelayMin() == 0) {
				String errorMsg = Config.getInstance().getFormatCommandsVoteNextInfoError();
				info = errorMsg;
			} else {

				LocalDateTime nextvote = lastVote.plusHours((long) votedelay)
						.plusMinutes((long) voteSite.getVoteDelayMin());

				if (time == 0 || now.isAfter(nextvote)) {
					info = Config.getInstance().getFormatCommandsVoteNextInfoCanVote();
				} else {
					Duration dur = Duration.between(now, nextvote);

					long diffHours = dur.getSeconds() / (60 * 60);
					long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

					String timeMsg = Config.getInstance().getFormatCommandsVoteNextInfoTime();
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Long.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;

				}
			}
		} else {
			LocalDateTime offsetoclockyesterday = TimeChecker.getInstance().getTime().plusDays(-1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktoday = TimeChecker.getInstance().getTime().withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktomorrow = TimeChecker.getInstance().getTime().plusDays(1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());

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

					String timeMsg = Config.getInstance().getFormatCommandsVoteNextInfoVoteDelayDaily();
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Integer.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					info = Config.getInstance().getFormatCommandsVoteNextInfoCanVote();
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

					String timeMsg = Config.getInstance().getFormatCommandsVoteNextInfoVoteDelayDaily();
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Integer.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					info = Config.getInstance().getFormatCommandsVoteNextInfoCanVote();
				}
			}
		}
		return info;
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
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsDay()), "amount", "" + diffDays);
				info += " ";
			} else if (diffDays > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsDays()), "amount", "" + diffDays);
				info += " ";
			}

			if (diffHours == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsHour()), "amount", "" + diffHours);
				info += " ";
			} else if (diffHours > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsHours()), "amount", "" + diffHours);
				info += " ";
			}

			if (diffMinutes == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsMinute()), "amount", "" + diffMinutes);
				info += " ";
			} else if (diffMinutes > 1) {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						Config.getInstance().getFormatTimeFormatsMinutes()), "amount", "" + diffMinutes);
				info += " ";
			}

			if (diffSeconds == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsSecond()), "amount", "" + diffSeconds);
			} else {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						Config.getInstance().getFormatTimeFormatsSeconds()), "amount", "" + diffSeconds);
			}

			info = StringParser.getInstance()
					.replacePlaceHolder(Config.getInstance().getFormatCommandsVoteLastLastVoted(), "times", info);

			return info;
		}
		return Config.getInstance().getFormatCommandsVoteLastNeverVoted();
	}

	public long voteNextDurationTime(VoteSite voteSite) {
		long time = getTime(voteSite);
		LocalDateTime now = TimeChecker.getInstance().getTime();
		;
		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(Main.plugin.getOptions().getTimeHourOffSet());

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
			LocalDateTime offsetoclockyesterday = TimeChecker.getInstance().getTime().plusDays(-1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktoday = TimeChecker.getInstance().getTime().withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());
			LocalDateTime offsetoclocktomorrow = TimeChecker.getInstance().getTime().plusDays(1).withHour(0)
					.withMinute(0).plusHours((long) voteSite.getTimeOffSet());

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

	public String voteCommandLastLine(VoteSite voteSite) {
		String timeString = voteCommandLastDate(voteSite);
		String timeSince = voteCommandLastDuration(voteSite);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return StringParser.getInstance().replacePlaceHolder(Config.getInstance().getFormatCommandsVoteLastLine(),
				placeholders);
	}

	/**
	 * Vote command last date.
	 *
	 * @param user     the user
	 * @param voteSite the vote site
	 * @return the string
	 */
	@Deprecated
	public String voteCommandLastDate(VoteSite voteSite) {
		long time = getTime(voteSite);
		if (time > 0) {
			Date date = new Date(time);
			String timeString = new SimpleDateFormat(Config.getInstance().getFormatTimeFormat()).format(date);
			if (StringParser.getInstance().containsIgnorecase(timeString, "YamlConfiguration")) {
				plugin.getLogger().warning("Detected issue parsing time, check time format");
			}
			return timeString;
		}
		return "";
	}

	/**
	 * Off vote.
	 */
	public void offVote() {
		if (!Main.plugin.getOptions().isProcessRewards()) {
			Main.plugin.debug("Processing rewards is disabled");
			return;
		}

		Main.plugin.extraDebug("Checking offline vote site votes");
		Player player = getPlayer();
		if (player != null) {
			boolean topVoterIngorePerm = player.hasPermission("VotingPlugin.TopVoter.Ignore");
			if (isTopVoterIgnore() != topVoterIngorePerm) {
				setTopVoterIgnore(topVoterIngorePerm);
			}
			ArrayList<String> offlineVotes = getOfflineVotes();
			// plugin.debug(ArrayUtils.getInstance().makeStringList(offlineVotes));
			if (offlineVotes.size() > 0) {
				sendVoteEffects(true);
				setOfflineVotes(new ArrayList<String>());
			}

			for (int i = 0; i < offlineVotes.size(); i++) {
				if (plugin.hasVoteSite(offlineVotes.get(i))) {
					plugin.debug("Giving offline site reward: " + offlineVotes.get(i));
					playerVote(plugin.getVoteSite(offlineVotes.get(i)), false, true, false);
				} else {
					plugin.debug("Site doesn't exist: " + offlineVotes.get(i));
				}
			}

			giveOfflineOtherRewards();
		}
	}

	public void playerVote(VoteSite voteSite, boolean online, boolean broadcast, boolean bungee) {
		if (Config.getInstance().getFormatBroadcastWhenOnline() && Config.getInstance().isBroadcastVotesEnabled()
				&& broadcast) {
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
		RewardHandler.getInstance().giveReward(this, SpecialRewardsConfig.getInstance().getData(),
				SpecialRewardsConfig.getInstance().getAnySiteRewardsPath(), new RewardOptions().setOnline(online));
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
		getUserData().setStringList("GottenMileStones", data);
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

	public void setLastCoolDownCheckArray(HashMap<VoteSite, Boolean> lastVotesCheck) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Boolean> entry : lastVotesCheck.entrySet()) {
			String str = entry.getKey().getKey() + "//" + entry.getValue().booleanValue();
			data.add(str);
		}
		getUserData().setStringList("LastVoteCoolDownCheck", data);
	}

	public void setLastMonthTotal(int total) {
		getData().setInt("LastMonthTotal", total);
	}

	public void setLastVoteCoolDownCheck(boolean lastDelay, VoteSite voteSite) {
		HashMap<VoteSite, Boolean> array = getLastCoolDownCheckArray();
		array.put(voteSite, Boolean.valueOf(lastDelay));
		setLastCoolDownCheckArray(array);
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

	public void setOfflineOtherRewards(ArrayList<String> offlineOtherRewards) {
		getUserData().setStringList("OfflineOtherRewards", offlineOtherRewards);
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
		getUserData().setInt("Points", value, !BungeeSettings.getInstance().isUseBungeecoord());
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
			if (Config.getInstance().isLimitMonthlyVotes()) {
				LocalDateTime time = TimeChecker.getInstance().getTime();
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

	public boolean voteStreakUpdatedToday(LocalDateTime time) {
		return MiscUtils.getInstance().getTime(getDayVoteStreakLastUpdate()).getDayOfYear() == time.getDayOfYear();
	}

}
