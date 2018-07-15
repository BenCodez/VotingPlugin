package com.Ben12345rocks.VotingPlugin.Objects;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Objects.RewardBuilder;
import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.VoteParty.VoteParty;
import com.Ben12345rocks.VotingPlugin.VoteReminding.VoteReminding;

// TODO: Auto-generated Javadoc
/**
 * The Class User.
 */
public class User extends com.Ben12345rocks.AdvancedCore.Objects.User {

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Instantiates a new user.
	 *
	 * @param player
	 *            the player
	 */
	@Deprecated
	public User(Player player) {
		super(plugin, player);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param playerName
	 *            the player name
	 */
	@Deprecated
	public User(String playerName) {
		super(plugin, playerName);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid
	 *            the uuid
	 */
	@Deprecated
	public User(UUID uuid) {
		super(plugin, uuid);
	}

	/**
	 * Instantiates a new user.
	 *
	 * @param uuid
	 *            the uuid
	 * @param loadName
	 *            the load name
	 */
	@Deprecated
	public User(UUID uuid, boolean loadName) {
		super(plugin, uuid, loadName);
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
	 * @param value
	 *            the value
	 */
	public void addPoints(int value) {
		setPoints(getPoints() + value);
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

	public boolean getToggleBroadcast() {
		return getUserData().getBoolean("ToggleBroadcast");
	}

	public void setToggleBroadcast(boolean value) {
		getUserData().setBoolean("ToggleBroadcast", value);
	}

	/**
	 * Can vote site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return true, if successful
	 */
	public boolean canVoteSite(VoteSite voteSite) {

		String siteName = voteSite.getKey();
		long time = getTime(voteSite);
		if (time == 0) {
			return true;
		}
		LocalDateTime now = LocalDateTime.now();
		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault());

		if (!voteSite.isVoteDelayDaily()) {
			int votedelay = ConfigVoteSites.getInstance().getVoteDelay(siteName);

			if (votedelay == 0) {
				return false;
			}

			LocalDateTime nextvote = lastVote.plusHours(votedelay);

			return now.isAfter(nextvote);
		} else {
			if (now.getDayOfYear() != lastVote.getDayOfYear() || now.getYear() != lastVote.getYear()) {
				return true;
			}
			return false;
		}

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

	public void clearOfflineRewards() {
		setOfflineVotes(new ArrayList<String>());
		setOfflineRewards(new ArrayList<String>());
		setOfflineOtherRewards(new ArrayList<String>());
	}

	public void clearTotals() {
		setAllTimeTotal(0);
		resetDailyTotalVotes();
		resetMonthlyTotalVotes();
		setWeeklyTotal(0);
	}

	public int getAllTimeTotal() {
		return getUserData().getInt("AllTimeTotal");
	}

	public int getBestDayVoteStreak() {
		return getData().getInt("BestDayVoteStreak");
	}

	public int getBestMonthVoteStreak() {
		return getData().getInt("BestMonthVoteStreak");
	}

	public int getBestWeekVoteStreak() {
		return getData().getInt("BestWeekVoteStreak");
	}

	public int getDailyTotal() {
		return getUserData().getInt("DailyTotal");
	}

	public int getDayVoteStreak() {
		return getData().getInt("DayVoteStreak");
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
		return getData().getInt("HighestDailyTotal");
	}

	public int getHighestMonthlyTotal() {
		return getData().getInt("HighestMonthlyTotal");
	}

	public int getHighestWeeklyTotal() {
		return getData().getInt("HighestWeeklyTotal");
	}

	public int getLastMonthTotal() {
		return getData().getInt("LastMonthTotal");
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
		LinkedHashMap<VoteSite, Long> sorted = (LinkedHashMap<VoteSite, Long>) times.entrySet().stream()
				.sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		return sorted;
	}

	public int getMilestoneCount() {
		return getData().getInt("MilestoneCount", getAllTimeTotal());
	}

	public int getMonthTotal() {
		return getData().getInt("MonthTotal");
	}

	public int getMonthVoteStreak() {
		return getData().getInt("MonthVoteStreak");
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
		return getUserData().getInt("Points");
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the time
	 */
	public long getTime(VoteSite voteSite) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		if (lastVotes.containsKey(voteSite)) {
			return lastVotes.get(voteSite);
		}
		return 0;
	}

	public int getVotePartyVotes() {
		return getUserData().getInt("VotePartyVotes");
	}

	public int getWeeklyTotal() {
		return getUserData().getInt("WeeklyTotal");
	}

	public int getWeekVoteStreak() {
		return getData().getInt("WeekVoteStreak");
	}

	public void giveDailyTopVoterAward(int place, String path) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getDailyAwardRewardsPath(path))
				.withPlaceHolder("place", "" + place).withPlaceHolder("topvoter", "Daily").send(this);
	}

	public void giveMonthlyTopVoterAward(int place, String path) {
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getMonthlyAwardRewardsPath(path))
				.withPlaceHolder("place", "" + place).withPlaceHolder("topvoter", "Monthly").send(this);
	}

	public void giveOfflineOtherRewards() {
		ArrayList<String> offlineRewards = getOfflineOtherRewards();
		for (String str : offlineRewards) {
			if (str.equalsIgnoreCase("FirstVote")) {
				OtherVoteReward.getInstance().giveFirstVoteRewards(this, false);
			} else if (str.equalsIgnoreCase("AllSites")) {
				OtherVoteReward.getInstance().giveAllSitesRewards(this, false);
			} else if (str.equalsIgnoreCase("VoteParty")) {
				VoteParty.getInstance().giveReward(this);
			} else if (str.contains("Cumulative")) {
				String st = str.substring("Cumulative".length());
				if (StringUtils.getInstance().isInt(st)) {
					int votesRequired = Integer.parseInt(st);
					if (votesRequired != 0) {
						if (Config.getInstance().getCumulativeRewardEnabled(votesRequired)) {
							OtherVoteReward.getInstance().giveCumulativeVoteReward(this, false, votesRequired);
						}
					}
				}
			} else if (str.contains("MileStone")) {
				String st = str.substring("MileStone".length());
				if (StringUtils.getInstance().isInt(st)) {
					int votesRequired = Integer.parseInt(st);
					if (votesRequired > 0) {
						if (Config.getInstance().getMilestoneRewardEnabled(votesRequired)) {
							OtherVoteReward.getInstance().giveMilestoneVoteReward(this, true, votesRequired);
						}
					}
				}
			} else if (str.contains("VoteStreak")) {
				String[] args = str.split("_");
				if (args.length > 2) {
					String type = args[1];
					String st = args[2];

					if (Config.getInstance().getVoteStreakRewardEnabled(type, st)) {
						OtherVoteReward.getInstance().giveVoteStreakReward(this, false, type, "" + st, -1);
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
		new RewardBuilder(Config.getInstance().getData(), Config.getInstance().getWeeklyAwardRewardsPath(path))
				.withPlaceHolder("place", "" + place).withPlaceHolder("topvoter", "Weekly").send(this);
	}

	/**
	 * Checks for gotten first vote.
	 *
	 * @return true if user got the first vote reward
	 */
	public boolean hasGottenFirstVote() {
		return getAllTimeTotal() != 0;
	}

	/**
	 * Checks for gotten milestone.
	 *
	 * @param votesRequired
	 *            the votes required
	 * @return true, if successful
	 */
	public boolean hasGottenMilestone(int votesRequired) {
		HashMap<String, Boolean> hasGottenMilestone = getHasGottenMilestone();
		if (hasGottenMilestone.containsKey("" + votesRequired)) {
			return hasGottenMilestone.get("" + votesRequired);
		}
		return false;
	}

	public boolean isReminded() {
		return Boolean.valueOf(getUserData().getString("Reminded"));
	}

	public boolean isTopVoterIgnore() {
		return Boolean.valueOf(getUserData().getString("TopVoterIgnore"));
	}

	/**
	 * Login message.
	 */
	public void loginMessage() {
		if (Config.getInstance().getVoteRemindingRemindOnLogin()) {
			VoteReminding.getInstance().runRemindLogin(this);
		}
	}

	/**
	 * Off vote.
	 */
	public void offVote() {
		AdvancedCoreHook.getInstance().extraDebug("Checking offline vote site votes");
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
			}

			for (int i = 0; i < offlineVotes.size(); i++) {
				if (plugin.hasVoteSite(offlineVotes.get(i))) {
					plugin.debug("Giving offline site reward: " + offlineVotes.get(i));
					playerVote(plugin.getVoteSite(offlineVotes.get(i)), false, true);
				} else {
					plugin.debug("Site doesn't exist: " + offlineVotes.get(i));
				}
			}

			setOfflineVotes(new ArrayList<String>());

			giveOfflineOtherRewards();
		}
	}

	/**
	 * Player vote.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param online
	 *            the online
	 * @param broadcast
	 *            the broadcast
	 */
	public void playerVote(VoteSite voteSite, boolean online, boolean broadcast) {
		if (Config.getInstance().getFormatBroadcastWhenOnline() && Config.getInstance().getBroadCastVotesEnabled()
				&& broadcast) {
			voteSite.broadcastVote(this);
		}
		voteSite.giveSiteReward(this, online);
	}

	/**
	 * Removes the points.
	 *
	 * @param points
	 *            the points
	 * @return true, if successful
	 */
	public boolean removePoints(int points) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points);
			return true;
		}
		return false;
	}

	public void resetDailyTotalVotes() {
		setDailyTotal(0);
	}

	public void resetMonthlyTotalVotes() {
		setLastMonthTotal(getMonthTotal());
		setMonthTotal(0);
	}

	public void resetWeeklyTotalVotes() {
		setWeeklyTotal(0);
	}

	/**
	 * Send vote effects.
	 *
	 * @param online
	 *            the online
	 */
	public void sendVoteEffects(boolean online) {
		RewardHandler.getInstance().giveReward(this, Config.getInstance().getData(),
				Config.getInstance().getAnySiteRewardsPath(), online);
	}

	public void setAllTimeTotal(int allTimeTotal) {
		getUserData().setInt("AllTimeTotal", allTimeTotal);
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

	public void setDailyTotal(int total) {
		getUserData().setInt("DailyTotal", total);
	}

	public void setDayVoteStreak(int streak) {
		getData().setInt("DayVoteStreak", streak);
		if (getBestDayVoteStreak() < streak) {
			setBestDayVoteStreak(streak);
		}
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

	public void setMonthTotal(int total) {
		getData().setInt("MonthTotal", total);
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
	 * @param value
	 *            the new points
	 */
	public void setPoints(int value) {
		getUserData().setInt("Points", value);
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

	public void setVotePartyVotes(int value) {
		getUserData().setInt("VotePartyVotes", value);
	}

	public void setWeeklyTotal(int total) {
		getUserData().setInt("WeeklyTotal", total);
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

}
