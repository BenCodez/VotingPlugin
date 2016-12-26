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

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Objects.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
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

	public synchronized void addMilestoneTotal() {
		setMileStoneTotal(getMileStoneTotal() + 1);
	}

	public synchronized int getMileStoneTotal() {
		return getUserData().getInt("MileStoneTotal");
	}

	public synchronized void setMileStoneTotal(int mileStoneTotal) {
		getUserData().setInt("MileStoneTotal", mileStoneTotal);
	}

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

	public synchronized void addAllTimeTotal() {
		setAllTimeTotal(getAllTimeTotal() + 1);
	}

	/**
	 * Adds the points.
	 */
	public synchronized void addPoints() {
		setPoints(getPoints() + 1);
	}

	/**
	 * Adds the points.
	 *
	 * @param value
	 *            the value
	 */
	public synchronized void addPoints(int value) {
		setPoints(getPoints() + value);
	}

	/**
	 * Adds the total.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotal(VoteSite voteSite) {
		setTotal(voteSite, getTotal(voteSite) + 1);
		addAllTimeTotal();
		addMilestoneTotal();
	}

	/**
	 * Adds the total daily.
	 *
	 *
	 */
	public synchronized void addTotalDaily() {
		if (Config.getInstance().getDailyAwardsEnabled()) {
			setDailyTotal(getDailyTotal() + 1);
		}
	}

	/**
	 * Adds the total weekly.
	 *
	 *
	 */
	public synchronized void addTotalWeekly() {
		if (Config.getInstance().getWeeklyAwardsEnabled()) {
			setWeeklyTotal(getWeeklyTotal() + 1);
		}
	}

	/**
	 * Can vote all.
	 *
	 * @return true, if successful
	 */
	public synchronized boolean canVoteAll() {
		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		for (VoteSite voteSite : voteSites) {
			boolean canVote = canVoteSite(voteSite);
			if (!canVote) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Can vote site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return true, if successful
	 */
	public synchronized boolean canVoteSite(VoteSite voteSite) {
		String siteName = voteSite.getSiteName();
		long time = getTime(voteSite);
		if (time == 0) {
			return true;
		}
		LocalDateTime date = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault());

		int votedelay = ConfigVoteSites.getInstance().getVoteDelay(siteName);

		if (votedelay == 0) {
			return false;
		}

		LocalDateTime nextvote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(votedelay);

		return date.isAfter(nextvote);
	}

	/**
	 * Check all votes.
	 *
	 * @return true, if successful
	 */
	public synchronized boolean checkAllVotes() {
		User user = this;

		ArrayList<VoteSite> voteSites = plugin.getVoteSites();
		ArrayList<Integer> months = new ArrayList<Integer>();
		ArrayList<Integer> days = new ArrayList<Integer>();

		for (int i = 0; i < voteSites.size(); i++) {
			long time = user.getTime(voteSites.get(i));
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

	/**
	 * Daily top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void dailyTopVoterAward(int place) {
		if (getPlayerName() == null) {
			setPlayerName(PlayerUtils.getInstance().getPlayerName(getUUID()));
		}

		if (PlayerUtils.getInstance().isPlayerOnline(getPlayerName())) {
			// online
			giveDailyTopVoterAward(place);
		} else {
			addOfflineOtherReward("DailyTopVoter" + place);
		}
	}

	public synchronized int getAllTimeTotal() {
		return getUserData().getInt("AllTimeTotal");
	}

	public synchronized int getDailyTotal() {
		return getUserData().getInt("DailyTotal");
	}

	public synchronized HashMap<String, Boolean> getHasGottenMilestone() {
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

	public synchronized HashMap<VoteSite, Long> getLastVotes() {
		HashMap<VoteSite, Long> lastVotes = new HashMap<VoteSite, Long>();
		ArrayList<String> LastVotesList = getUserData().getStringList("LastVotes");
		for (String str : LastVotesList) {
			String[] data = str.split("//");
			if (data.length > 1) {
				VoteSite site = plugin.getVoteSite(data[0]);
				long time = Long.parseLong(data[1]);
				lastVotes.put(site, time);
			}
		}
		return lastVotes;
	}

	/**
	 * Gets the last vote times sorted.
	 *
	 * @return the last vote times sorted
	 */
	public synchronized HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		HashMap<VoteSite, Long> times = new LinkedHashMap<VoteSite, Long>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			times.put(voteSite, getTime(voteSite));
		}
		HashMap<VoteSite, Long> sorted = (HashMap<VoteSite, Long>) times.entrySet().stream()
				.sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		return sorted;
	}

	public synchronized int getMonthTotal() {
		return getUserData().getInt("MonthTotal");
	}

	public synchronized ArrayList<String> getOfflineOtherRewards() {
		return getUserData().getStringList("OfflineOtherRewards");
	}

	public synchronized ArrayList<String> getOfflineVotes() {
		return getUserData().getStringList("OfflineVotes");
	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public synchronized int getPoints() {
		return getUserData().getInt("Points");
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the time
	 */
	public synchronized long getTime(VoteSite voteSite) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		if (lastVotes.containsKey(voteSite)) {
			return lastVotes.get(voteSite);
		}
		return 0;
	}

	public synchronized int getTotal(VoteSite voteSite) {
		HashMap<VoteSite, Integer> voteSiteTotal = getVoteSiteTotal();
		if (voteSiteTotal.containsKey(voteSite)) {
			return voteSiteTotal.get(voteSite);
		}
		return 0;
	}

	public synchronized int getVotePartyVotes() {
		return getUserData().getInt("VotePartyVotes");
	}

	public synchronized HashMap<VoteSite, Integer> getVoteSiteTotal() {
		HashMap<VoteSite, Integer> voteSiteTotal = new HashMap<VoteSite, Integer>();
		ArrayList<String> voteTotalList = getUserData().getStringList("VoteSiteTotals");
		for (String str : voteTotalList) {
			String[] data = str.split("//");
			if (data.length > 1) {
				VoteSite site = plugin.getVoteSite(data[0]);
				int total = Integer.parseInt(data[1]);
				voteSiteTotal.put(site, total);
			}
		}
		return voteSiteTotal;
	}

	public synchronized int getWeeklyTotal() {
		return getUserData().getInt("WeeklyTotal");
	}

	/**
	 * Give daily top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void giveDailyTopVoterAward(int place) {
		for (String reward : Config.getInstance().getDailyAwardRewards(place)) {
			RewardHandler.getInstance().giveReward(this, reward);
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(getUUID()));
		if (player != null) {
			player.sendMessage(StringUtils.getInstance()
					.colorize(Config.getInstance().getFormatTopVoterRewardMsg().replace("%place%", "" + place)));
		}
	}

	/**
	 * Give monthly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void giveMonthlyTopVoterAward(int place) {
		for (String reward : Config.getInstance().getMonthlyAwardRewards(place)) {
			RewardHandler.getInstance().giveReward(this, reward);
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(getUUID()));
		if (player != null) {
			player.sendMessage(StringUtils.getInstance()
					.colorize(Config.getInstance().getFormatTopVoterRewardMsg().replace("%place%", "" + place)));
		}
	}

	/**
	 * Give weekly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void giveWeeklyTopVoterAward(int place) {
		for (String reward : Config.getInstance().getWeeklyAwardRewards(place)) {
			RewardHandler.getInstance().giveReward(this, reward);
		}
		Player player = Bukkit.getPlayer(java.util.UUID.fromString(getUUID()));
		if (player != null) {
			player.sendMessage(StringUtils.getInstance()
					.colorize(Config.getInstance().getFormatTopVoterRewardMsg().replace("%place%", "" + place)));
		}
	}

	/**
	 * Checks for gotten first vote.
	 *
	 * @return true if user got the first vote reward
	 */
	public synchronized boolean hasGottenFirstVote() {
		return getAllTimeTotal() != 0;
	}

	/**
	 * Checks for gotten milestone.
	 *
	 * @param votesRequired
	 *            the votes required
	 * @return true, if successful
	 */
	public synchronized boolean hasGottenMilestone(int votesRequired) {
		HashMap<String, Boolean> hasGottenMilestone = getHasGottenMilestone();
		if (hasGottenMilestone.containsKey("" + votesRequired)) {
			return hasGottenMilestone.get("" + votesRequired);
		}
		return false;
	}

	public synchronized void loadFromOldData() {
		setPoints(getData().getData(getUUID()).getInt("VotingPlugin.Points", 0));
		for (VoteSite site : plugin.getVoteSites()) {
			setTime(site, getData().getData(getUUID()).getLong("VotingPlugin.VoteLast." + site.getSiteName()));
			setTotal(site, getData().getData(getUUID()).getInt("VotingPlugin.Total." + site.getSiteName()));
		}

		setAllTimeTotal(getData().getData(getUUID()).getInt("VotingPlugin.AllTimeTotal"));

		if (getData().getData(getUUID()).isConfigurationSection("VotingPlugin.MilestonesGiven")) {
			for (String data : getData().getData(getUUID()).getConfigurationSection("VotingPlugin.MilestoneGiven")
					.getKeys(false)) {
				if (getData().getData(getUUID()).getBoolean("VotingPlugin.MilestonesGiven." + data)) {
					setHasGotteMilestone(Integer.parseInt(data), true);
				}
			}
		}
	}

	public synchronized boolean isTopVoterIgnore() {
		return Boolean.valueOf(getUserData().getString("TopVoterIgnore"));
	}

	public synchronized void setTopVoterIgnore(boolean topVoterIgnore) {
		getUserData().setString("TopVoterIgnore", "" + topVoterIgnore);
	}

	public synchronized boolean isReminded() {
		return Boolean.valueOf(getUserData().getString("Reminded"));
	}

	/**
	 * Login message.
	 */
	public void loginMessage() {
		if (Config.getInstance().getVoteRemindingRemindOnLogin()) {
			VoteReminding.getInstance().runRemind(this);
		}
	}

	/**
	 * Monthly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void monthlyTopVoterAward(int place) {
		if (getPlayerName() == null) {
			setPlayerName(PlayerUtils.getInstance().getPlayerName(getUUID()));
		}

		if (PlayerUtils.getInstance().isPlayerOnline(getPlayerName())) {
			// online
			giveMonthlyTopVoterAward(place);
		} else {
			addOfflineOtherReward("MonthlyTopVoter" + place);
		}
	}

	/**
	 * Off vote.
	 */
	public synchronized void offVote() {
		Player player = getPlayer();
		if (player != null) {
			setTopVoterIgnore(player.hasPermission("VotingPlugin.TopVoter.Ignore"));
			ArrayList<String> offlineVotes = getOfflineVotes();
			if (offlineVotes.size() > 0) {
				sendVoteEffects(true);
			}

			for (int i = 0; i < offlineVotes.size(); i++) {
				playerVote(plugin.getVoteSite(offlineVotes.get(i)), false, true);
			}
			setOfflineVotes(new ArrayList<String>());

			for (String str : getOfflineOtherRewards()) {
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
				} else if (str.contains("MontlyTopVoter")) {
					String st = str.substring("MontlyTopVoter".length());
					if (StringUtils.getInstance().isInt(st)) {
						int place = Integer.parseInt(st);
						if (place > 0) {
							if (Config.getInstance().getMonthlyAwardsEnabled()) {
								giveMonthlyTopVoterAward(place);
							}
						}
					}
				} else if (str.contains("WeeklyTopVoter")) {
					String st = str.substring("WeeklyTopVoter".length());
					if (StringUtils.getInstance().isInt(st)) {
						int place = Integer.parseInt(st);
						if (place > 0) {
							if (Config.getInstance().getWeeklyAwardsEnabled()) {
								giveWeeklyTopVoterAward(place);
							}
						}
					}
				} else if (str.contains("DailyTopVoter")) {
					String st = str.substring("DailyTopVoter".length());
					if (StringUtils.getInstance().isInt(st)) {
						int place = Integer.parseInt(st);
						if (place > 0) {
							if (Config.getInstance().getDailyAwardsEnabled()) {
								giveDailyTopVoterAward(place);
							}
						}
					}
				} else {
					plugin.debug("Reward handle for " + str + " does not exist!");
				}

			}
			setOfflineOtherRewards(new ArrayList<String>());
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
	public synchronized void playerVote(VoteSite voteSite, boolean online, boolean broadcast) {
		if (Config.getInstance().getBroadCastVotesEnabled() && Config.getInstance().getFormatBroadcastWhenOnline()
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
	public synchronized boolean removePoints(int points) {
		if (getPoints() >= points) {
			setPoints(getPoints() - points);
			return true;
		}
		return false;
	}

	public synchronized void resetDailyTotalVotes() {
		setDailyTotal(0);
	}

	public synchronized void resetMonthlyTotalVotes() {
		for (VoteSite site : plugin.getVoteSites()) {
			setTotal(site, 0);
		}
	}

	public synchronized void resetWeeklyTotalVotes() {
		setWeeklyTotal(0);
	}

	/**
	 * Send vote effects.
	 *
	 * @param online
	 *            the online
	 */
	public synchronized void sendVoteEffects(boolean online) {
		for (String reward : Config.getInstance().getAnySiteRewards()) {
			if (reward != "") {
				RewardHandler.getInstance().giveReward(this, reward);
			}
		}
	}

	public synchronized void setAllTimeTotal(int allTimeTotal) {
		getUserData().setInt("AllTimeTotal", allTimeTotal);
	}

	public synchronized void setDailyTotal(int dailyTotal) {
		getUserData().setInt("DailyTotal", dailyTotal);
	}

	public synchronized void setHasGottenMilestone(HashMap<String, Boolean> hasGottenMilestone) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<String, Boolean> entry : hasGottenMilestone.entrySet()) {
			String str = entry.getKey() + "//" + entry.getValue().booleanValue();
			data.add(str);
		}
		getUserData().setStringList("GottenMileStones", data);
	}

	public synchronized void setLastVotes(HashMap<VoteSite, Long> lastVotes) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Long> entry : lastVotes.entrySet()) {
			String str = entry.getKey().getSiteName() + "//" + entry.getValue().longValue();
			data.add(str);
		}
		getUserData().setStringList("LastVotes", data);
	}

	public synchronized void setMonthTotal(int monthTotal) {
		getUserData().setInt("DailyTotal", monthTotal);
	}

	public synchronized void setOfflineOtherRewards(ArrayList<String> offlineOtherRewards) {
		getUserData().setStringList("OfflineOtherRewards", offlineOtherRewards);
	}

	public synchronized void setOfflineVotes(ArrayList<String> offlineVotes) {
		getUserData().setStringList("OfflineVotes", offlineVotes);
	}

	/**
	 * Sets the points.
	 *
	 * @param value
	 *            the new points
	 */
	public synchronized void setPoints(int value) {
		getUserData().setInt("Points", value);
	}

	public synchronized void setReminded(boolean reminded) {
		getUserData().setString("Reminded", "" + reminded);
	}

	public synchronized void setTotal(VoteSite voteSite, int value) {
		HashMap<VoteSite, Integer> voteSiteTotal = getVoteSiteTotal();
		voteSiteTotal.put(voteSite, value);
		setVoteSiteTotal(voteSiteTotal);
	}

	public synchronized void setVotePartyVotes(int value) {
		getUserData().setInt("VotePartyVotes", value);
	}

	public synchronized void setVoteSiteTotal(HashMap<VoteSite, Integer> voteSiteTotal) {
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Integer> entry : voteSiteTotal.entrySet()) {
			String str = entry.getKey().getSiteName() + "//" + entry.getValue().intValue();
			data.add(str);
		}
		getUserData().setStringList("VoteSiteTotals", data);

	}

	public synchronized void setWeeklyTotal(int weeklyTotal) {
		getUserData().setInt("WeeklyTotal", weeklyTotal);
	}

	/**
	 * Weekly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public synchronized void weeklyTopVoterAward(int place) {
		if (getPlayerName() == null) {
			setPlayerName(PlayerUtils.getInstance().getPlayerName(getUUID()));
		}

		if (PlayerUtils.getInstance().isPlayerOnline(getPlayerName())) {
			// online
			giveWeeklyTopVoterAward(place);
		} else {
			addOfflineOtherReward("WeeklyTopVoter" + place);
		}
	}

	public synchronized void addOfflineOtherReward(String reward) {
		ArrayList<String> offlineOtherRewards = getOfflineOtherRewards();
		offlineOtherRewards.add(reward);
		setOfflineOtherRewards(offlineOtherRewards);
	}

	public synchronized void setHasGotteMilestone(int votesRequired, boolean b) {
		HashMap<String, Boolean> hasGottenMilestone = getHasGottenMilestone();
		hasGottenMilestone.put("" + votesRequired, b);
		setHasGottenMilestone(hasGottenMilestone);
	}

	public synchronized void setTime(VoteSite voteSite) {
		setTime(voteSite, LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
	}

	public synchronized void setTime(VoteSite voteSite, Long time) {
		HashMap<VoteSite, Long> lastVotes = getLastVotes();
		lastVotes.put(voteSite, time);
		setLastVotes(lastVotes);
	}

	public synchronized void addOfflineVote(String voteSiteName) {
		ArrayList<String> offlineVotes = getOfflineVotes();
		offlineVotes.add(voteSiteName);
		setOfflineVotes(offlineVotes);
	}

}
