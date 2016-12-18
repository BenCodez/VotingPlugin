package com.Ben12345rocks.VotingPlugin.Objects;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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

	private int points;

	private ArrayList<String> offlineVotes;

	private ArrayList<String> offlineOtherRewards;

	private int allTimeTotal;
	private int monthTotal;
	private int weeklyTotal;
	private int dailyTotal;
	private HashMap<VoteSite, Long> lastVotes;
	private HashMap<VoteSite, Integer> voteSiteTotal;
	private int votePartyVotes;
	private boolean reminded;
	private HashMap<String, Boolean> hasGottenMilestone;
	private int mileStoneTotal;

	public int getMileStoneTotal() {
		return mileStoneTotal;
	}

	public void setMileStoneTotal(int mileStoneTotal) {
		this.mileStoneTotal = mileStoneTotal;
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
		init();
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
		init();
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
		init();

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
		init();
	}

	public void addAllTimeTotal() {
		setAllTimeTotal(getAllTimeTotal() + 1);
	}

	/**
	 * Adds the points.
	 */
	public void addPoints() {
		setPoints(getPoints() + 1);
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
	 * @param voteSite
	 *            the vote site
	 */
	public void addTotal(VoteSite voteSite) {
		setTotal(voteSite, getTotal(voteSite) + 1);
		addAllTimeTotal();
	}

	/**
	 * Adds the total daily.
	 *
	 *
	 */
	public void addTotalDaily() {
		if (Config.getInstance().getDailyAwardsEnabled()) {
			setDailyTotal(getTotalDaily() + 1);
		}
	}

	/**
	 * Adds the total weekly.
	 *
	 *
	 */
	public void addTotalWeekly() {
		if (Config.getInstance().getWeeklyAwardsEnabled()) {
			setWeeklyTotal(getTotalWeekly() + 1);
		}
	}

	/**
	 * Can vote all.
	 *
	 * @return true, if successful
	 */
	public boolean canVoteAll() {
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
	public boolean canVoteSite(VoteSite voteSite) {
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
	public boolean checkAllVotes() {
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
	public void dailyTopVoterAward(int place) {
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

	public int getAllTimeTotal() {
		return allTimeTotal;
	}

	public int getDailyTotal() {
		return dailyTotal;
	}

	public HashMap<String, Boolean> getHasGottenMilestone() {
		return hasGottenMilestone;
	}

	public HashMap<VoteSite, Long> getLastVotes() {
		return lastVotes;
	}

	/**
	 * Gets the last vote times sorted.
	 *
	 * @return the last vote times sorted
	 */
	public HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		HashMap<VoteSite, Long> times = new HashMap<VoteSite, Long>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			times.put(voteSite, getTime(voteSite));
		}
		HashMap<VoteSite, Long> sorted = (HashMap<VoteSite, Long>) times.entrySet().stream()
				.sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		return sorted;
	}

	public int getMonthTotal() {
		return monthTotal;
	}

	public ArrayList<String> getOfflineOtherRewards() {
		return offlineOtherRewards;
	}

	public ArrayList<String> getOfflineVotes() {
		return offlineVotes;
	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public int getPoints() {
		return points;
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the time
	 */
	public long getTime(VoteSite voteSite) {
		return lastVotes.get(voteSite);
	}

	public int getTotal(VoteSite voteSite) {
		if (voteSiteTotal.containsKey(voteSite)) {
			return voteSiteTotal.get(voteSite);
		}
		return 0;
	}

	/**
	 * Gets the total daily all.
	 *
	 * @return the total daily all
	 */

	public int getTotalDaily() {
		return dailyTotal;

	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public int getTotalVotes() {
		return monthTotal;
	}

	/**
	 * Gets the total votes today.
	 *
	 * @return the total votes today
	 */
	public int getTotalVotesToday() {
		return dailyTotal;

	}

	/**
	 * Gets the total weekly all.
	 *
	 * @return the total weekly all
	 */
	public int getTotalWeekly() {
		return weeklyTotal;
	}

	public int getVotePartyVotes() {
		return votePartyVotes;
	}

	public HashMap<VoteSite, Integer> getVoteSiteTotal() {
		return voteSiteTotal;
	}

	public int getWeeklyTotal() {
		return weeklyTotal;
	}

	/**
	 * Give daily top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void giveDailyTopVoterAward(int place) {
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
	public void giveMonthlyTopVoterAward(int place) {
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
	public void giveWeeklyTopVoterAward(int place) {
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
	public boolean hasGottenFirstVote() {
		return allTimeTotal != 0;
	}

	/**
	 * Checks for gotten milestone.
	 *
	 * @param votesRequired
	 *            the votes required
	 * @return true, if successful
	 */
	public boolean hasGottenMilestone(int votesRequired) {
		if (hasGottenMilestone.containsKey("" + votesRequired)) {
			return hasGottenMilestone.get("" + votesRequired);
		}
		return false;
	}

	/**
	 * Checks for top voter ignore permission.
	 *
	 * @return true, if successful
	 */
	public boolean hasTopVoterIgnorePermission() {
		// NEED TO USE VAULT PERMISIONS
		return false;
	}

	@Override
	public void init() {
		mileStoneTotal = getUserData().getInt("MileStoneTotal");
		points = getUserData().getInt("Points");
		offlineOtherRewards = getUserData().getStringList("OfflineOtherRewards");
		allTimeTotal = getUserData().getInt("AllTimeTotal");
		monthTotal = getUserData().getInt("MonthTotal");
		weeklyTotal = getUserData().getInt("WeeklyTotal");
		dailyTotal = getUserData().getInt("DailyTotal");
		votePartyVotes = getUserData().getInt("VotePartyVotes");
		reminded = Boolean.valueOf(getUserData().getString("Reminded"));

		offlineVotes = getUserData().getStringList("OfflineVotes");

		lastVotes = new HashMap<VoteSite, Long>();
		ArrayList<String> LastVotesList = getUserData().getStringList("LastVotes");
		for (String str : LastVotesList) {
			String[] data = str.split("//");
			VoteSite site = plugin.getVoteSite(data[0]);
			long time = Long.parseLong(data[1]);
			lastVotes.put(site, time);
		}

		hasGottenMilestone = new HashMap<String, Boolean>();
		ArrayList<String> milestoneList = getUserData().getStringList("GottenMileStones");
		for (String str : milestoneList) {
			String[] data = str.split("//");
			boolean gotten = Boolean.parseBoolean(data[1]);
			hasGottenMilestone.put(data[0], gotten);
		}

		voteSiteTotal = new HashMap<VoteSite, Integer>();
		ArrayList<String> voteTotalList = getUserData().getStringList("VoteSiteTotals");
		for (String str : voteTotalList) {
			String[] data = str.split("//");
			VoteSite site = plugin.getVoteSite(data[0]);
			int total = Integer.parseInt(data[0]);
			voteSiteTotal.put(site, total);
		}
	}

	public boolean isReminded() {
		return reminded;
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
	public void monthlyTopVoterAward(int place) {
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
	public void offVote() {
		Player player = getPlayer();
		if (player != null) {
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
	public void playerVote(VoteSite voteSite, boolean online, boolean broadcast) {
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
		for (VoteSite site : plugin.getVoteSites()) {
			setTotal(site, 0);
		}
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
		for (String reward : Config.getInstance().getAnySiteRewards()) {
			if (reward != "") {
				RewardHandler.getInstance().giveReward(this, reward);
			}
		}
	}

	public void setAllTimeTotal(int allTimeTotal) {
		this.allTimeTotal = allTimeTotal;
		getUserData().setInt("AllTimeTotal", allTimeTotal);
	}

	public void setDailyTotal(int dailyTotal) {
		this.dailyTotal = dailyTotal;
		getUserData().setInt("DailyTotal", dailyTotal);
	}

	public void setHasGottenMilestone(HashMap<String, Boolean> hasGottenMilestone) {
		this.hasGottenMilestone = hasGottenMilestone;
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<String, Boolean> entry : hasGottenMilestone.entrySet()) {
			String str = entry.getKey() + "//" + entry.getValue().booleanValue();
			data.add(str);
		}
		getUserData().setStringList("GottenMileStones", data);
	}

	public void setLastVotes(HashMap<VoteSite, Long> lastVotes) {
		this.lastVotes = lastVotes;
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Long> entry : lastVotes.entrySet()) {
			String str = entry.getKey().getSiteName() + "//" + entry.getValue().longValue();
			data.add(str);
		}
		getUserData().setStringList("LastVotes", data);
	}

	public void setMonthTotal(int monthTotal) {
		this.monthTotal = monthTotal;
		getUserData().setInt("DailyTotal", monthTotal);
	}

	public void setOfflineOtherRewards(ArrayList<String> offlineOtherRewards) {
		this.offlineOtherRewards = offlineOtherRewards;
		getUserData().setStringList("OfflineOtherRewards", offlineOtherRewards);
	}

	public void setOfflineVotes(ArrayList<String> offlineVotes) {
		this.offlineVotes = offlineVotes;
		getUserData().setStringList("OfflineVotes", offlineVotes);
	}

	/**
	 * Sets the points.
	 *
	 * @param value
	 *            the new points
	 */
	public void setPoints(int value) {
		points = value;
		getUserData().setInt("Points", value);
	}

	public void setReminded(boolean reminded) {
		this.reminded = reminded;
		getUserData().setString("Reminded", "" + reminded);
	}

	public void setTotal(VoteSite voteSite, int value) {
		voteSiteTotal.put(voteSite, value);
		setVoteSiteTotal(getVoteSiteTotal());
	}

	public void setVotePartyVotes(int value) {
		votePartyVotes = value;
		getUserData().setInt("VotePartyVotes", value);
	}

	public void setVoteSiteTotal(HashMap<VoteSite, Integer> voteSiteTotal) {
		this.voteSiteTotal = voteSiteTotal;
		ArrayList<String> data = new ArrayList<String>();
		for (Entry<VoteSite, Integer> entry : voteSiteTotal.entrySet()) {
			String str = entry.getKey().getSiteName() + "//" + entry.getValue().intValue();
			data.add(str);
		}
		getUserData().setStringList("VoteSiteTotals", data);

	}

	public void setWeeklyTotal(int weeklyTotal) {
		this.weeklyTotal = weeklyTotal;
		getUserData().setInt("WeeklyTotal", weeklyTotal);
	}

	/**
	 * Weekly top voter award.
	 *
	 * @param place
	 *            the place
	 */
	public void weeklyTopVoterAward(int place) {
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

	public void addOfflineOtherReward(String reward) {
		offlineOtherRewards.add(reward);
		setOfflineOtherRewards(getOfflineOtherRewards());
	}

	public void setHasGotteMilestone(int votesRequired, boolean b) {
		hasGottenMilestone.put("" + votesRequired, b);
		setHasGottenMilestone(getHasGottenMilestone());
	}

	public void setTime(VoteSite voteSite) {
		lastVotes.put(voteSite, LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
		setLastVotes(getLastVotes());
	}

	public void addOfflineVote(String voteSiteName) {
		offlineVotes.add(voteSiteName);
		setOfflineVotes(getOfflineVotes());
	}

}
