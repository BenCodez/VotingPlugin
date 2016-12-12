package com.Ben12345rocks.VotingPlugin.Objects;

import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.TemporalField;
import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
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

	/**
	 * Adds the offline vote.
	 *
	 * @param voteSite
	 *            the vote site
	 */
	public synchronized void addOfflineVote(VoteSite voteSite) {
		setOfflineVotes(voteSite, getOfflineVotes(voteSite) + 1);
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
	public synchronized void addTotal(VoteSite voteSite) {
		setPluginData("Total." + voteSite.getSiteName(), getTotal(voteSite) + 1);
		setAllTimeTotal(getAllTimeTotal() + 1);

	}

	/**
	 * Adds the total daily.
	 *
	 * 
	 */
	public synchronized void addTotalDaily() {
		if (Config.getInstance().getDailyAwardsEnabled()) {
			setTotalDaily(getTotalDailyAll() + 1);
		}
	}

	/**
	 * Adds the total weekly.
	 *
	 * 
	 */
	public synchronized void addTotalWeekly() {
		if (Config.getInstance().getWeeklyAwardsEnabled()) {
			setTotalWeekly(getTotalWeeklyAll() + 1);
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

	public synchronized int getVotePartyVotes() {
		return getPluginData().getInt("VotePartyVotes");
	}

	public synchronized void setVotePartyVotes(int value) {
		setPluginData("VotePartyVotes", value);
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
			setTopVoterAwardOfflineDaily(place);
		}
	}

	public synchronized void setTopVoterAwardOfflineDaily(int place) {
		setPluginData("TopVoter." + LocalDateTime.now().getYear() + "." + LocalDateTime.now().getMonth().toString()
				+ "." + LocalDateTime.now().getDayOfMonth(), place);

	}

	/**
	 * Gets the last vote times sorted.
	 *
	 * @return the last vote times sorted
	 */
	public synchronized HashMap<VoteSite, Long> getLastVoteTimesSorted() {
		HashMap<VoteSite, Long> times = new HashMap<VoteSite, Long>();

		for (VoteSite voteSite : plugin.getVoteSites()) {
			times.put(voteSite, getTime(voteSite));
		}
		HashMap<VoteSite, Long> sorted = (HashMap<VoteSite, Long>) times.entrySet().stream()
				.sorted(Collections.reverseOrder(Map.Entry.comparingByValue()))
				.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
		return sorted;
	}

	/**
	 * Gets the offline milestone votes.
	 *
	 * @return the offline milestone votes
	 */
	@SuppressWarnings("unchecked")
	public synchronized ArrayList<String> getOfflineMilestoneVotes() {
		return (ArrayList<String>) getPluginData().getList("OfflineMilestones", new ArrayList<String>());
	}

	/**
	 * Gets the offline top voter.
	 *
	 * @return the offline top voter
	 */
	public synchronized int getOfflineTopVoter() {
		return getPluginData()
				.getInt("TopVoter." + LocalDateTime.now().getYear() + "." + LocalDateTime.now().getMonth().toString());
	}

	/**
	 * Gets the offline votes.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the offline votes
	 */
	public synchronized int getOfflineVotes(VoteSite voteSite) {
		return getPluginData().getInt("OfflineVotes." + voteSite.getSiteName());
	}

	/**
	 * Gets the points.
	 *
	 * @return the points
	 */
	public synchronized int getPoints() {
		return getPluginData().getInt("Points");
	}

	/**
	 * Gets the reminded.
	 *
	 * @return the reminded
	 */
	public synchronized boolean getReminded() {
		return getPluginData().getBoolean("Reminded");
	}

	/**
	 * Gets the time.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the time
	 */
	public synchronized long getTime(VoteSite voteSite) {
		return getPluginData().getLong("VoteLast." + voteSite.getSiteName());
	}

	/**
	 * Gets the total.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total
	 */
	public synchronized int getTotal(VoteSite voteSite) {
		return getPluginData().getInt("Total." + voteSite.getSiteName());
	}

	/**
	 * Gets the total daily all.
	 *
	 * @return the total daily all
	 */

	public synchronized int getTotalDailyAll() {
		return getPluginData().getInt("TotalDaily");

	}

	public synchronized int getTotalMileStone(int milestone) {
		return getPluginData().getInt("Milestone." + milestone);
	}

	/**
	 * Gets the total votes.
	 *
	 * @return the total votes
	 */
	public synchronized int getTotalVotes() {
		int total = 0;
		for (VoteSite voteSite : plugin.getVoteSites()) {
			total += getTotal(voteSite);
		}
		return total;
	}

	/**
	 * Gets the total votes site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the total votes site
	 */
	@Deprecated
	public synchronized int getTotalVotesSite(VoteSite voteSite) {
		return getTotal(voteSite);
	}

	/**
	 * Gets the total votes today.
	 *
	 * @return the total votes today
	 */
	@SuppressWarnings("deprecation")
	public synchronized int getTotalVotesToday() {
		int total = 0;
		for (VoteSite voteSite : plugin.getVoteSites()) {
			Date date = new Date(getTime(voteSite));
			if (date.getDate() == new Date().getDate()) {
				total++;
			}
		}
		return total;

	}

	/**
	 * Gets the total weekly all.
	 *
	 * @return the total weekly all
	 */
	public synchronized int getTotalWeeklyAll() {
		return getPluginData().getInt("TotalWeekly");

	}

	/**
	 * Gets the vote time last.
	 *
	 * @return the vote time last
	 */
	public synchronized long getVoteTimeLast() {
		ArrayList<Long> times = new ArrayList<Long>();
		for (VoteSite voteSite : plugin.getVoteSites()) {
			times.add(getTime(voteSite));
		}
		Long last = Collections.max(times);
		return last;
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
	 * @return true, if successful
	 */
	public synchronized boolean hasGottenFirstVote() {
		return getPluginData().getBoolean("FirstVoteGotten");
	}

	/**
	 * Checks for gotten milestone.
	 *
	 * @param votesRequired
	 *            the votes required
	 * @return true, if successful
	 */
	public synchronized boolean hasGottenMilestone(int votesRequired) {
		return getPluginData().getBoolean("MilestonesGiven." + votesRequired);
	}

	/**
	 * Checks for top voter ignore permission.
	 *
	 * @return true, if successful
	 */
	public synchronized boolean hasTopVoterIgnorePermission() {
		return getPluginData().getBoolean("TopVoterIgnore");
	}

	/**
	 * Load name.
	 */
	public synchronized void loadName() {
		setPlayerName(PlayerUtils.getInstance().getPlayerName(getUUID()));
	}

	/**
	 * Login message.
	 */
	public synchronized void loginMessage() {
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
			setTopVoterAwardOffline(place);
		}
	}

	private void setTopVoterAwardOffline(int value) {
		setPluginData("TopVoter." + LocalDateTime.now().getYear() + "." + LocalDateTime.now().getMonth().toString(),
				value);

	}

	/**
	 * Off vote.
	 */
	public synchronized void offVote() {
		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		ArrayList<String> offlineVotes = new ArrayList<String>();

		String playerName = getPlayerName();

		Player player = getPlayer();
		if (player != null) {
			setHasTopVoterIgnorePermssion(player.hasPermission("VotingPlugin.TopVoter.Ignore"));
		}

		boolean sendEffects = false;

		for (VoteSite voteSite : voteSites) {
			int offvotes = getOfflineVotes(voteSite);
			if (offvotes > 0) {
				sendEffects = true;

				plugin.debug("Offline Vote Reward on Site '" + voteSite.getSiteName() + "' given for player '"
						+ playerName + "'");

				for (int i = 0; i < offvotes; i++) {
					offlineVotes.add(voteSite.getSiteName());
				}
			}
		}

		if (sendEffects) {
			sendVoteEffects(true);
		}

		for (int i = 0; i < offlineVotes.size(); i++) {
			playerVote(plugin.getVoteSite(offlineVotes.get(i)), false, true);
		}
		for (int i = 0; i < offlineVotes.size(); i++) {
			setOfflineVotes(plugin.getVoteSite(offlineVotes.get(i)), 0);
		}

		int firstVotes = getFirstVoteOffline();
		int allSites = getAllSitesOffline();
		for (int i = 0; i < firstVotes; i++) {
			OtherVoteReward.getInstance().giveFirstVoteRewards(this, false);
		}

		for (int i = 0; i < allSites; i++) {
			OtherVoteReward.getInstance().giveAllSitesRewards(this, false);
		}

		Set<String> list = Config.getInstance().getCumulativeVotes();
		for (String str : list) {
			if (StringUtils.getInstance().isInt(str)) {
				int votesRequired = Integer.parseInt(str);
				if (votesRequired != 0) {
					if (Config.getInstance().getCumulativeRewardEnabled(votesRequired)) {
						int offlineVote = getCumulativeVotesOffline(votesRequired);
						for (int i = 0; i < offlineVote; i++) {
							OtherVoteReward.getInstance().giveCumulativeVoteReward(this, false, votesRequired);

						}
						if (offlineVote != 0) {
							setCumuatliveVotesOffline(votesRequired, 0);
						}
					}
				}
			}
		}

		ArrayList<String> milestones = getOfflineMilestoneVotes();
		for (String str : milestones) {
			if (StringUtils.getInstance().isInt(str)) {
				int votesRequired = Integer.parseInt(str);
				if (votesRequired != 0) {
					if (Config.getInstance().getMilestoneRewardEnabled(votesRequired)) {
						OtherVoteReward.getInstance().giveMilestoneVoteReward(this, true, votesRequired);
					}
				}
			}
		}
		setOfflineMilestoneVotes(new ArrayList<String>());

		if (firstVotes != 0) {
			setFirstVoteOffline(0);
		}
		if (allSites != 0) {
			setAllSitesOffline(0);
		}

		int place = getOfflineTopVoter();
		if (place > 0) {
			giveMonthlyTopVoterAward(place);
			setTopVoterAwardOffline(0);
		}

		if (Config.getInstance().getMonthlyAwardsEnabled()) {
			for (int i = 0; i <= 6; i++) {
				int place1 = getTopVoterAwardOfflineWeekly(i);
				if (place1 > 0) {
					giveWeeklyTopVoterAward(place1);
					setTopVoterAwardOfflineWeekly(0);
				}
			}
		}

		if (Config.getInstance().getDailyAwardsEnabled()) {
			for (int i = 0; i <= 31; i++) {
				int place2 = getTopVoterAwardOfflineDaily(i);
				if (place2 > 0) {
					giveDailyTopVoterAward(place2);
					setDailyTopVoterAwardOffline(0);
				}
			}
		}

		if (VoteParty.getInstance().getOfflineVotePartyVotes(this) > 0) {
			for (int i = VoteParty.getInstance().getOfflineVotePartyVotes(this); i > 0; i--) {
				VoteParty.getInstance().giveReward(this);
			}
			VoteParty.getInstance().setOfflineVotePartyVotes(this, 0);
		}

	}

	public synchronized void setCumuatliveVotesOffline(int votesRequired, int i) {
		setPluginData("OtherRewards.Cumulative." + votesRequired, i);

	}

	public synchronized int getCumulativeVotesOffline(int votesRequired) {
		return getPluginData().getInt("OtherRewards.Cumulative." + votesRequired);
	}

	public synchronized void setDailyTopVoterAwardOffline(int i) {
		setPluginData("TopVoter." + LocalDateTime.now().getYear() + "." + LocalDateTime.now().getMonth().toString()
				+ "." + LocalDateTime.now().getDayOfMonth(), i);

	}

	public synchronized void setTopVoterAwardOfflineWeekly(int i) {
		LocalDate date = LocalDate.now();
		TemporalField woy = WeekFields.of(Locale.getDefault()).weekOfWeekBasedYear();
		int weekNumber = date.get(woy);
		setPluginData("TopVoter." + LocalDateTime.now().getYear() + "." + weekNumber, i);

	}

	public synchronized int getTopVoterAwardOfflineDaily(int i) {
		return getPluginData().getInt("TopVoter." + LocalDateTime.now().getYear() + "."
				+ LocalDateTime.now().getMonth().toString() + "." + LocalDateTime.now().getDayOfMonth());
	}

	public synchronized int getTopVoterAwardOfflineWeekly(int i) {
		LocalDate date = LocalDate.now();
		TemporalField woy = WeekFields.of(Locale.getDefault()).weekOfWeekBasedYear();
		int weekNumber = date.get(woy);
		return getPluginData().getInt("TopVoter." + LocalDateTime.now().getYear() + "." + weekNumber);
	}

	public synchronized void setAllSitesOffline(int value) {
		setPluginData("OtherRewards.AllSites", value);

	}

	public synchronized void setFirstVoteOffline(int value) {
		setPluginData("OtherRewards.FirstVote", value);

	}

	public synchronized int getAllSitesOffline() {
		return getPluginData().getInt("OtherRewards.AllSites");
	}

	public synchronized int getFirstVoteOffline() {
		return getPluginData().getInt("OtherRewards.FirstVote");
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

	/**
	 * Sets the has gotte milestone.
	 *
	 * @param votesRequired
	 *            the votes required
	 * @param value
	 *            the value
	 */
	public synchronized void setHasGotteMilestone(int votesRequired, boolean value) {
		setPluginData("MilestonesGiven." + votesRequired, value);
	}

	/**
	 * Sets the checks for gotten first vote.
	 *
	 * @param value
	 *            the new checks for gotten first vote
	 */
	public synchronized void setHasGottenFirstVote(boolean value) {
		setPluginData("FirstVoteGotten", value);
	}

	/**
	 * Sets the checks for top voter ignore permssion.
	 *
	 * @param value
	 *            the new checks for top voter ignore permssion
	 */
	public synchronized void setHasTopVoterIgnorePermssion(boolean value) {
		setPluginData("TopVoterIgnore", value);
	}

	/**
	 * Sets the offline milestone votes.
	 *
	 * @param value
	 *            the value
	 */
	public synchronized void setOfflineMilestoneVotes(ArrayList<String> value) {
		setPluginData("OfflineMilestones", value);
	}

	public synchronized int getAllTimeTotal() {
		return getPluginData().getInt("AllTimeTotal");
	}

	public synchronized void setAllTimeTotal(int value) {
		setPluginData("AllTimeTotal", value);
	}

	/**
	 * Sets the offline top voter.
	 *
	 * @param place
	 *            the new offline top voter
	 */
	public synchronized void setOfflineTopVoter(int place) {
		setPluginData("TopVoter." + LocalDateTime.now().getYear() + "." + LocalDateTime.now().getMonth().toString(),
				place);
	}

	/**
	 * Sets the offline votes.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public synchronized void setOfflineVotes(VoteSite voteSite, int amount) {
		setPluginData("OfflineVotes." + voteSite.getSiteName(), amount);
	}

	/**
	 * Sets the points.
	 *
	 * @param value
	 *            the new points
	 */
	public synchronized void setPoints(int value) {
		setPluginData("Points", value);
	}

	/**
	 * Sets the reminded.
	 *
	 * @param reminded
	 *            the new reminded
	 */
	public synchronized void setReminded(boolean reminded) {
		setPluginData("Reminded", reminded);
	}

	/**
	 * Sets the time.
	 *
	 * @param voteSite
	 *            the new time
	 */
	public synchronized void setTime(VoteSite voteSite) {
		setPluginData("VoteLast." + voteSite.getSiteName(), System.currentTimeMillis());
	}

	/**
	 * Sets the total.
	 *
	 * @param voteSite
	 *            the vote site
	 * @param amount
	 *            the amount
	 */
	public synchronized void setTotal(VoteSite voteSite, int amount) {
		setPluginData("Total." + voteSite.getSiteName(), amount);
	}

	public synchronized void setTotalDaily(int value) {
		getPluginData().set("TotalDaily", value);
	}

	public synchronized void setTotalMileStone(int milestone, int value) {
		setPluginData("Milestone." + milestone, value);
	}

	public synchronized void setTotalWeekly(int value) {
		setPluginData("TotalWeekly", value);
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
			setTopVoterAwardOfflineWeekly(place);
		}
	}

	public synchronized void resetMonthlyTotalVotes() {
		for (VoteSite site : plugin.getVoteSites()) {
			setTotal(site, 0);
		}
	}

	public synchronized void resetWeeklyTotalVotes() {
		setTotalWeekly(0);
	}

	public synchronized void resetDailyTotalVotes() {
		setTotalDaily(0);
	}

}
