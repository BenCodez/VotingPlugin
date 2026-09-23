package com.bencodez.votingplugin.data;

import java.time.temporal.WeekFields;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.configuration.ConfigurationSection;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.signs.SignHandler;
import com.bencodez.votingplugin.timequeue.VoteTimeQueue;
import com.bencodez.votingplugin.topvoter.TopVoter;

public class ServerData {
	public enum TimeChangeRewardState {
		UNCLAIMED, CLAIMED, COMPLETE
	}

	public record TimeChangeUserProgress(String uuid, int streakTarget, boolean rewardRequired,
			boolean rewardComplete) { }
	public record TimeChangeUserPolicy(boolean voteStreaks, boolean highestTotals,
			boolean monthDateTotalsPrimary, boolean streakUsesPercentage,
			double dayPercentage, double weekPercentage, double monthPercentage,
			boolean proxyOwnsResets, boolean waitForProxy) { }
	public record TimeChangeTopPolicy(boolean rewardsEnabled, boolean awardTies,
			boolean ignorePermission, boolean archiveRequired, List<String> rewardPlaces,
			List<String> blacklistedPlayers) {
		public TimeChangeTopPolicy {
			rewardPlaces = List.copyOf(rewardPlaces);
			blacklistedPlayers = List.copyOf(blacklistedPlayers);
		}
	}
	public record TimeChangeRewardTarget(String uuid, String playerName, int place, String reward, int votes) { }
	public record TimeChangeArchiveSection(String name, List<String> lines) {
		public TimeChangeArchiveSection {
			lines = List.copyOf(lines);
		}
	}
	public record TimeChangeArchiveSnapshot(List<TimeChangeArchiveSection> sections) {
		public TimeChangeArchiveSnapshot {
			sections = List.copyOf(sections);
		}
	}

	private static final String TIME_CHANGE_RECOVERY = "TimeChangeRecovery";
	private static final List<String> TIME_CHANGE_PHASES = List.of("START", "SNAPSHOT", "COPY_TOTALS",
			"USER_UPDATES", "TOP_REWARDS", "VOTE_SHOP", "BUNGEE_WAIT", "TOTALS_RESET", "CACHE_CLEAR",
			"POST_DATE", "COMPLETE");

	private VotingPluginMain plugin = VotingPluginMain.plugin;
	private final TimeChangeUserCheckpointStore timeChangeUsers;

	/**
	 * Constructs a new ServerData.
	 *
	 * @param plugin the main plugin instance
	 */
	public ServerData(VotingPluginMain plugin) {
		this.plugin = plugin;
		timeChangeUsers = new TimeChangeUserCheckpointStore(
				plugin == null || plugin.getDataFolder() == null ? null : plugin.getDataFolder().toPath());
	}

	/**
	 * Adds an auto-cached placeholder.
	 *
	 * @param placeholder the placeholder to add
	 */
	public void addAutoCachedPlaceholder(String placeholder) {
		List<String> p = getAutoCachedPlaceholder();

		if (!ArrayUtils.containsIgnoreCase(p, placeholder)) {
			p.add(placeholder);
			setAutoCachedPlaceholder(p);
		}
	}

	/**
	 * Adds a service site to the list.
	 *
	 * @param site the service site to add
	 */
	public synchronized void addServiceSite(String site) {
		ArrayList<String> l = getServiceSites();
		if (!getServiceSites().contains(site)) {
			l.add(site);
		}
		setServiceSites(ArrayUtils.removeDuplicates(l));
	}

	/**
	 * Adds the sign.
	 *
	 * @param location the location
	 * @param data     the data
	 * @param position the position
	 */
	public void addSign(Location location, String data, int position) {

		int count = nextSignNumber();

		getData().set("Signs." + count + ".World", location.getWorld().getName());
		getData().set("Signs." + count + ".X", location.getBlockX());
		getData().set("Signs." + count + ".Y", location.getBlockY());
		getData().set("Signs." + count + ".Z", location.getBlockZ());
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		saveData();
		plugin.getSigns().getSigns().add(new SignHandler(plugin, "" + count, getSignLocation("" + count),
				getSignSkullLocation("" + count), getSignData("" + count), getSignPosition("" + count)));
	}

	/**
	 * Adds a timed vote to the cache.
	 *
	 * @param num the vote number
	 * @param vote the vote time queue entry
	 */
	public void addTimeVoted(int num, VoteTimeQueue vote) {
		getData().set("TimedVoteCache." + num + ".Name", vote.getName());
		getData().set("TimedVoteCache." + num + ".Service", vote.getService());
		getData().set("TimedVoteCache." + num + ".Time", vote.getTime());
		getData().set("TimedVoteCache." + num + ".VoteId",
				vote.getVoteId() == null ? null : vote.getVoteId().toString());
		saveData();
	}

	/** Replaces the durable timed-vote snapshot with one ordered in-memory queue. */
	public synchronized void replaceTimedVoteCache(List<VoteTimeQueue> votes) {
		getData().set("TimedVoteCache", null);
		int index = 0;
		for (VoteTimeQueue vote : votes) {
			String path = "TimedVoteCache." + index++;
			getData().set(path + ".Name", vote.getName());
			getData().set(path + ".Service", vote.getService());
			getData().set(path + ".Time", vote.getTime());
			getData().set(path + ".VoteId", vote.getVoteId() == null ? null : vote.getVoteId().toString());
		}
		saveData();
	}

	/**
	 * Adds a vote shop purchase for the specified identifier.
	 *
	 * @param ident the vote shop identifier
	 */
	public void addVoteShopPurchase(String ident) {
		setVoteShopPurchases(ident, (getVoteShopPurchases(ident) + 1));
	}

	/**
	 * Clears the timed vote cache.
	 */
	public void clearTimedVoteCache() {
		getData().set("TimedVoteCache", null);
		saveData();
	}

	/**
	 * Gets the auto cached placeholders.
	 *
	 * @return the auto cached placeholder list
	 */
	public List<String> getAutoCachedPlaceholder() {
		return getData().getStringList("AutoCachePlaceholders");
	}

	/**
	 * Gets the current bungee vote party count.
	 *
	 * @return the bungee vote party current count
	 */
	public int getBungeeVotePartyCurrent() {
		return getData().getInt("BungeeVotePartyCurrent");
	}

	/**
	 * Gets the required votes for bungee vote party.
	 *
	 * @return the bungee vote party required votes
	 */
	public int getBungeeVotePartyRequired() {
		return getData().getInt("BungeeVotePartyRequired");
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public ConfigurationSection getData() {
		ConfigurationSection data = plugin.getServerDataFile().getData().getConfigurationSection("VotingPlugin");
		if (data == null) {
			plugin.getServerDataFile().getData().createSection("VotingPlugin");
			data = plugin.getServerDataFile().getData().getConfigurationSection("VotingPlugin");
		}
		return data;
	}

	/**
	 * Gets the list of disabled reminders.
	 *
	 * @return the disabled reminders
	 */
	public List<String> getDisabledReminders() {
		return getData().getStringList("DisabledReminders");
	}

	/**
	 * Gets the service sites list.
	 *
	 * @return the service sites
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getServiceSites() {
		return (ArrayList<String>) getData().getList("GottenServiceSites", new ArrayList<>());
	}

	/**
	 * Returns a detached service-site snapshot without creating the VotingPlugin
	 * server-data section. Intended for read-only diagnostics and inspections.
	 *
	 * @return persisted service-site observations
	 */
	public synchronized List<String> getServiceSitesReadOnly() {
		ConfigurationSection root = plugin.getServerDataFile().getData().getConfigurationSection("VotingPlugin");
		return root == null ? List.of() : List.copyOf(root.getStringList("GottenServiceSites"));
	}

	/**
	 * Gets the sign data.
	 *
	 * @param sign the sign
	 * @return the sign data
	 */
	public String getSignData(String sign) {
		return getData().getString("Signs." + sign + ".Data");
	}

	/**
	 * Gets the sign location.
	 *
	 * @param sign the sign
	 * @return the sign location
	 */
	public Location getSignLocation(String sign) {
		return new Location(Bukkit.getWorld(getData().getString("Signs." + sign + ".World")),
				getData().getDouble("Signs." + sign + ".X"), getData().getDouble("Signs." + sign + ".Y"),
				getData().getDouble("Signs." + sign + ".Z"));
	}

	/**
	 * Gets the sign position.
	 *
	 * @param sign the sign
	 * @return the sign position
	 */
	public int getSignPosition(String sign) {
		return getData().getInt("Signs." + sign + ".Position");
	}

	/**
	 * Gets the signs.
	 *
	 * @return the signs
	 */
	public Set<String> getSigns() {
		try {
			return getData().getConfigurationSection("Signs").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<>();
		}
	}

	/**
	 * Gets the sign location.
	 *
	 * @param sign the sign
	 * @return the sign location
	 */
	public Location getSignSkullLocation(String sign) {
		if (getData().getString("Signs." + sign + ".Skull.World", "").isEmpty()) {
			return null;
		}
		return new Location(Bukkit.getWorld(getData().getString("Signs." + sign + ".Skull.World")),
				getData().getDouble("Signs." + sign + ".Skull.X"), getData().getDouble("Signs." + sign + ".Skull.Y"),
				getData().getDouble("Signs." + sign + ".Skull.Z"));
	}

	/**
	 * Gets the timed vote cache keys.
	 *
	 * @return the timed vote cache keys
	 */
	public Set<String> getTimedVoteCacheKeys() {
		if (getData().isConfigurationSection("TimedVoteCache")) {
			return getData().getConfigurationSection("TimedVoteCache").getKeys(false);
		}
		return new HashSet<>();
	}

	/**
	 * Gets the timed vote cache section for the given number.
	 *
	 * @param num the cache entry number
	 * @return the timed vote cache section
	 */
	public ConfigurationSection getTimedVoteCacheSection(String num) {
		return getData().getConfigurationSection("TimedVoteCache." + num);
	}

	/**
	 * Gets the extra required votes for vote party.
	 *
	 * @return the vote party extra required votes
	 */
	public int getVotePartyExtraRequired() {
		return getData().getInt("VotePartyExtraRequired", 0);
	}

	/**
	 * Gets the number of purchases for a vote shop item.
	 *
	 * @param ident the vote shop identifier
	 * @return the vote shop purchases
	 */
	public int getVoteShopPurchases(String ident) {
		return getData().getInt("VoteShopPurchases." + ident);
	}

	/**
	 * Checks if the last vote party was on the same day.
	 *
	 * @return true if same day
	 */
	public boolean isLastVotePartySameDay() {
		int num = getData().getInt("LastVoteParty", 0);
		if (num == plugin.getTimeChecker().getTime().getDayOfYear()) {
			return true;
		}
		return false;
	}

	/**
	 * Checks if the last vote party was in the same week.
	 *
	 * @return true if same week
	 */
	public boolean isLastVotePartySameWeek() {
		int num = getData().getInt("LastVotePartyWeek", -1);
		if (num == plugin.getTimeChecker().getTime().get(WeekFields.of(Locale.getDefault()).weekOfYear())
				&& num != -1) {
			return true;
		}
		return false;
	}

	/**
	 * Checks if the vote shop has been converted.
	 *
	 * @return true if converted
	 */
	public boolean isVoteShopConverted() {
		return getData().getBoolean("VoteShopConverted");
	}

	/**
	 * Next sign number.
	 *
	 * @return the int
	 */
	public int nextSignNumber() {
		Set<String> signs = getSigns();

		if (signs != null) {
			for (int i = 0; i < 100000; i++) {
				if (!signs.contains(Integer.toString(i))) {
					return i;
				}
			}
		}
		return 0;
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		plugin.getServerDataFile().reloadData();
	}

	/**
	 * Gets the Discord message ID for the top voter.
	 *
	 * @param top the top voter type
	 * @return the top voter message ID
	 */
	public long getTopVoterMessageId(TopVoter top) {
		return getData().getLong("DiscordSRV.TopVoterMessageId." + top.toString(), 0);
	}

	/**
	 * Sets the Discord message ID for the top voter.
	 *
	 * @param top the top voter type
	 * @param messageId the message ID
	 */
	public void setTopVoterMessageId(TopVoter top, long messageId) {
		getData().set("DiscordSRV.TopVoterMessageId." + top.toString(), messageId);
		saveData();
	}

	/**
	 * Removes the sign.
	 *
	 * @param sign the sign
	 */
	public void removeSign(String sign) {
		getData().set("Signs." + sign + ".World", null);
		getData().set("Signs." + sign + ".X", null);
		getData().set("Signs." + sign + ".Y", null);
		getData().set("Signs." + sign + ".Z", null);
		getData().set("Signs." + sign + ".Data", null);
		getData().set("Signs." + sign + ".Position", null);
		getData().set("Signs." + sign, null);
		getData().set("Signs." + sign + ".Skull.World", null);
		getData().set("Signs." + sign + ".Skull.X", null);
		getData().set("Signs." + sign + ".Skull.Y", null);
		getData().set("Signs." + sign + ".Skull.Z", null);
		saveData();
	}

	/**
	 * Save data.
	 */
	public synchronized void saveData() {
		plugin.getServerDataFile().saveData();
	}

	/**
	 * Starts or resumes the compact local checkpoint for a durable core time
	 * transition. Only the current transition for each time type is retained;
	 * per-recipient reward receipts are bounded by the top-voter recipient list.
	 *
	 * @param transition the core-owned durable transition
	 */
	public synchronized void beginTimeChangeRecovery(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		boolean sameTransition = transition.getId().equals(getData().getString(path + ".Id", ""));
		if (timeChangeUsers.isFileBacked()) {
			TimeChangeUserProgress legacyUser = null;
			TimeChangeRewardState legacyReward = TimeChangeRewardState.UNCLAIMED;
			if (sameTransition) {
				String userPath = path + ".CurrentUser";
				String uuid = getData().getString(userPath + ".Uuid", "");
				if (!uuid.isEmpty()) {
					boolean complete = getData().getBoolean(userPath + ".RewardComplete", false);
					legacyUser = new TimeChangeUserProgress(uuid,
							getData().getInt(userPath + ".StreakTarget"),
							getData().getBoolean(userPath + ".RewardRequired", false), complete);
					legacyReward = complete ? TimeChangeRewardState.COMPLETE
							: getData().getBoolean(userPath + ".RewardClaimed", false)
									? TimeChangeRewardState.CLAIMED : TimeChangeRewardState.UNCLAIMED;
				}
			}
			timeChangeUsers.begin(transition,
					sameTransition ? getData().getString(path + ".Cursor", "") : "", legacyUser, legacyReward);
		}
		if (sameTransition) return;
		getData().set(path, null);
		getData().set(path + ".Id", transition.getId());
		getData().set(path + ".Period", transition.getPeriodKey());
		getData().set(path + ".Phase", "START");
		getData().set(path + ".Cursor", "");
		saveData();
	}

	/** Fixes per-user period processing policy for the lifetime of one transition. */
	public synchronized TimeChangeUserPolicy prepareTimeChangeUserPolicy(TimeChangeTransition transition,
			TimeChangeUserPolicy proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String policyPath = path + ".UserPolicy";
		if (!getData().getBoolean(policyPath + ".Prepared", false)) {
			getData().set(policyPath + ".VoteStreaks", proposed.voteStreaks());
			getData().set(policyPath + ".HighestTotals", proposed.highestTotals());
			getData().set(policyPath + ".MonthDateTotalsPrimary", proposed.monthDateTotalsPrimary());
			getData().set(policyPath + ".StreakUsesPercentage", proposed.streakUsesPercentage());
			getData().set(policyPath + ".DayPercentage", proposed.dayPercentage());
			getData().set(policyPath + ".WeekPercentage", proposed.weekPercentage());
			getData().set(policyPath + ".MonthPercentage", proposed.monthPercentage());
			getData().set(policyPath + ".ProxyOwnsResets", proposed.proxyOwnsResets());
			getData().set(policyPath + ".WaitForProxy", proposed.waitForProxy());
			getData().set(policyPath + ".Prepared", true);
			try {
				saveData();
			} catch (RuntimeException failure) {
				getData().set(policyPath, null);
				throw failure;
			}
		}
		return getTimeChangeUserPolicy(transition);
	}

	/** Returns the per-user policy captured when recovery began. */
	public synchronized TimeChangeUserPolicy getTimeChangeUserPolicy(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		String policyPath = path + ".UserPolicy";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !getData().getBoolean(policyPath + ".Prepared", false)) {
			throw new IllegalStateException("Time change user policy is not prepared");
		}
		return new TimeChangeUserPolicy(getData().getBoolean(policyPath + ".VoteStreaks"),
				getData().getBoolean(policyPath + ".HighestTotals"),
				getData().getBoolean(policyPath + ".MonthDateTotalsPrimary"),
				getData().getBoolean(policyPath + ".StreakUsesPercentage"),
				getData().getDouble(policyPath + ".DayPercentage"),
				getData().getDouble(policyPath + ".WeekPercentage"),
				getData().getDouble(policyPath + ".MonthPercentage"),
				getData().getBoolean(policyPath + ".ProxyOwnsResets"),
				getData().getBoolean(policyPath + ".WaitForProxy"));
	}

	/** Fixes the VoteShop identifiers selected for this transition before resets begin. */
	public synchronized List<String> prepareTimeChangeVoteShopTargets(TimeChangeTransition transition,
			List<String> proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String targetsPath = path + ".VoteShopTargets";
		if (!getData().getBoolean(targetsPath + ".Prepared", false)) {
			getData().set(targetsPath + ".Identifiers", List.copyOf(proposed));
			getData().set(targetsPath + ".Prepared", true);
			try {
				saveData();
			} catch (RuntimeException failure) {
				getData().set(targetsPath, null);
				throw failure;
			}
		}
		return List.copyOf(getData().getStringList(targetsPath + ".Identifiers"));
	}

	public synchronized List<String> getTimeChangeVoteShopTargets(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		String targetsPath = path + ".VoteShopTargets";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !getData().getBoolean(targetsPath + ".Prepared", false)) {
			throw new IllegalStateException("Time change VoteShop targets are not prepared");
		}
		return List.copyOf(getData().getStringList(targetsPath + ".Identifiers"));
	}

	/** Fixes top reward and archive selection inputs before the period boundary is copied. */
	public synchronized TimeChangeTopPolicy prepareTimeChangeTopPolicy(TimeChangeTransition transition,
			TimeChangeTopPolicy proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String policyPath = path + ".TopPolicy";
		if (!getData().getBoolean(policyPath + ".Prepared", false)) {
			getData().set(policyPath + ".RewardsEnabled", proposed.rewardsEnabled());
			getData().set(policyPath + ".AwardTies", proposed.awardTies());
			getData().set(policyPath + ".IgnorePermission", proposed.ignorePermission());
			getData().set(policyPath + ".ArchiveRequired", proposed.archiveRequired());
			getData().set(policyPath + ".RewardPlaces", proposed.rewardPlaces());
			getData().set(policyPath + ".BlacklistedPlayers", proposed.blacklistedPlayers());
			getData().set(policyPath + ".Prepared", true);
			try {
				saveData();
			} catch (RuntimeException failure) {
				getData().set(policyPath, null);
				throw failure;
			}
		}
		return getTimeChangeTopPolicy(transition);
	}

	public synchronized TimeChangeTopPolicy getTimeChangeTopPolicy(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		String policyPath = path + ".TopPolicy";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !getData().getBoolean(policyPath + ".Prepared", false)) {
			throw new IllegalStateException("Time change top policy is not prepared");
		}
		return new TimeChangeTopPolicy(getData().getBoolean(policyPath + ".RewardsEnabled"),
				getData().getBoolean(policyPath + ".AwardTies"),
				getData().getBoolean(policyPath + ".IgnorePermission"),
				getData().getBoolean(policyPath + ".ArchiveRequired"),
				getData().getStringList(policyPath + ".RewardPlaces"),
				getData().getStringList(policyPath + ".BlacklistedPlayers"));
	}

	/** Fixes whether one listener effect belongs to this transition. */
	public synchronized boolean prepareTimeChangeEffectPolicy(TimeChangeTransition transition, String effect,
			boolean proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String policyPath = path + ".EffectPolicies." + effect;
		if (!getData().contains(policyPath)) {
			getData().set(policyPath, proposed);
			try {
				saveData();
			} catch (RuntimeException failure) {
				getData().set(policyPath, null);
				throw failure;
			}
		}
		return getData().getBoolean(policyPath);
	}

	/** Returns whether the named phase has been durably completed. */
	public synchronized boolean hasTimeChangePhase(TimeChangeTransition transition, String phase) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) return false;
		int requested = TIME_CHANGE_PHASES.indexOf(phase);
		int completed = TIME_CHANGE_PHASES.indexOf(getData().getString(path + ".Phase", "START"));
		return requested >= 0 && completed >= requested;
	}

	/** Records the next completed recovery phase synchronously. */
	public synchronized void completeTimeChangePhase(TimeChangeTransition transition, String phase) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		getData().set(path + ".Phase", phase);
		saveData();
	}

	/** Returns the last durably completed UUID in sorted user processing. */
	public synchronized String getTimeChangeCursor(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) return "";
		if (timeChangeUsers.isFileBacked()) return timeChangeUsers.cursor(transition);
		return getData().getString(path + ".Cursor", "");
	}

	/** Advances the compact sorted-user cursor after that user's work is done. */
	public synchronized void completeTimeChangeUser(TimeChangeTransition transition, String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		if (timeChangeUsers.isFileBacked()) {
			timeChangeUsers.completeUser(transition, uuid);
			return;
		}
		getData().set(path + ".Cursor", uuid);
		getData().set(path + ".CurrentUser", null);
		saveData();
	}

	/**
	 * Persists the absolute streak target before changing a user. Only one user is
	 * in flight because the period walk is ordered and synchronous.
	 */
	public synchronized TimeChangeUserProgress prepareTimeChangeUserStreak(TimeChangeTransition transition,
			String uuid, int streakTarget, boolean rewardRequired) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		if (timeChangeUsers.isFileBacked()) {
			return timeChangeUsers.prepareStreak(transition, uuid, streakTarget, rewardRequired);
		}
		String userPath = path + ".CurrentUser";
		if (!uuid.equals(getData().getString(userPath + ".Uuid", ""))) {
			getData().set(userPath, null);
			getData().set(userPath + ".Uuid", uuid);
			getData().set(userPath + ".StreakTarget", streakTarget);
			getData().set(userPath + ".RewardRequired", rewardRequired);
			getData().set(userPath + ".RewardClaimed", false);
			getData().set(userPath + ".RewardComplete", false);
			saveData();
		}
		return new TimeChangeUserProgress(uuid, getData().getInt(userPath + ".StreakTarget"),
				getData().getBoolean(userPath + ".RewardRequired", false),
				getData().getBoolean(userPath + ".RewardComplete", false));
	}

	public synchronized TimeChangeRewardState getTimeChangeUserStreakRewardState(
			TimeChangeTransition transition, String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		if (timeChangeUsers.isFileBacked()) return timeChangeUsers.rewardState(transition, uuid);
		String userPath = path + ".CurrentUser";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !uuid.equals(getData().getString(userPath + ".Uuid", ""))) {
			throw new IllegalStateException("Time change recovery user does not match");
		}
		if (getData().getBoolean(userPath + ".RewardComplete", false)) return TimeChangeRewardState.COMPLETE;
		return getData().getBoolean(userPath + ".RewardClaimed", false)
				? TimeChangeRewardState.CLAIMED : TimeChangeRewardState.UNCLAIMED;
	}

	/** Durably claims the in-flight user's streak reward before invoking it. */
	public synchronized void claimTimeChangeUserStreakReward(TimeChangeTransition transition, String uuid) {
		if (getTimeChangeUserStreakRewardState(transition, uuid) != TimeChangeRewardState.UNCLAIMED) {
			throw new IllegalStateException("Time change streak reward is already claimed");
		}
		if (timeChangeUsers.isFileBacked()) {
			timeChangeUsers.setRewardState(transition, uuid, TimeChangeRewardState.CLAIMED);
			return;
		}
		String userPath = timeChangeRecoveryPath(transition.getType()) + ".CurrentUser";
		getData().set(userPath + ".RewardClaimed", true);
		try {
			saveData();
		} catch (RuntimeException failure) {
			getData().set(userPath + ".RewardClaimed", false);
			throw failure;
		}
	}

	/** Marks the in-flight user's streak reward call as returned successfully. */
	public synchronized void completeTimeChangeUserStreakReward(TimeChangeTransition transition, String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		if (timeChangeUsers.isFileBacked()) {
			if (timeChangeUsers.rewardState(transition, uuid) != TimeChangeRewardState.CLAIMED) {
				throw new IllegalStateException("Time change streak reward is not claimed");
			}
			timeChangeUsers.setRewardState(transition, uuid, TimeChangeRewardState.COMPLETE);
			return;
		}
		String userPath = path + ".CurrentUser";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !uuid.equals(getData().getString(userPath + ".Uuid", ""))) {
			throw new IllegalStateException("Time change recovery user does not match");
		}
		getData().set(userPath + ".RewardComplete", true);
		try {
			saveData();
		} catch (RuntimeException failure) {
			getData().set(userPath + ".RewardComplete", false);
			throw failure;
		}
	}

	/**
	 * Persists the ranked recipients and reward assignments selected at the period
	 * boundary. Retries always return this first durable selection.
	 */
	public synchronized List<TimeChangeRewardTarget> prepareTimeChangeRewardTargets(
			TimeChangeTransition transition, List<TimeChangeRewardTarget> proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String targetsPath = path + ".RewardTargets";
		if (getData().getBoolean(targetsPath + ".Prepared", false)) {
			return getTimeChangeRewardTargets(transition);
		}
		List<TimeChangeRewardTarget> snapshot = List.copyOf(proposed);
		for (TimeChangeRewardTarget target : snapshot) {
			validateRewardTarget(target);
		}
		getData().set(targetsPath, null);
		getData().set(targetsPath + ".Count", snapshot.size());
		for (int index = 0; index < snapshot.size(); index++) {
			TimeChangeRewardTarget target = snapshot.get(index);
			String targetPath = targetsPath + ".Entries." + index;
			getData().set(targetPath + ".Uuid", target.uuid());
			getData().set(targetPath + ".PlayerName", target.playerName());
			getData().set(targetPath + ".Place", target.place());
			getData().set(targetPath + ".Reward", target.reward());
			getData().set(targetPath + ".Votes", target.votes());
		}
		getData().set(targetsPath + ".Prepared", true);
		try {
			saveData();
		} catch (RuntimeException | Error failure) {
			getData().set(targetsPath, null);
			throw failure;
		}
		return snapshot;
	}

	/** Returns the durable ranked reward selection for this transition. */
	public synchronized List<TimeChangeRewardTarget> getTimeChangeRewardTargets(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		String targetsPath = path + ".RewardTargets";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !getData().getBoolean(targetsPath + ".Prepared", false)) {
			return List.of();
		}
		int count = getData().getInt(targetsPath + ".Count", -1);
		if (count < 0) {
			throw new IllegalStateException("Invalid time change reward target count");
		}
		List<TimeChangeRewardTarget> targets = new ArrayList<>(count);
		for (int index = 0; index < count; index++) {
			String targetPath = targetsPath + ".Entries." + index;
			TimeChangeRewardTarget target = new TimeChangeRewardTarget(
					getData().getString(targetPath + ".Uuid", ""),
					getData().getString(targetPath + ".PlayerName", ""),
					getData().getInt(targetPath + ".Place", 0),
					getData().getString(targetPath + ".Reward", ""),
					getData().getInt(targetPath + ".Votes", -1));
			validateRewardTarget(target);
			targets.add(target);
		}
		return List.copyOf(targets);
	}

	private void validateRewardTarget(TimeChangeRewardTarget target) {
		if (target == null || target.playerName() == null || target.place() <= 0 || target.votes() < 0
				|| target.reward() == null || target.reward().isEmpty()) {
			throw new IllegalStateException("Invalid time change reward target");
		}
		try {
			UUID.fromString(target.uuid());
		} catch (RuntimeException invalidUuid) {
			throw new IllegalStateException("Invalid time change reward target UUID", invalidUuid);
		}
	}

	/** Persists the reward and archive boundary snapshots in one checkpoint. */
	public synchronized void prepareTimeChangeSnapshot(TimeChangeTransition transition,
			List<TimeChangeRewardTarget> targets, TimeChangeArchiveSnapshot archive) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String snapshotPath = path + ".BoundarySnapshot";
		if (getData().getBoolean(snapshotPath + ".Prepared", false)) return;
		List<TimeChangeRewardTarget> targetSnapshot = List.copyOf(targets);
		for (TimeChangeRewardTarget target : targetSnapshot) validateRewardTarget(target);
		validateArchive(archive);
		String targetsPath = path + ".RewardTargets";
		String archivePath = path + ".Archive";
		getData().set(targetsPath, null);
		getData().set(targetsPath + ".Count", targetSnapshot.size());
		for (int index = 0; index < targetSnapshot.size(); index++) {
			TimeChangeRewardTarget target = targetSnapshot.get(index);
			String targetPath = targetsPath + ".Entries." + index;
			getData().set(targetPath + ".Uuid", target.uuid());
			getData().set(targetPath + ".PlayerName", target.playerName());
			getData().set(targetPath + ".Place", target.place());
			getData().set(targetPath + ".Reward", target.reward());
			getData().set(targetPath + ".Votes", target.votes());
		}
		getData().set(targetsPath + ".Prepared", true);
		getData().set(archivePath, null);
		getData().set(archivePath + ".Count", archive.sections().size());
		for (int index = 0; index < archive.sections().size(); index++) {
			TimeChangeArchiveSection section = archive.sections().get(index);
			String sectionPath = archivePath + ".Sections." + index;
			getData().set(sectionPath + ".Name", section.name());
			getData().set(sectionPath + ".Lines", section.lines());
		}
		getData().set(archivePath + ".Prepared", true);
		getData().set(snapshotPath + ".Prepared", true);
		try {
			saveData();
		} catch (RuntimeException | Error failure) {
			getData().set(targetsPath, null);
			getData().set(archivePath, null);
			getData().set(snapshotPath, null);
			throw failure;
		}
	}

	/** Persists the complete top-voter archive contents selected at the period boundary. */
	public synchronized TimeChangeArchiveSnapshot prepareTimeChangeArchive(TimeChangeTransition transition,
			TimeChangeArchiveSnapshot proposed) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String archivePath = path + ".Archive";
		if (getData().getBoolean(archivePath + ".Prepared", false)) return getTimeChangeArchive(transition);
		validateArchive(proposed);
		getData().set(archivePath, null);
		getData().set(archivePath + ".Count", proposed.sections().size());
		for (int index = 0; index < proposed.sections().size(); index++) {
			TimeChangeArchiveSection section = proposed.sections().get(index);
			String sectionPath = archivePath + ".Sections." + index;
			getData().set(sectionPath + ".Name", section.name());
			getData().set(sectionPath + ".Lines", section.lines());
		}
		getData().set(archivePath + ".Prepared", true);
		try {
			saveData();
		} catch (RuntimeException | Error failure) {
			getData().set(archivePath, null);
			throw failure;
		}
		return proposed;
	}

	/** Returns the durable top-voter archive contents for this transition. */
	public synchronized TimeChangeArchiveSnapshot getTimeChangeArchive(TimeChangeTransition transition) {
		String path = timeChangeRecoveryPath(transition.getType());
		String archivePath = path + ".Archive";
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))
				|| !getData().getBoolean(archivePath + ".Prepared", false)) {
			return new TimeChangeArchiveSnapshot(List.of());
		}
		int count = getData().getInt(archivePath + ".Count", -1);
		if (count < 0) throw new IllegalStateException("Invalid time change archive section count");
		List<TimeChangeArchiveSection> sections = new ArrayList<>(count);
		for (int index = 0; index < count; index++) {
			String sectionPath = archivePath + ".Sections." + index;
			sections.add(new TimeChangeArchiveSection(getData().getString(sectionPath + ".Name", ""),
					getData().getStringList(sectionPath + ".Lines")));
		}
		TimeChangeArchiveSnapshot snapshot = new TimeChangeArchiveSnapshot(sections);
		validateArchive(snapshot);
		return snapshot;
	}

	private void validateArchive(TimeChangeArchiveSnapshot snapshot) {
		if (snapshot == null) throw new IllegalStateException("Invalid time change archive");
		Set<String> names = new HashSet<>();
		for (TimeChangeArchiveSection section : snapshot.sections()) {
			if (section == null || section.name() == null || section.name().isEmpty() || section.lines() == null
					|| !names.add(section.name()) || section.lines().stream().anyMatch(line -> line == null)) {
				throw new IllegalStateException("Invalid time change archive section");
			}
		}
	}

	/** Returns the durable delivery state for one top-voter reward recipient. */
	public synchronized TimeChangeRewardState getTimeChangeRewardState(TimeChangeTransition transition,
			String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			return TimeChangeRewardState.UNCLAIMED;
		}
		if (getData().getBoolean(path + ".Rewards." + uuid, false)) return TimeChangeRewardState.COMPLETE;
		String stored = getData().getString(path + ".RewardStates." + uuid, "");
		try {
			return stored.isEmpty() ? TimeChangeRewardState.UNCLAIMED : TimeChangeRewardState.valueOf(stored);
		} catch (IllegalArgumentException invalid) {
			throw new IllegalStateException("Invalid time change reward state for " + uuid, invalid);
		}
	}

	/** Checks the durable receipt for one top-voter reward recipient. */
	public synchronized boolean hasTimeChangeRewardReceipt(TimeChangeTransition transition, String uuid) {
		return getTimeChangeRewardState(transition, uuid) == TimeChangeRewardState.COMPLETE;
	}

	/** Durably claims a top-voter reward before invoking its existing reward API. */
	public synchronized void claimTimeChangeReward(TimeChangeTransition transition, String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		if (getTimeChangeRewardState(transition, uuid) != TimeChangeRewardState.UNCLAIMED) {
			throw new IllegalStateException("Time change reward is already claimed for " + uuid);
		}
		String statePath = path + ".RewardStates." + uuid;
		getData().set(statePath, TimeChangeRewardState.CLAIMED.name());
		try {
			saveData();
		} catch (RuntimeException failure) {
			getData().set(statePath, null);
			throw failure;
		}
	}

	/** Persists a recipient receipt only after the existing reward API returns. */
	public synchronized void completeTimeChangeReward(TimeChangeTransition transition, String uuid) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String receiptPath = path + ".Rewards." + uuid;
		String statePath = path + ".RewardStates." + uuid;
		Object previousReceipt = getData().get(receiptPath);
		String previousState = getData().getString(statePath, "");
		getData().set(receiptPath, true);
		getData().set(statePath, TimeChangeRewardState.COMPLETE.name());
		try {
			saveData();
		} catch (RuntimeException failure) {
			getData().set(receiptPath, previousReceipt);
			getData().set(statePath, previousState.isEmpty() ? null : previousState);
			throw failure;
		}
	}

	/** Records idempotent non-reward listener effects such as VoteParty resets. */
	public synchronized boolean hasTimeChangeEffect(TimeChangeTransition transition, String effect) {
		String path = timeChangeRecoveryPath(transition.getType());
		return transition.getId().equals(getData().getString(path + ".Id", ""))
				&& getData().getBoolean(path + ".Effects." + effect, false);
	}

	/** Marks a completed non-reward listener effect. */
	public synchronized void completeTimeChangeEffect(TimeChangeTransition transition, String effect) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		getData().set(path + ".Effects." + effect, true);
		saveData();
	}

	/**
	 * Clears YAML-backed VoteParty state before the recoverable database reset.
	 * The in-memory marker deliberately remains applied when saving fails. A later
	 * vote will therefore persist the cleared boundary and its own new state
	 * together, while a restart can safely retry the clear before votes resume.
	 */
	public synchronized void prepareTimeChangeVotePartyReset(TimeChangeTransition transition, String effect) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String resetPath = path + ".VotePartyResets." + effect + ".StateReset";
		if (getData().getBoolean(resetPath, false)) return;
		getData().set("VoteParty.Total", 0);
		getData().set("VoteParty.Voted", new ArrayList<>());
		getData().set(resetPath, true);
		saveData();
	}

	/** Records the VoteParty reset receipt after its database boundary is removed. */
	public synchronized void completeTimeChangeVotePartyReset(TimeChangeTransition transition, String effect) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		String resetPath = path + ".VotePartyResets." + effect + ".StateReset";
		if (!getData().getBoolean(resetPath, false)) {
			throw new IllegalStateException("VoteParty state reset is not prepared");
		}
		String effectPath = path + ".Effects." + effect;
		Object previousEffect = getData().get(effectPath);
		try {
			getData().set(effectPath, true);
			saveData();
		} catch (RuntimeException | Error failure) {
			getData().set(effectPath, previousEffect);
			throw failure;
		}
	}

	/** Resets VoteParty's extra requirement and records its receipt in one save. */
	public synchronized void completeTimeChangeVotePartyExtraReset(TimeChangeTransition transition, String effect) {
		String path = timeChangeRecoveryPath(transition.getType());
		if (!transition.getId().equals(getData().getString(path + ".Id", ""))) {
			throw new IllegalStateException("Time change recovery transition does not match");
		}
		int previousExtra = getData().getInt("VotePartyExtraRequired");
		String effectPath = path + ".Effects." + effect;
		Object previousEffect = getData().get(effectPath);
		try {
			getData().set("VotePartyExtraRequired", 0);
			getData().set(effectPath, true);
			saveData();
		} catch (RuntimeException | Error failure) {
			getData().set("VotePartyExtraRequired", previousExtra);
			getData().set(effectPath, previousEffect);
			throw failure;
		}
	}

	private String timeChangeRecoveryPath(TimeType type) {
		return TIME_CHANGE_RECOVERY + "." + type.name();
	}

	/**
	 * Saves the disabled reminders list.
	 *
	 * @param disabledReminders the list of UUIDs with disabled reminders
	 */
	public void saveDisabledReminders(ArrayList<UUID> disabledReminders) {
		ArrayList<String> uuids = new ArrayList<>();
		for (UUID uuid : disabledReminders) {
			uuids.add(uuid.toString());
		}
		getData().set("DisabledReminders", uuids);
		saveData();
	}

	/**
	 * Sets the auto cached placeholders.
	 *
	 * @param placeholders the list of placeholders
	 */
	public void setAutoCachedPlaceholder(List<String> placeholders) {
		getData().set("AutoCachePlaceholders", placeholders);
		saveData();
	}

	/**
	 * Sets the current bungee vote party count.
	 *
	 * @param current the current vote count
	 */
	public void setBungeeVotePartyCurrent(int current) {
		getData().set("BungeeVotePartyCurrent", current);
		saveData();
	}

	/**
	 * Sets the required votes for bungee vote party.
	 *
	 * @param required the required vote count
	 */
	public void setBungeeVotePartyRequired(int required) {
		getData().set("BungeeVotePartyRequired", required);
		saveData();
	}

	/**
	 * Sets the service sites list.
	 *
	 * @param list the service sites list
	 */
	public void setServiceSites(ArrayList<String> list) {
		getData().set("GottenServiceSites", list);
		saveData();
	}

	/**
	 * Sets the vote shop converted status.
	 *
	 * @param value the converted status
	 */
	public void setShopConverted(boolean value) {
		getData().set("VoteShopConverted", value);
		saveData();
	}

	/**
	 * Sets sign data.
	 *
	 * @param count the sign count
	 * @param location the sign location
	 * @param skullLocation the skull location
	 * @param data the sign data
	 * @param position the sign position
	 */
	public void setSign(String count, Location location, Location skullLocation, String data, int position) {

		getData().set("Signs." + count + ".World", location.getWorld().getName());
		getData().set("Signs." + count + ".X", (int) location.getX());
		getData().set("Signs." + count + ".Y", (int) location.getY());
		getData().set("Signs." + count + ".Z", (int) location.getZ());
		getData().set("Signs." + count + ".Data", data);
		getData().set("Signs." + count + ".Position", position);
		if (skullLocation != null) {
			getData().set("Signs." + count + ".Skull.World", skullLocation.getWorld().getName());
			getData().set("Signs." + count + ".Skull.X", (int) skullLocation.getX());
			getData().set("Signs." + count + ".Skull.Y", (int) skullLocation.getY());
			getData().set("Signs." + count + ".Skull.Z", (int) skullLocation.getZ());
		}
		saveData();
	}

	/**
	 * Sets the skull location for a sign.
	 *
	 * @param count the sign count
	 * @param skullLocation the skull location
	 */
	public void setSkullLocation(String count, Location skullLocation) {
		if (skullLocation != null) {
			getData().set("Signs." + count + ".Skull.World", skullLocation.getWorld().getName());
			getData().set("Signs." + count + ".Skull.X", (int) skullLocation.getX());
			getData().set("Signs." + count + ".Skull.Y", (int) skullLocation.getY());
			getData().set("Signs." + count + ".Skull.Z", (int) skullLocation.getZ());
		}
	}

	/**
	 * Sets the version.
	 */
	public void setVersion() {
		getData().set("Version", Bukkit.getVersion());
		saveData();
	}

	/**
	 * Sets the extra required votes for vote party.
	 *
	 * @param value the extra required votes
	 */
	public void setVotePartyExtraRequired(int value) {
		getData().set("VotePartyExtraRequired", value);
		saveData();
	}

	/**
	 * Sets the number of purchases for a vote shop item.
	 *
	 * @param ident the vote shop identifier
	 * @param amount the purchase amount
	 */
	public void setVoteShopPurchases(String ident, int amount) {
		getData().set("VoteShopPurchases." + ident, amount);
		saveData();
	}

	/**
	 * Updates the last vote party timestamp to today.
	 */
	public void updateLastVoteParty() {
		getData().set("LastVoteParty", plugin.getTimeChecker().getTime().getDayOfYear());
		saveData();
	}

	/**
	 * Updates the last vote party week timestamp to this week.
	 */
	public void updateLastVotePartyWeek() {
		getData().set("LastVotePartyWeek",
				plugin.getTimeChecker().getTime().get(WeekFields.of(Locale.getDefault()).weekOfYear()));
		saveData();
	}

	/**
	 * Updates placeholders to lowercase format.
	 */
	public void updatePlaceholders() {
		boolean data = getData().getBoolean("AutoCacheUpdated", false);
		if (!data) {
			List<String> placeholders = getAutoCachedPlaceholder();
			for (int i = 0; i < placeholders.size(); i++) {
				placeholders.set(i, placeholders.get(i).toLowerCase());
			}
			setAutoCachedPlaceholder(placeholders);
			getData().set("AutoCacheUpdated", true);
			saveData();
		}
	}

	/**
	 * Update values.
	 */
	public void updateValues() {
		setVersion();
	}
}
