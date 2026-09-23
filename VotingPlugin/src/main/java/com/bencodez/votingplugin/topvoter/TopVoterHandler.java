package com.bencodez.votingplugin.topvoter;

import java.io.File;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CancellationException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.PreDateChangedEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.file.YMLFileHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSection;
import com.bencodez.votingplugin.data.ServerData.TimeChangeArchiveSnapshot;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardTarget;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardState;
import com.bencodez.votingplugin.data.ServerData.TimeChangeTopPolicy;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserPolicy;
import com.bencodez.votingplugin.user.PeriodTotalMutationFence;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.voteshop.service.VoteShopLimitMutationFence;
import com.bencodez.votingplugin.voteshop.service.VoteShopPurchaseService;

/**
 * Handles top voter rankings and statistics.
 */
public class TopVoterHandler implements Listener {
	private static final String SNAPSHOT = "SNAPSHOT";
	private static final String COPY_TOTALS = "COPY_TOTALS";
	private static final String USER_UPDATES = "USER_UPDATES";
	private static final String TOP_REWARDS = "TOP_REWARDS";
	private static final String VOTE_SHOP = "VOTE_SHOP";
	private static final String BUNGEE_WAIT = "BUNGEE_WAIT";
	private static final String TOTALS_RESET = "TOTALS_RESET";
	private static final String CACHE_CLEAR = "CACHE_CLEAR";
	private static final String POST_DATE = "POST_DATE";
	private static final String COMPLETE = "COMPLETE";

	private VotingPluginMain plugin;
	private final TopVoterLoader loader;
	private final HashMap<String, TimeChangeTransition.Lease> retainedTransitions = new HashMap<>();

	/**
	 * Constructs a new top voter handler.
	 * @param plugin the plugin instance
	 */
	public TopVoterHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.loader = new TopVoterLoader(plugin);
	}

	/**
	 * Checks if bungee should handle resets.
	 * @return true if bungee handles resets
	 */
	public boolean bungeeHandleResets() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			if (plugin.getBungeeSettings().isGloblalDataEnabled()) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Gets monthly top voters at a specific time.
	 * @param atTime the time to check
	 * @return map of top voters and their vote counts
	 */
	public LinkedHashMap<TopVoterPlayer, Integer> getMonthlyTopVotersAtTime(LocalDateTime atTime) {
		return loader.getMonthlyTopVotersAtTime(atTime);
	}


	/**
	 * Gets the top voter blacklist.
	 * @return list of blacklisted players
	 */
	public ArrayList<String> getTopVoterBlackList() {
		return plugin.getConfigFile().getBlackList();
	}

	/**
	 * Gets top voters for a specific month.
	 * @param month the year-month to check
	 * @param cols player data columns
	 * @return map of top voters and their vote counts
	 */
	public LinkedHashMap<TopVoterPlayer, Integer> getTopVotersOfMonth(YearMonth month,
			HashMap<UUID, ArrayList<Column>> cols) {
		return loader.getTopVotersOfMonth(month, cols);
	}


	/**
	 * Top voters weekly.
	 *
	 * @return the string[]
	 */

	/**
	 * Gets weekly top voters as formatted strings.
	 * @return array of formatted top voter lines
	 */
	public String[] getTopVotersWeekly() {
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<TopVoterPlayer> users = new ArrayList<>(plugin.getTopVoter(TopVoter.Weekly).keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = plugin.getConfigFile().getFormatCommandsVoteTopLine().replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%", "" + plugin.getTopVoter(TopVoter.Weekly).get(users.get(i)));
			msg.add(line);
		}
		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	private HashMap<Integer, String> handlePlaces(Set<String> places) {
		return TopVoterRanking.mapAwardPlaces(places);
	}


	/**
	 * Loads last month's top voter data.
	 */
	public void loadLastMonth() {
		loader.loadLastMonth();
	}


	/**
	 * Loads previous month top voters for all configured months.
	 */
	public void loadPreviousMonthTopVoters() {
		loader.loadPreviousMonthTopVoters();
	}


	/**
	 * Handles date change events.
	 *
	 * @param event date changed event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onDateChanged(DateChangedEvent event) {
		if (event.getTransition() != null) {
			finishRecoverableDateChange(event);
			return;
		}
		plugin.setUpdate(true);
		plugin.update();
		if (event.getTimeType().equals(TimeType.MONTH)) {
			loadLastMonth();
		}
		if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
			plugin.getMysql().clearCacheBasic();
		}
	}

	/**
	 * Handles day change events.
	 * @param event the day change event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChange(DayChangeEvent event) {
		if (event.getTransition() != null) {
			processRecoverableChange(TopVoter.Daily, event.getTransition());
			return;
		}
		synchronized (VotingPluginMain.plugin) {
			long startTime = System.currentTimeMillis();
			if (plugin.getConfigFile().isStoreTopVotersDaily()) {
				plugin.getLogger().info("Saving TopVoters Daily");
				storeTopVoters(TopVoter.Daily);
			}

			plugin.getUserManager().copyColumnData(TopVoter.Daily.getColumnName(), TopVoter.Daily.getLastColumnName());
			if (plugin.getConfigFile().isUseVoteStreaks() || plugin.getConfigFile().isUseHighestTotals()) {
				plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
					user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
					user.updateTempCacheWithColumns(columns);

					if (plugin.getConfigFile().isUseVoteStreaks()) {
						if (!user.voteStreakUpdatedToday(LocalDateTime.now().minusDays(1))) {
							if (user.getDayVoteStreak() != 0) {
								user.setDayVoteStreak(0);
							}
						}
					}

					if (plugin.getConfigFile().isUseHighestTotals()) {
						if (user.getHighestDailyTotal() < user.getTotal(TopVoter.Daily)) {
							user.setHighestDailyTotal(user.getTotal(TopVoter.Daily));
						}
					}
					user.clearTempCache();
				}, (count) -> {
					// finished
				});
			}

			try {
				if (plugin.getSpecialRewardsConfig().isEnableDailyRewards()) {
					HashMap<Integer, String> places = handlePlaces(
							plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;
					@SuppressWarnings("unchecked")
					LinkedHashMap<TopVoterPlayer, Integer> clone = (LinkedHashMap<TopVoterPlayer, Integer>) plugin
							.getTopVoter(TopVoter.Daily).clone();
					for (Entry<TopVoterPlayer, Integer> entry : clone.entrySet()) {
						if (plugin.getConfigFile().isTopVoterAwardsTies()) {
							if (entry.getValue().intValue() != lastTotal) {
								i++;
							}
						} else {
							i++;
						}
						if (places.containsKey(i)) {
							VotingPluginUser user = entry.getKey().getUser();
							user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
							if (!plugin.getConfigFile().isTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
								user.giveDailyTopVoterAward(i, places.get(i));
								plugin.getLogger().info(
										"Giving daily top voter reward " + i + " to " + entry.getKey().getPlayerName());
							}
						}
						lastTotal = entry.getValue().intValue();
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			for (String shopIdent : plugin.getShopFile().getShopIdentifiers()) {
				if (plugin.getShopFile().getVoteShopResetDaily(shopIdent)) {
					resetVoteShopLimit(shopIdent,
							VoteShopPurchaseService.currentLimitGenerationId(plugin, shopIdent));
				}
			}

			// give time for other servers to catch up
			if (!plugin.getTopVoterHandler().bungeeHandleResets() && plugin.getBungeeSettings().isUseBungeecoord()) {
				plugin.debug("Delaying time change 10 seconds for other servers to catchup");
				try {
					Thread.sleep(10000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			if (!bungeeHandleResets()) {
				resetTotals(TopVoter.Daily);
			}

			if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
				plugin.getMysql().clearCacheBasic();
			}

			long now = ((System.currentTimeMillis() - startTime) / 1000);
			plugin.getLogger().info("Finished processing day change, took " + now + " seconds");
		}
	}

	/**
	 * Handles month change events.
	 * @param event the month change event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onMonthChange(MonthChangeEvent event) {
		if (event.getTransition() != null) {
			processRecoverableChange(TopVoter.Monthly, event.getTransition());
			return;
		}
		long startTime = System.currentTimeMillis();
		synchronized (VotingPluginMain.plugin) {
			plugin.getLogger().info("Saving TopVoters Monthly");
			storeTopVoters(TopVoter.Monthly);
			if (!bungeeHandleResets()) {
				plugin.getUserManager().copyColumnData(TopVoter.Monthly.getColumnName(),
						TopVoter.Monthly.getLastColumnName());
			}
			LocalDateTime lastMonthTime = plugin.getTimeChecker().getTime().minusMonths(1);
			if (plugin.getConfigFile().isUseHighestTotals() || plugin.getConfigFile().isUseVoteStreaks()) {
				plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
					user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
					user.updateTempCacheWithColumns(columns);

					if (plugin.getConfigFile().isUseVoteStreaks()) {
						if (user.getTotal(TopVoter.Monthly, lastMonthTime) == 0 && user.getMonthVoteStreak() != 0) {
							user.setMonthVoteStreak(0);
						} else {
							if (!plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage()
									|| user.hasPercentageTotal(TopVoter.Monthly,
											plugin.getSpecialRewardsConfig().getVoteStreakRequirementMonth(),
											lastMonthTime)) {
								user.addMonthVoteStreak();
								plugin.getSpecialRewards().checkVoteStreak(null, user, "Month",
										plugin.getBungeeSettings().isUseBungeecoord());
							}
						}
					}

					// using new system
					// user.setLastMonthTotal(user.getTotal(TopVoter.Monthly));

					if (plugin.getConfigFile().isUseHighestTotals()) {
						if (user.getHighestMonthlyTotal() < user.getTotal(TopVoter.Monthly, lastMonthTime)) {
							user.setHighestMonthlyTotal(user.getTotal(TopVoter.Monthly, lastMonthTime));
						}
					}
					user.clearTempCache();
				}, (count) -> {
					// finished
				});

			}

			try {
				if (plugin.getSpecialRewardsConfig().isEnableMonthlyAwards()) {
					HashMap<Integer, String> places = handlePlaces(
							plugin.getSpecialRewardsConfig().getMonthlyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;

					LinkedHashMap<TopVoterPlayer, Integer> topVoters = null;
					if (plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal()) {
						topVoters = getMonthlyTopVotersAtTime(lastMonthTime);
					} else {
						@SuppressWarnings("unchecked")
						LinkedHashMap<TopVoterPlayer, Integer> clone = (LinkedHashMap<TopVoterPlayer, Integer>) plugin
								.getTopVoter(TopVoter.Monthly).clone();
						topVoters = clone;
					}

					for (Entry<TopVoterPlayer, Integer> entry : topVoters.entrySet()) {
						if (plugin.getConfigFile().isTopVoterAwardsTies()) {
							if (entry.getValue().intValue() != lastTotal) {
								i++;
							}
						} else {
							i++;
						}

						if (places.containsKey(i)) {
							VotingPluginUser user = entry.getKey().getUser();
							user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
							if (!plugin.getConfigFile().isTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
								user.giveMonthlyTopVoterAward(i, places.get(i));
								plugin.getLogger().info("Giving Monthly top voter reward " + i + " to "
										+ entry.getKey().getPlayerName());
							}
						}
						lastTotal = entry.getValue().intValue();
					}

				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			for (String shopIdent : plugin.getShopFile().getShopIdentifiers()) {
				if (plugin.getShopFile().getVoteShopResetMonthly(shopIdent)) {
					resetVoteShopLimit(shopIdent,
							VoteShopPurchaseService.currentLimitGenerationId(plugin, shopIdent));
				}
			}

			// give time for other servers to catch up
			if (!bungeeHandleResets() && plugin.getBungeeSettings().isUseBungeecoord()) {
				plugin.debug("Delaying time change 10 seconds for other servers to catchup");
				try {
					Thread.sleep(10000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			if (!bungeeHandleResets()) {
				resetTotals(TopVoter.Monthly);
			}

			if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
				plugin.getMysql().clearCacheBasic();
			}
			long now = ((System.currentTimeMillis() - startTime) / 1000);
			plugin.getLogger().info("Finished processing day change, took " + now + " seconds");
		}
	}

	/**
	 * Handles pre-date change events.
	 * @param event the pre-date changed event
	 */
	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onPreDateChanged(PreDateChangedEvent event) {
		TimeChangeTransition transition = event.getTransition();
		if (transition != null) {
			TimeChangeTransition.Lease lease = transition.retain();
			try {
				ensureTransitionActive(transition);
				applyPreDateChange(event.getTimeType());
				ensureTransitionActive(transition);
				lease.complete();
			} catch (Throwable failure) {
				lease.fail(failure);
				plugin.getLogger().warning("Pre time-change work remains pending: "
						+ failure.getClass().getSimpleName());
				plugin.debug(failure);
			}
			return;
		}
		applyPreDateChange(event.getTimeType());
	}

	private void applyPreDateChange(TimeType type) {
		if (type.equals(TimeType.DAY)) {
			plugin.getBannedPlayers().clear();
			for (OfflinePlayer p : Bukkit.getBannedPlayers()) {
				plugin.getBannedPlayers().add(p.getUniqueId().toString());
			}
		}
		plugin.setUpdate(true);
		plugin.update();
	}

	/**
	 * Handles week change events.
	 *
	 * @param event week change event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onWeekChange(WeekChangeEvent event) {
		if (event.getTransition() != null) {
			processRecoverableChange(TopVoter.Weekly, event.getTransition());
			return;
		}
		long startTime = System.currentTimeMillis();
		synchronized (VotingPluginMain.plugin) {
			if (plugin.getConfigFile().isStoreTopVotersWeekly()) {
				plugin.getLogger().info("Saving TopVoters Weekly");
				storeTopVoters(TopVoter.Weekly);
			}

			plugin.getUserManager().copyColumnData(TopVoter.Weekly.getColumnName(),
					TopVoter.Weekly.getLastColumnName());
			if (plugin.getConfigFile().isUseVoteStreaks() || plugin.getConfigFile().isUseHighestTotals()) {
				plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
					user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
					user.updateTempCacheWithColumns(columns);

					if (plugin.getConfigFile().isUseVoteStreaks()) {
						if (user.getTotal(TopVoter.Weekly) == 0 && user.getWeekVoteStreak() != 0) {
							user.setWeekVoteStreak(0);
						} else {
							if (!plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage()
									|| user.hasPercentageTotal(TopVoter.Weekly,
											plugin.getSpecialRewardsConfig().getVoteStreakRequirementWeek(), null)) {
								user.addWeekVoteStreak();
								plugin.getSpecialRewards().checkVoteStreak(null, user, "Week",
										plugin.getBungeeSettings().isUseBungeecoord());
							}
						}
					}

					if (plugin.getConfigFile().isUseHighestTotals()) {
						if (user.getHighestWeeklyTotal() < user.getTotal(TopVoter.Weekly)) {
							user.setHighestWeeklyTotal(user.getTotal(TopVoter.Weekly));
						}
					}
					user.clearTempCache();
				}, (count) -> {
					// finished
				});
			}

			try {
				if (plugin.getSpecialRewardsConfig().isEnableWeeklyAwards()) {
					HashMap<Integer, String> places = handlePlaces(
							plugin.getSpecialRewardsConfig().getWeeklyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;
					@SuppressWarnings("unchecked")
					LinkedHashMap<TopVoterPlayer, Integer> clone = (LinkedHashMap<TopVoterPlayer, Integer>) plugin
							.getTopVoter(TopVoter.Weekly).clone();
					for (Entry<TopVoterPlayer, Integer> entry : clone.entrySet()) {
						if (plugin.getConfigFile().isTopVoterAwardsTies()) {
							if (entry.getValue().intValue() != lastTotal) {
								i++;
							}
						} else {
							i++;
						}
						if (places.containsKey(i)) {
							VotingPluginUser user = entry.getKey().getUser();
							user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
							if (!plugin.getConfigFile().isTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
								user.giveWeeklyTopVoterAward(i, places.get(i));
								plugin.getLogger().info("Giving weekly top voter reward " + i + " to "
										+ entry.getKey().getPlayerName());
							}
						}
						lastTotal = entry.getValue().intValue();
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			for (String shopIdent : plugin.getShopFile().getShopIdentifiers()) {
				if (plugin.getShopFile().getVoteShopResetWeekly(shopIdent)) {
					resetVoteShopLimit(shopIdent,
							VoteShopPurchaseService.currentLimitGenerationId(plugin, shopIdent));
				}
			}

			// give time for other servers to catch up
			if (!plugin.getTopVoterHandler().bungeeHandleResets() && plugin.getBungeeSettings().isUseBungeecoord()) {
				plugin.debug("Delaying time change 10 seconds for other servers to catchup");
				try {
					Thread.sleep(10000);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			if (!bungeeHandleResets()) {
				resetTotals(TopVoter.Weekly);
			}

			if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
				plugin.getMysql().clearCacheBasic();
			}

			long now = ((System.currentTimeMillis() - startTime) / 1000);
			plugin.getLogger().info("Finished processing day change, took " + now + " seconds");
		}
	}

	/**
	 * Performs the existing period work with a durable cursor. The transition
	 * lease is deliberately retained until DateChangedEvent has applied its post
	 * effects, because AdvancedCore completes its marker only after that lease.
	 */
	private void processRecoverableChange(TopVoter top, TimeChangeTransition transition) {
		TimeChangeTransition.Lease lease = transition.retain();
		try {
			synchronized (VotingPluginMain.plugin) {
				plugin.getServerData().beginTimeChangeRecovery(transition);
				plugin.getServerData().prepareTimeChangeUserPolicy(transition, currentTimeChangeUserPolicy());
				plugin.getServerData().prepareTimeChangeVoteShopTargets(transition,
						currentVoteShopResetTargets(top));
				plugin.getServerData().prepareTimeChangeTopPolicy(transition, currentTimeChangeTopPolicy(top));
				if (!plugin.getServerData().hasTimeChangePhase(transition, COMPLETE)) {
					runRecoverablePeriod(top, transition);
				}
				ensureTransitionActive(transition);
				synchronized (retainedTransitions) {
					retainedTransitions.put(transition.getId(), lease);
				}
			}
		} catch (Throwable failure) {
			lease.fail(failure);
			plugin.getLogger().warning("Time change recovery for " + top + " remains pending: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private TimeChangeUserPolicy currentTimeChangeUserPolicy() {
		boolean proxyOwnsResets = bungeeHandleResets();
		return new TimeChangeUserPolicy(plugin.getConfigFile().isUseVoteStreaks(),
				plugin.getConfigFile().isUseHighestTotals(),
				plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal(),
				plugin.getSpecialRewardsConfig().isVoteStreakRequirementUsePercentage(),
				plugin.getSpecialRewardsConfig().getVoteStreakRequirementDay(),
				plugin.getSpecialRewardsConfig().getVoteStreakRequirementWeek(),
				plugin.getSpecialRewardsConfig().getVoteStreakRequirementMonth(), proxyOwnsResets,
				plugin.getBungeeSettings().isUseBungeecoord() && !proxyOwnsResets);
	}

	private List<String> currentVoteShopResetTargets(TopVoter top) {
		return plugin.getShopFile().getShopIdentifiers().stream()
				.filter(identifier -> shouldResetVoteShop(top, identifier)).toList();
	}

	private TimeChangeTopPolicy currentTimeChangeTopPolicy(TopVoter top) {
		boolean archiveRequired = (top == TopVoter.Daily && plugin.getConfigFile().isStoreTopVotersDaily())
				|| (top == TopVoter.Weekly && plugin.getConfigFile().isStoreTopVotersWeekly())
				|| top == TopVoter.Monthly;
		return new TimeChangeTopPolicy(isTopRewardEnabled(top),
				plugin.getConfigFile().isTopVoterAwardsTies(),
				plugin.getConfigFile().isTopVoterIgnorePermission(), archiveRequired,
				new ArrayList<>(getPossibleRewardPlaces(top)));
	}

	private void runRecoverablePeriod(TopVoter top, TimeChangeTransition transition) {
		// Capture the boundary before any long-running phase. The database journal
		// makes the later COPY_TOTALS retry a no-op, so votes accepted while reward
		// or user recovery is pending remain above this boundary.
		copyTotalBoundary(top, transition);

		if (!plugin.getServerData().hasTimeChangePhase(transition, SNAPSHOT)) {
			ensureTransitionActive(transition);
			boolean archiveRequired = plugin.getServerData().getTimeChangeTopPolicy(transition).archiveRequired();
			TimeChangeArchiveSnapshot proposedArchive = archiveRequired
					? buildTopVoterArchiveSnapshot(top, transition) : new TimeChangeArchiveSnapshot(List.of());
			plugin.getServerData().prepareTimeChangeSnapshot(transition,
					buildTopRewardSnapshot(top, transition), proposedArchive);
			if (archiveRequired) {
				plugin.getLogger().info("Saving TopVoters " + top);
				storeTopVoters(top, transition, plugin.getServerData().getTimeChangeArchive(transition));
			}
			plugin.getServerData().completeTimeChangePhase(transition, SNAPSHOT);
		}

		if (!plugin.getServerData().hasTimeChangePhase(transition, COPY_TOTALS)) {
			ensureTransitionActive(transition);
			copyTotalBoundary(top, transition);
			plugin.getServerData().completeTimeChangePhase(transition, COPY_TOTALS);
		}

		if (!plugin.getServerData().hasTimeChangePhase(transition, USER_UPDATES)) {
			processRecoverableUsers(top, transition);
			plugin.getServerData().completeTimeChangePhase(transition, USER_UPDATES);
		}
		if (top == TopVoter.Daily && plugin.getStorageType().equals(UserStorage.MYSQL)
				&& !VoteShopPurchaseService.completeMysqlDailyStreakReset(plugin,
						"time-streak-reset:" + transition.getId())) {
			throw new IllegalStateException("Unable to publish the completed daily streak reset");
		}

		if (!plugin.getServerData().hasTimeChangePhase(transition, TOP_REWARDS)) {
			processRecoverableTopRewards(top, transition);
			plugin.getServerData().completeTimeChangePhase(transition, TOP_REWARDS);
		}

		processRecoverableVoteShop(top, transition);

		if (!plugin.getServerData().hasTimeChangePhase(transition, BUNGEE_WAIT)) {
			waitForBungee(top, transition);
			plugin.getServerData().completeTimeChangePhase(transition, BUNGEE_WAIT);
		}

		if (!plugin.getServerData().hasTimeChangePhase(transition, TOTALS_RESET)) {
			ensureTransitionActive(transition);
			if (!plugin.getServerData().getTimeChangeUserPolicy(transition).proxyOwnsResets()) {
				resetTotals(top, transition);
			}
			plugin.getServerData().completeTimeChangePhase(transition, TOTALS_RESET);
		}

		if (!plugin.getServerData().hasTimeChangePhase(transition, CACHE_CLEAR)) {
			ensureTransitionActive(transition);
			if (plugin.getStorageType().equals(UserStorage.MYSQL)) plugin.getMysql().clearCacheBasic();
			plugin.getServerData().completeTimeChangePhase(transition, CACHE_CLEAR);
		}
	}

	void copyTotalBoundary(TopVoter top, TimeChangeTransition transition) {
		ensureTransitionActive(transition);
		boolean[] copied = { false };
		PeriodTotalMutationFence.withReset(() -> {
			copied[0] = TimeChangeTotalReset.copyBoundary(plugin, top.getColumnName(), top.getLastColumnName(),
					"time-copy:" + transition.getId());
			if (copied[0] && top == TopVoter.Daily
					&& plugin.getServerData().getTimeChangeUserPolicy(transition).voteStreaks()) {
				copied[0] = TimeChangeTotalReset.copyDailyStreakBoundary(plugin,
						"time-streak-copy:" + transition.getId());
			}
		});
		if (!copied[0]) {
			throw new IllegalStateException("Unable to durably copy " + top + " boundary state");
		}
	}

	void processRecoverableUsers(TopVoter top, TimeChangeTransition transition) {
		TimeChangeUserPolicy policy = plugin.getServerData().getTimeChangeUserPolicy(transition);
		if (!policy.voteStreaks() && !policy.highestTotals()) return;
		AtomicReference<String> cursor = new AtomicReference<>(plugin.getServerData().getTimeChangeCursor(transition));
		LocalDateTime lastMonthTime = top == TopVoter.Monthly ? previousMonthTime(transition) : null;
		// AdvancedCore streams deterministic UUID-ordered SQL pages synchronously.
		// Process each row as it arrives so memory stays bounded and a failure cannot
		// leave an uncancelled enumeration running behind a falsely failed transition.
		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			String value = uuid.toString();
			if (value.compareTo(cursor.get()) <= 0) return;
			try {
				ensureTransitionActive(transition);
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
				user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
				user.updateTempCacheWithColumns(columns);
				try {
					if (top == TopVoter.Daily) processDailyUser(user, transition, value, policy);
					else if (top == TopVoter.Weekly) processWeeklyUser(user, transition, value, policy);
					else processMonthlyUser(user, lastMonthTime, transition, value, policy);
					if (user.getCache() != null) user.getCache().flushChangesAndRun(() -> { });
				} finally {
					user.clearTempCache();
				}
				plugin.getServerData().completeTimeChangeUser(transition, value);
				cursor.set(value);
			} catch (Throwable userFailure) {
				throw new IllegalStateException("Unable to durably process time-change user " + value, userFailure);
			}
		}, count -> { });
	}

	void processDailyUser(VotingPluginUser user, TimeChangeTransition transition, String uuid) {
		processDailyUser(user, transition, uuid, plugin.getConfigFile().isUseVoteStreaks());
	}

	void processDailyUser(VotingPluginUser user, TimeChangeTransition transition, String uuid,
			boolean processVoteStreaks) {
		processDailyUser(user, transition, uuid, new TimeChangeUserPolicy(processVoteStreaks,
				plugin.getConfigFile().isUseHighestTotals(), false, false, 0, 0, 0, false, false));
	}

	void processDailyUser(VotingPluginUser user, TimeChangeTransition transition, String uuid,
			TimeChangeUserPolicy policy) {
		int boundaryTotal = user.getLastDailyTotal();
		if (policy.voteStreaks()) {
			PeriodTotalMutationFence.withReset(() -> {
				int boundaryStreak = user.getLastDayVoteStreak();
				long boundaryUpdate = user.getLastDayVoteStreakLastUpdate();
				if (!user.voteStreakUpdatedAt(boundaryUpdate, previousDayTime(transition)) && boundaryStreak != 0) {
					// A vote accepted after the boundary has already started the new day's
					// streak. Preserve that contribution while removing the stale streak.
					int target = user.getDayVoteStreakLastUpdate() == boundaryUpdate ? 0 : 1;
					applyRecoverableStreak(user, transition, uuid, TopVoter.Daily, target, false);
				}
			});
		}
		if (policy.highestTotals()
				&& user.getHighestDailyTotal() < boundaryTotal) {
			user.setHighestDailyTotal(boundaryTotal);
		}
	}

	void processWeeklyUser(VotingPluginUser user, TimeChangeTransition transition, String uuid) {
		processWeeklyUser(user, transition, uuid, currentTimeChangeUserPolicy());
	}

	void processWeeklyUser(VotingPluginUser user, TimeChangeTransition transition, String uuid,
			TimeChangeUserPolicy policy) {
		int boundaryTotal = user.getLastWeeklyTotal();
		if (policy.voteStreaks()) {
			if (boundaryTotal == 0 && user.getWeekVoteStreak() != 0) {
				applyRecoverableStreak(user, transition, uuid, TopVoter.Weekly, 0, false);
			} else if (!policy.streakUsesPercentage()
					|| user.hasPercentageTotal(TopVoter.Weekly,
							policy.weekPercentage(), null, boundaryTotal)) {
				applyRecoverableStreak(user, transition, uuid, TopVoter.Weekly,
						user.getWeekVoteStreak() + 1, true);
			}
		}
		if (policy.highestTotals()
				&& user.getHighestWeeklyTotal() < boundaryTotal) {
			user.setHighestWeeklyTotal(boundaryTotal);
		}
	}

	void processMonthlyUser(VotingPluginUser user, LocalDateTime lastMonthTime,
			TimeChangeTransition transition, String uuid) {
		processMonthlyUser(user, lastMonthTime, transition, uuid, currentTimeChangeUserPolicy());
	}

	void processMonthlyUser(VotingPluginUser user, LocalDateTime lastMonthTime,
			TimeChangeTransition transition, String uuid, TimeChangeUserPolicy policy) {
		int boundaryTotal = policy.monthDateTotalsPrimary()
				? user.getTotal(TopVoter.Monthly, lastMonthTime) : user.getLastMonthTotal();
		if (policy.voteStreaks()) {
			if (boundaryTotal == 0 && user.getMonthVoteStreak() != 0) {
				applyRecoverableStreak(user, transition, uuid, TopVoter.Monthly, 0, false);
			} else if (!policy.streakUsesPercentage()
					|| user.hasPercentageTotal(TopVoter.Monthly,
							policy.monthPercentage(), lastMonthTime,
							boundaryTotal)) {
				applyRecoverableStreak(user, transition, uuid, TopVoter.Monthly,
						user.getMonthVoteStreak() + 1, true);
			}
		}
		if (policy.highestTotals()
				&& user.getHighestMonthlyTotal() < boundaryTotal) {
			user.setHighestMonthlyTotal(boundaryTotal);
		}
	}

	void processRecoverableVoteShop(TopVoter top, TimeChangeTransition transition) {
		if (plugin.getServerData().hasTimeChangePhase(transition, VOTE_SHOP)) return;
		ensureTransitionActive(transition);
		String generation = VoteShopPurchaseService.limitGenerationIdForTransition(transition);
		for (String shopIdent : plugin.getServerData().getTimeChangeVoteShopTargets(transition)) {
			if (!resetVoteShopLimit(shopIdent, generation)) {
				throw new IllegalStateException("Unable to durably reset VoteShop limit " + shopIdent);
			}
		}
		plugin.getServerData().completeTimeChangePhase(transition, VOTE_SHOP);
	}

	void applyRecoverableStreak(VotingPluginUser user, TimeChangeTransition transition, String uuid,
			TopVoter top, int proposedTarget, boolean rewardRequired) {
		TimeChangeUserProgress progress = plugin.getServerData().prepareTimeChangeUserStreak(transition, uuid,
				proposedTarget, rewardRequired);
		int current = switch (top) {
		case Daily -> user.getDayVoteStreak();
		case Weekly -> user.getWeekVoteStreak();
		case Monthly -> user.getMonthVoteStreak();
		default -> proposedTarget;
		};
		if (top == TopVoter.Daily && UserStorage.MYSQL.equals(plugin.getStorageType())) {
			if (!VoteShopPurchaseService.resetMysqlDailyStreakAtBoundary(plugin, uuid,
					user.getLastDayVoteStreakLastUpdate())) {
				throw new IllegalStateException("Unable to serialize shared MySQL daily streak reset");
			}
		} else if (current != progress.streakTarget()) {
			switch (top) {
			case Daily -> user.setDayVoteStreak(progress.streakTarget());
			case Weekly -> user.setWeekVoteStreak(progress.streakTarget());
			case Monthly -> user.setMonthVoteStreak(progress.streakTarget());
			default -> { }
			}
		}
		if (progress.rewardRequired() && !progress.rewardComplete()) {
			TimeChangeRewardState rewardState = plugin.getServerData()
					.getTimeChangeUserStreakRewardState(transition, uuid);
			if (rewardState == TimeChangeRewardState.CLAIMED) {
				throw new IllegalStateException("Streak reward for " + uuid
						+ " may already have run and requires manual reconciliation");
			}
			if (rewardState == TimeChangeRewardState.COMPLETE) return;
			plugin.getServerData().claimTimeChangeUserStreakReward(transition, uuid);
			plugin.getSpecialRewards().checkVoteStreak(null, user,
					top == TopVoter.Weekly ? "Week" : "Month", plugin.getBungeeSettings().isUseBungeecoord());
			plugin.getServerData().completeTimeChangeUserStreakReward(transition, uuid);
		}
	}

	void processRecoverableTopRewards(TopVoter top, TimeChangeTransition transition) {
		for (TimeChangeRewardTarget target : plugin.getServerData().getTimeChangeRewardTargets(transition)) {
			ensureTransitionActive(transition);
			TimeChangeRewardState rewardState = plugin.getServerData()
					.getTimeChangeRewardState(transition, target.uuid());
			if (rewardState == TimeChangeRewardState.COMPLETE) continue;
			if (rewardState == TimeChangeRewardState.CLAIMED) {
				throw new IllegalStateException("Top voter reward for " + target.uuid()
						+ " may already have run and requires manual reconciliation");
			}
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(
					UUID.fromString(target.uuid()), target.playerName());
			user.userDataFetechMode(UserDataFetchMode.NO_CACHE);
			if (!plugin.getServerData().getTimeChangeTopPolicy(transition).ignorePermission()
					|| !user.isTopVoterIgnore()) {
				plugin.getServerData().claimTimeChangeReward(transition, target.uuid());
				giveTopVoterAward(top, user, target.place(), target.reward(), target.votes());
				plugin.getServerData().completeTimeChangeReward(transition, target.uuid());
				plugin.getLogger().info("Giving " + top + " top voter reward " + target.place() + " to "
						+ target.playerName());
			}
		}
	}

	List<TimeChangeRewardTarget> buildTopRewardSnapshot(TopVoter top, TimeChangeTransition transition) {
		TimeChangeTopPolicy policy = plugin.getServerData().getTimeChangeTopPolicy(transition);
		if (!policy.rewardsEnabled()) return List.of();
		HashMap<Integer, String> places = handlePlaces(Set.copyOf(policy.rewardPlaces()));
		List<TimeChangeRewardTarget> targets = new ArrayList<>();
		int place = 0;
		int lastTotal = -1;
		for (Entry<TopVoterPlayer, Integer> entry : boundaryTopVotersFor(top, transition).entrySet()) {
			ensureTransitionActive(transition);
			if (policy.awardTies()) {
				if (entry.getValue().intValue() != lastTotal) place++;
			} else place++;
			if (places.containsKey(place)) {
				targets.add(new TimeChangeRewardTarget(entry.getKey().getUuid().toString(),
						entry.getKey().getPlayerName() == null ? "" : entry.getKey().getPlayerName(),
						place, places.get(place), entry.getValue().intValue()));
			}
			lastTotal = entry.getValue().intValue();
		}
		return List.copyOf(targets);
	}

	LinkedHashMap<TopVoterPlayer, Integer> boundaryTopVotersFor(TopVoter top,
			TimeChangeTransition transition) {
		if (top == TopVoter.Monthly
				&& plugin.getServerData().getTimeChangeUserPolicy(transition).monthDateTotalsPrimary()) {
			return loader.getBoundaryMonthlyTopVotersAtTime(previousMonthTime(transition));
		}
		return loader.getBoundaryTopVoters(top);
	}

	private LocalDateTime previousMonthTime(TimeChangeTransition transition) {
		try {
			return YearMonth.parse(transition.getPeriodKey()).minusMonths(1).atDay(15).atStartOfDay();
		} catch (RuntimeException invalidPeriod) {
			plugin.debug(invalidPeriod);
			return plugin.getTimeChecker().getTime().minusMonths(1);
		}
	}

	LocalDateTime previousDayTime(TimeChangeTransition transition) {
		try {
			return LocalDate.parse(transition.getPeriodKey()).minusDays(1).atStartOfDay();
		} catch (RuntimeException invalidPeriod) {
			plugin.debug(invalidPeriod);
			return plugin.getTimeChecker().getTime().minusDays(1);
		}
	}

	private boolean isTopRewardEnabled(TopVoter top) {
		return switch (top) {
		case Daily -> plugin.getSpecialRewardsConfig().isEnableDailyRewards();
		case Weekly -> plugin.getSpecialRewardsConfig().isEnableWeeklyAwards();
		case Monthly -> plugin.getSpecialRewardsConfig().isEnableMonthlyAwards();
		default -> false;
		};
	}

	private Set<String> getPossibleRewardPlaces(TopVoter top) {
		return switch (top) {
		case Daily -> plugin.getSpecialRewardsConfig().getDailyPossibleRewardPlaces();
		case Weekly -> plugin.getSpecialRewardsConfig().getWeeklyPossibleRewardPlaces();
		case Monthly -> plugin.getSpecialRewardsConfig().getMonthlyPossibleRewardPlaces();
		default -> Collections.emptySet();
		};
	}

	private void giveTopVoterAward(TopVoter top, VotingPluginUser user, int place, String reward, int votes) {
		switch (top) {
		case Daily -> user.giveDailyTopVoterAward(place, reward, votes);
		case Weekly -> user.giveWeeklyTopVoterAward(place, reward, votes);
		case Monthly -> user.giveMonthlyTopVoterAward(place, reward, votes);
		default -> { }
		}
	}

	private boolean shouldResetVoteShop(TopVoter top, String shopIdent) {
		return switch (top) {
		case Daily -> plugin.getShopFile().getVoteShopResetDaily(shopIdent);
		case Weekly -> plugin.getShopFile().getVoteShopResetWeekly(shopIdent);
		case Monthly -> plugin.getShopFile().getVoteShopResetMonthly(shopIdent);
		default -> false;
		};
	}

	private void waitForBungee(TopVoter top, TimeChangeTransition transition) {
		boolean wait = plugin.getServerData().getTimeChangeUserPolicy(transition).waitForProxy();
		if (!wait) return;
		ensureTransitionActive(transition);
		plugin.debug("Delaying time change 10 seconds for other servers to catchup");
		try {
			Thread.sleep(10000);
		} catch (InterruptedException interrupted) {
			Thread.currentThread().interrupt();
			throw new CancellationException("Time change sleep was interrupted");
		}
		ensureTransitionActive(transition);
	}

	private void finishRecoverableDateChange(DateChangedEvent event) {
		TimeChangeTransition transition = event.getTransition();
		TimeChangeTransition.Lease lease;
		synchronized (retainedTransitions) {
			lease = retainedTransitions.remove(transition.getId());
		}
		if (lease == null) {
			return;
		}
		try {
			ensureTransitionActive(transition);
			if (!plugin.getServerData().hasTimeChangePhase(transition, POST_DATE)) {
				plugin.setUpdate(true);
				plugin.update();
				if (transition.getType().equals(TimeType.MONTH)) loadLastMonth();
				if (plugin.getStorageType().equals(UserStorage.MYSQL)) plugin.getMysql().clearCacheBasic();
				plugin.getServerData().completeTimeChangePhase(transition, POST_DATE);
			}
			ensureTransitionActive(transition);
			plugin.getServerData().completeTimeChangePhase(transition, COMPLETE);
			lease.complete();
		} catch (Throwable failure) {
			lease.fail(failure);
			plugin.getLogger().warning("Post time-change recovery remains pending: "
					+ failure.getClass().getSimpleName());
			plugin.debug(failure);
		}
	}

	private void ensureTransitionActive(TimeChangeTransition transition) {
		if (transition.isCancellationRequested()) {
			throw new CancellationException("Time transition was cancelled before recovery completed");
		}
	}

	/**
	 * Registers this handler as a listener.
	 */
	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	/**
	 * Resets vote totals for a specific period.
	 * @param topVoter the top voter period to reset
	 */
	public void resetTotals(TopVoter topVoter) {
		plugin.getUserManager().removeAllKeyValues(topVoter.getColumnName(), DataType.INTEGER);
	}

	void resetTotals(TopVoter topVoter, TimeChangeTransition transition) {
		String generation = "time-total:" + transition.getId();
		if (!TimeChangeTotalReset.reset(plugin, topVoter.getColumnName(), topVoter.getLastColumnName(), generation)) {
			throw new IllegalStateException("Unable to durably reset " + topVoter + " totals");
		}
	}

	/**
	 * Resets vote shop limits for a specific shop.
	 * @param shopIdent the shop identifier
	 */
	public void resetVoteShopLimit(String shopIdent) {
		resetVoteShopLimit(shopIdent, null);
	}

	private boolean resetVoteShopLimit(String shopIdent, String resetGeneration) {
		String limitColumn = "VoteShopLimit" + shopIdent;
		if (resetGeneration != null) {
			if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
				return VoteShopPurchaseService.resetMysqlLimitWithPurchaseFence(plugin, limitColumn, resetGeneration);
			}
			if (UserStorage.SQLITE.equals(plugin.getStorageType())) {
				return VoteShopLimitMutationFence.withLock(() -> TimeChangeTotalReset.resetToZero(plugin, limitColumn,
						resetGeneration + ':' + limitColumn));
			}
			return false;
		}
		if (UserStorage.MYSQL.equals(plugin.getStorageType()) && !plugin.getBungeeSettings().isPerServerPoints()) {
			VoteShopPurchaseService.resetSharedMysqlLimit(plugin, limitColumn);
			return true;
		}
		if (UserStorage.MYSQL.equals(plugin.getStorageType())) {
			// The limit column is still shared with backends that have not switched to
			// per-server points.  Its wipe and epoch advance must therefore use the
			// journal's one transaction; doing the UserManager wipe after advancing the
			// epoch can erase a new-epoch reservation.
			return VoteShopPurchaseService.resetMysqlLimitWithPurchaseFence(plugin, limitColumn,
					UUID.randomUUID().toString());
		}
		VoteShopLimitMutationFence.withLock(
				() -> plugin.getUserManager().removeAllKeyValues(limitColumn, DataType.INTEGER));
		return true;
	}

	/**
	 * Sorts top voters by their vote counts.
	 * @param map the map to sort
	 * @param order true for ascending order, false for descending
	 * @return sorted map of top voters
	 */
	public LinkedHashMap<TopVoterPlayer, Integer> sortByValues(LinkedHashMap<TopVoterPlayer, Integer> map,
			final boolean order) {
		return TopVoterRanking.sortByValues(map, order);
	}


	/**
	 * Stores top voter data to file.
	 * @param top the top voter period to store
	 */
	public void storeTopVoters(TopVoter top) {
		LocalDateTime time = LocalDateTime.now().minusDays(1);
		String month = time.getMonth().toString();
		int year = time.getYear();
		int week = time.getDayOfYear();
		int day = time.getDayOfMonth();
		String fileName = "TopVoter" + File.separator + top.toString() + File.separator + year + "_" + month;

		if (top.equals(TopVoter.Daily)) {
			fileName += "_" + day;
		} else if (top.equals(TopVoter.Weekly)) {
			fileName += "_" + week;
		}
		fileName += "_" + System.currentTimeMillis() + ".yml";

		YMLFileHandler file = new YMLFileHandler(plugin, new File(plugin.getDataFolder(), fileName));
		file.setup();
		file.header("Saving top voters for " + top.toString() + ", file also contains other top voter info as backup");
		for (TopVoter cTop : TopVoter.values()) {
			ArrayList<String> topVoters = new ArrayList<>();
			int cTotal = 0;
			try {
				ArrayList<Integer> nums = plugin.getUserManager().getNumbersInColumn(cTop.getColumnName());
				for (Integer num : nums) {
					cTotal += num.intValue();
				}
				topVoters.add("Combined total: " + cTotal);
			} catch (Exception e) {
				e.printStackTrace();
			}
			if (plugin.getTopVoter().containsKey(cTop)) {

				int count = 1;
				for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(cTop).entrySet()) {
					topVoters.add(count + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
					count++;
				}
				file.getData().set(cTop.toString(), topVoters);
			}
		}
		file.saveData();
	}

	TimeChangeArchiveSnapshot buildTopVoterArchiveSnapshot(TopVoter boundaryTop,
			TimeChangeTransition transition) {
		List<TimeChangeArchiveSection> sections = new ArrayList<>();
		TopVoterLoader.BoundaryRanking boundary = boundaryRankingFor(boundaryTop, transition);
		LinkedHashMap<TopVoterPlayer, Integer> boundaryRanking = boundary.players();
		for (TopVoter current : TopVoter.values()) {
			ArrayList<String> lines = new ArrayList<>();
			int total = 0;
			if (current == boundaryTop) {
				total = boundary.combinedTotal();
			} else {
				try {
					for (Integer value : plugin.getUserManager().getNumbersInColumn(current.getColumnName())) {
						total += value.intValue();
					}
				} catch (Exception failure) {
					plugin.debug(failure);
					throw new IllegalStateException("Unable to build " + current + " archive total", failure);
				}
			}
			lines.add("Combined total: " + total);
			if (current == boundaryTop || plugin.getTopVoter().containsKey(current)) {
				int place = 1;
				Iterable<Entry<TopVoterPlayer, Integer>> ranking = current == boundaryTop
						? boundaryRanking.entrySet() : plugin.getTopVoter(current).entrySet();
				for (Entry<TopVoterPlayer, Integer> entry : ranking) {
					lines.add(place + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
					place++;
				}
				sections.add(new TimeChangeArchiveSection(current.toString(), lines));
			}
		}
		return new TimeChangeArchiveSnapshot(sections);
	}

	private TopVoterLoader.BoundaryRanking boundaryRankingFor(TopVoter top,
			TimeChangeTransition transition) {
		if (top == TopVoter.Monthly
				&& plugin.getServerData().getTimeChangeUserPolicy(transition).monthDateTotalsPrimary()) {
			return loader.getBoundaryRanking(TopVoter.Monthly, previousMonthTime(transition));
		}
		return loader.getBoundaryRanking(top, null);
	}

	void storeTopVoters(TopVoter top, TimeChangeTransition transition, TimeChangeArchiveSnapshot snapshot) {
		String fileName = timeChangeArchiveFileName(top, transition);
		YMLFileHandler file = new YMLFileHandler(plugin, new File(plugin.getDataFolder(), fileName));
		file.setup();
		file.header("Saving top voters for " + top + ", file also contains other top voter info as backup");
		for (TimeChangeArchiveSection section : snapshot.sections()) {
			file.getData().set(section.name(), section.lines());
		}
		file.saveData();
	}

	String timeChangeArchiveFileName(TopVoter top, TimeChangeTransition transition) {
		String stablePeriod = transition.getPeriodKey().replaceAll("[^A-Za-z0-9_-]", "_");
		return "TopVoter" + File.separator + top + File.separator + top + "_" + stablePeriod + ".yml";
	}

	/**
	 * Top voter all time
	 *
	 * @param page the page
	 * @return the string[]
	 */
	public String[] topVoterAllTime(int page) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<String> topVoters = new ArrayList<>();
		int count = 1;
		for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
			String line = plugin.getConfigFile().getFormatCommandsVoteTopLine();
			line = line.replace("%num%", "" + count);
			line = line.replace("%player%", entry.getKey().getPlayerName());
			line = line.replace("%votes%", "" + entry.getValue());
			topVoters.add(line);
			count++;
		}

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = plugin.getConfigFile().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", plugin.getConfigFile().getFormatTopVoterAllTime());
		msg.add(MessageAPI.colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets daily top voters with pagination.
	 * @param page the page number
	 * @return array of formatted top voter lines
	 */
	public String[] topVoterDaily(int page) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<String> topVoters = new ArrayList<>();
		int count = 1;
		for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
			String line = plugin.getConfigFile().getFormatCommandsVoteTopLine();
			line = line.replace("%num%", "" + count);
			line = line.replace("%player%", entry.getKey().getPlayerName());
			line = line.replace("%votes%", "" + entry.getValue());
			topVoters.add(line);
			count++;
		}

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = plugin.getConfigFile().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", plugin.getConfigFile().getFormatTopVoterDaily());
		msg.add(MessageAPI.colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets monthly top voters with pagination.
	 * @param page the page number
	 * @return array of formatted top voter lines
	 */
	public String[] topVoterMonthly(int page) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<String> topVoters = new ArrayList<>();
		int count = 1;
		for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
			String line = plugin.getConfigFile().getFormatCommandsVoteTopLine();
			line = line.replace("%num%", "" + count);
			line = line.replace("%player%", entry.getKey().getPlayerName());
			line = line.replace("%votes%", "" + entry.getValue());
			topVoters.add(line);
			count++;
		}

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = plugin.getConfigFile().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", plugin.getConfigFile().getFormatTopVoterMonthly());
		msg.add(MessageAPI.colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets all-time top voters as formatted strings.
	 * @return array of formatted top voter lines
	 */
	public String[] topVotersAllTime() {
		ArrayList<String> msg = new ArrayList<>();
		List<Entry<TopVoterPlayer, Integer>> list = new LinkedList<>(plugin.getTopVoter(TopVoter.AllTime).entrySet());
		int i = 0;
		for (Entry<TopVoterPlayer, Integer> entry : list) {
			String line = "%num%: %player%, %votes%";
			line = line.replace("%num%", "" + (i + 1));
			try {
				line = line.replace("%player%", entry.getKey().getPlayerName());
			} catch (Exception ex) {
				VotingPluginMain.plugin.debug(ex);
			}
			line = line.replace("%votes%", "" + entry.getValue());

			msg.add(line);
			i++;
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets daily top voters as formatted strings.
	 * @return array of formatted top voter lines
	 */
	public String[] topVotersDaily() {
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<TopVoterPlayer> users = new ArrayList<>(plugin.getTopVoter(TopVoter.Daily).keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = "%num%: %player%, %votes%";
			line = line.replace("%num%", "" + (i + 1));
			try {
				line = line.replace("%player%", users.get(i).getPlayerName());
			} catch (Exception ex) {
				VotingPluginMain.plugin.debug(ex);
			}
			line = line.replace("%votes%", "" + plugin.getTopVoter(TopVoter.Monthly).get(users.get(i)));
			msg.add(line);
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets monthly top voters as formatted strings.
	 * @return array of formatted top voter lines
	 */
	public String[] topVotersMonthly() {
		ArrayList<String> msg = new ArrayList<>();
		List<Entry<TopVoterPlayer, Integer>> list = new LinkedList<>(plugin.getTopVoter(TopVoter.Monthly).entrySet());
		int i = 0;
		for (Entry<TopVoterPlayer, Integer> entry : list) {
			String line = "%num%: %player%, %votes%";
			line = line.replace("%num%", "" + (i + 1));
			try {
				line = line.replace("%player%", entry.getKey().getPlayerName());
			} catch (Exception ex) {
				VotingPluginMain.plugin.debug(ex);
			}
			line = line.replace("%votes%", "" + entry.getValue());

			msg.add(line);
			i++;
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Gets weekly top voters with pagination.
	 * @param page the page number
	 * @return array of formatted top voter lines
	 */
	public String[] topVoterWeekly(int page) {
		int pagesize = plugin.getConfigFile().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<String> topVoters = new ArrayList<>();
		int count = 1;
		for (Entry<TopVoterPlayer, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
			String line = plugin.getConfigFile().getFormatCommandsVoteTopLine();
			line = line.replace("%num%", "" + count);
			line = line.replace("%player%", entry.getKey().getPlayerName());
			line = line.replace("%votes%", "" + entry.getValue());
			topVoters.add(line);
			count++;
		}

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = plugin.getConfigFile().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", plugin.getConfigFile().getFormatTopVoterWeekly());
		msg.add(MessageAPI.colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.colorize(msg);
		return ArrayUtils.convert(msg);
	}

	/**
	 * Updates top voter rankings for a specific period.
	 * @param tempTopVoter map of top voter periods to player rankings
	 */
	public synchronized void updateTopVoters(
			LinkedHashMap<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> tempTopVoter) {

		int limitSize = plugin.getConfigFile().getMaxiumNumberOfTopVotersToLoad();
		for (Entry<TopVoter, LinkedHashMap<TopVoterPlayer, Integer>> entry : tempTopVoter.entrySet()) {
			LinkedHashMap<TopVoterPlayer, Integer> map = entry.getValue();
			map = sortByValues(map, false);
			if (limitSize > 0) {
				ArrayList<TopVoterPlayer> listKeys = new ArrayList<>(map.keySet());
				if (listKeys.size() > limitSize) {
					for (int i = listKeys.size() - 1; i >= 0 && i >= limitSize; i--) {
						map.remove(listKeys.get(i));
					}
				}

			}
			tempTopVoter.put(entry.getKey(), map);
			plugin.setTopVoter(tempTopVoter);
			plugin.debug(entry.getKey().getName() + " TopVoter loaded, number of players " + map.size());

		}

		plugin.debug("Updated TopVoter");
	}
}
