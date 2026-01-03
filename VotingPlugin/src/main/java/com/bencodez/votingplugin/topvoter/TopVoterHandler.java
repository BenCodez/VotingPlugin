package com.bencodez.votingplugin.topvoter;

import java.io.File;
import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.PreDateChangedEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.advancedcore.api.user.UserStorage;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.simpleapi.file.YMLFileHandler;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.simpleapi.sql.DataType;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

public class TopVoterHandler implements Listener {

	private VotingPluginMain plugin;

	public TopVoterHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public boolean bungeeHandleResets() {
		if (plugin.getBungeeSettings().isUseBungeecoord()) {
			if (plugin.getBungeeSettings().isGloblalDataEnabled()) {
				return true;
			}
		}

		return false;
	}

	public String getGottenMilestonesPath() {
		if (plugin.getBungeeSettings().isPerServerMilestones()) {
			return plugin.getBungeeSettings().getServerNameStorage() + "_" + "GottenMilestones";
		}
		return "GottenMileStones";
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getMonthlyTopVotersAtTime(LocalDateTime atTime) {
		// int limitSize = plugin.getConfigFile().getMaxiumNumberOfTopVotersToLoad();

		LinkedHashMap<TopVoterPlayer, Integer> topVoter = new LinkedHashMap<>();

		// forEachUserKeys is callback-based; wait for completion because this method
		// returns a value.
		// NOTE: do not call this from the main thread if forEachUserKeys is async.
		CountDownLatch latch = new CountDownLatch(1);

		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (plugin != null && plugin.isEnabled() && uuid != null) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
				user.dontCache();
				user.updateTempCacheWithColumns(columns);

				int total = user.getTotal(TopVoter.Monthly, atTime);
				if (total > 0) {
					topVoter.put(user.getTopVoterPlayer(), total);
				}

				user.clearTempCache();
			}
		}, (count) -> {
			latch.countDown();
		});

		try {
			latch.await(10, TimeUnit.MINUTES);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}

		return sortByValues(topVoter, false);

	}

	public ArrayList<String> getTopVoterBlackList() {
		return plugin.getConfigFile().getBlackList();
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getTopVotersOfMonth(YearMonth month,
			HashMap<UUID, ArrayList<Column>> cols) {

		LinkedHashMap<TopVoterPlayer, Integer> totals = new LinkedHashMap<>();
		LocalDateTime atTime = month.atDay(15).atTime(0, 0);
		for (Entry<UUID, ArrayList<Column>> playerData : cols.entrySet()) {

			String uuid = playerData.getKey().toString();
			if (uuid != null && !uuid.isEmpty()) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(UUID.fromString(uuid),
						false);
				user.dontCache();
				user.updateTempCacheWithColumns(playerData.getValue());
				int total = 0;
				total = user.getTotal(TopVoter.Monthly, atTime);

				if (total > 0) {
					totals.put(user.getTopVoterPlayer(), total);
				}
				user.clearTempCache();
			}
		}

		return sortByValues(totals, false);
	}

	/**
	 * Top voters weekly.
	 *
	 * @return the string[]
	 */

	public String[] getTopVotersWeekly() {
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<TopVoterPlayer> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Weekly).keySet());
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
		HashMap<Integer, String> place = new HashMap<>();
		for (String p : places) {
			String[] data = p.split("-");
			try {
				if (data.length > 1) {
					for (int i = Integer.parseInt(data[0]); i < Integer.parseInt(data[1]); i++) {
						place.put(i, p);
					}
				} else {
					place.put(Integer.parseInt(data[0]), p);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		return place;
	}

	public void loadLastMonth() {
		if (!plugin.getGui().isLastMonthGUI()) {
			return;
		}

		plugin.getLastMonthTopVoter().clear();

		// If you don’t need insertion order here, HashMap is faster.
		final HashMap<TopVoterPlayer, Integer> totals = new HashMap<>();

		final LocalDateTime lastMonthTime = plugin.getTimeChecker().getTime().minusMonths(1);
		final boolean useDateTotalsPrimary = plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal();

		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (uuid == null || plugin == null || !plugin.isEnabled()) {
				return;
			}

			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			user.dontCache();
			user.updateTempCacheWithColumns(columns);

			try {
				final int total = useDateTotalsPrimary ? user.getTotal(TopVoter.Monthly, lastMonthTime)
						: user.getLastMonthTotal();

				if (total > 0) {
					// Only build tvp if we actually need it
					totals.put(user.getTopVoterPlayer(), total);
				}
			} finally {
				user.clearTempCache();
			}

		}, (count) -> {
			plugin.getLastMonthTopVoter().putAll(sortByValues(new LinkedHashMap<>(totals), false));
			plugin.debug("Loaded last month top voters");
		});
	}

	public void loadPreviousMonthTopVoters() {
		if (!plugin.getConfigFile().isStoreMonthTotalsWithDate()) {
			return;
		}

		final LocalDateTime now = plugin.getTimeChecker().getTime();
		final YearMonth currentYm = YearMonth.of(now.getYear(), now.getMonth());

		// Discover month columns -> YearMonth list + matching column names
		final ArrayList<YearMonth> months = new ArrayList<>();
		final ArrayList<String> monthColumns = new ArrayList<>();

		for (String column : plugin.getUserManager().getAllColumns()) {
			if (!column.startsWith("MonthTotal-")) {
				continue;
			}

			// Expected: MonthTotal-MONTH-YEAR (ex: MonthTotal-JANUARY-2025)
			// Faster than split("-") (regex)
			final int firstDash = column.indexOf('-');              // after MonthTotal
			final int secondDash = column.indexOf('-', firstDash + 1); // after MONTH
			if (firstDash < 0 || secondDash < 0) {
				continue;
			}

			final String monthStr = column.substring(firstDash + 1, secondDash);
			final String yearStr = column.substring(secondDash + 1);

			if (!MessageAPI.isInt(yearStr)) {
				continue;
			}

			try {
				final int year = Integer.parseInt(yearStr);
				final Month month = Month.valueOf(monthStr);
				final YearMonth ym = YearMonth.of(year, month);

				if (ym.isBefore(currentYm)) {
					months.add(ym);
					monthColumns.add(column);
				}
			} catch (Exception ignored) {
				// bad month string etc.
			}
		}

		if (months.isEmpty()) {
			return;
		}

		// Sort months AND keep monthColumns aligned
		{
			final Integer[] idx = new Integer[months.size()];
			for (int i = 0; i < idx.length; i++) idx[i] = i;

			java.util.Arrays.sort(idx, (a, b) -> months.get(a).compareTo(months.get(b)));

			final ArrayList<YearMonth> monthsSorted = new ArrayList<>(months.size());
			final ArrayList<String> colsSorted = new ArrayList<>(months.size());
			for (int i = 0; i < idx.length; i++) {
				monthsSorted.add(months.get(idx[i]));
				colsSorted.add(monthColumns.get(idx[i]));
			}

			months.clear();
			months.addAll(monthsSorted);

			monthColumns.clear();
			monthColumns.addAll(colsSorted);
		}

		// Prepare destination maps once
		final LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> prev = plugin.getPreviousMonthsTopVoters();
		prev.clear();

		for (YearMonth ym : months) {
			plugin.debug("Loading previous month top voters of " + ym);
			prev.put(ym, new LinkedHashMap<>());
		}

		@SuppressWarnings("unchecked")
		final LinkedHashMap<TopVoterPlayer, Integer>[] monthMaps = new LinkedHashMap[months.size()];
		for (int i = 0; i < months.size(); i++) {
			monthMaps[i] = prev.get(months.get(i));
		}

		// Snapshot arrays for faster access inside the loop
		final String[] monthColsArr = monthColumns.toArray(new String[0]);

		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (uuid == null) {
				return;
			}

			final VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			user.dontCache();
			user.updateTempCacheWithColumns(columns);

			try {
				TopVoterPlayer tvp = null;

				// Read MonthTotal-* columns directly (FAST)
				for (int i = 0; i < monthColsArr.length; i++) {
					final int total = user.getData().getIntTempOnly(monthColsArr[i], 0);
					if (total > 0) {
						if (tvp == null) {
							tvp = user.getTopVoterPlayer();
						}
						monthMaps[i].put(tvp, total);
					}
				}
			} finally {
				user.clearTempCache();
			}

		}, (count) -> {
			// Sort each month map once
			for (int i = 0; i < months.size(); i++) {
				final YearMonth ym = months.get(i);
				prev.put(ym, sortByValues(prev.get(ym), false));
			}

			plugin.extraDebug("Previous Months: " + prev.keySet());
		});
	}


	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onDateChanged(DateChangedEvent event) {
		plugin.setUpdate(true);
		plugin.update();
		if (event.getTimeType().equals(TimeType.MONTH)) {
			loadLastMonth();
		}
		if (plugin.getStorageType().equals(UserStorage.MYSQL)) {
			plugin.getMysql().clearCacheBasic();
		}
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChange(DayChangeEvent event) {
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
					user.dontCache();
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
							user.dontCache();
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
					resetVoteShopLimit(shopIdent);
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

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onMonthChange(MonthChangeEvent event) {
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
					user.dontCache();
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
							user.dontCache();
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

			if (plugin.getSpecialRewardsConfig().isResetMilestonesMonthly()) {
				resetMilestoneCount();
				resetGottenMilestones();
			}

			for (String shopIdent : plugin.getShopFile().getShopIdentifiers()) {
				if (plugin.getShopFile().getVoteShopResetMonthly(shopIdent)) {
					resetVoteShopLimit(shopIdent);
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

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onPreDateChanged(PreDateChangedEvent event) {
		if (event.getTimeType().equals(TimeType.DAY)) {
			plugin.getBannedPlayers().clear();
			for (OfflinePlayer p : Bukkit.getBannedPlayers()) {
				plugin.getBannedPlayers().add(p.getUniqueId().toString());
			}
		}
		plugin.setUpdate(true);
		plugin.update();
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onWeekChange(WeekChangeEvent event) {
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
					user.dontCache();
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
							user.dontCache();
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
					resetVoteShopLimit(shopIdent);
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

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void resetGottenMilestones() {
		plugin.getUserManager().removeAllKeyValues(getGottenMilestonesPath(), DataType.STRING);
	}

	public void resetMilestoneCount() {
		plugin.getUserManager().removeAllKeyValues("MilestoneCount", DataType.INTEGER);
	}

	public void resetTotals(TopVoter topVoter) {
		plugin.getUserManager().removeAllKeyValues(topVoter.getColumnName(), DataType.INTEGER);
	}

	public void resetVoteShopLimit(String shopIdent) {
		plugin.getUserManager().removeAllKeyValues("VoteShopLimit" + shopIdent, DataType.INTEGER);
	}

	public LinkedHashMap<TopVoterPlayer, Integer> sortByValues(LinkedHashMap<TopVoterPlayer, Integer> map,
			final boolean order) {

		List<Entry<TopVoterPlayer, Integer>> list = new LinkedList<>(map.entrySet());

		// Sorting the list based on values
		Collections.sort(list, new Comparator<Entry<TopVoterPlayer, Integer>>() {
			@Override
			public int compare(Entry<TopVoterPlayer, Integer> o1, Entry<TopVoterPlayer, Integer> o2) {
				if (order) {
					int result = (o1.getValue()).compareTo(o2.getValue());
					if (result != 0) {
						return result;
					}
					return o2.getKey().getLastVoteTime().compareTo(o1.getKey().getLastVoteTime());
				}
				int result = (o2.getValue()).compareTo(o1.getValue());
				if (result != 0) {
					return result;
				}
				return o1.getKey().getLastVoteTime().compareTo(o2.getKey().getLastVoteTime());
			}
		});

		// Maintaining insertion order with the help of LinkedList
		LinkedHashMap<TopVoterPlayer, Integer> sortedMap = new LinkedHashMap<>();
		for (Entry<TopVoterPlayer, Integer> entry : list) {
			sortedMap.put(entry.getKey(), entry.getValue());
		}

		return sortedMap;
	}

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
	 * Top voter weekly.
	 *
	 * @param page the page
	 * @return the string[]
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
	 * Top voter monthly
	 *
	 * @param page the page
	 * @return the string[]
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
	 * Top voters all time
	 *
	 * @return the string[]
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
	 * Top voters daily.
	 *
	 * @return the string[]
	 */

	public String[] topVotersDaily() {
		ArrayList<String> msg = new ArrayList<>();
		ArrayList<TopVoterPlayer> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Daily).keySet());
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
	 * Top voters.
	 *
	 * @return the string[]
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
	 * Top voter weekly.
	 *
	 * @param page the page
	 * @return the string[]
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
