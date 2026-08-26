package com.bencodez.votingplugin.topvoter;

import java.time.LocalDateTime;
import java.time.Month;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import com.bencodez.advancedcore.api.user.UserDataFetchMode;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.simpleapi.sql.Column;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** Loads top-voter rankings from user storage. */
public class TopVoterLoader {

	private final VotingPluginMain plugin;

	public TopVoterLoader(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getMonthlyTopVotersAtTime(LocalDateTime atTime) {
		LinkedHashMap<TopVoterPlayer, Integer> topVoters = new LinkedHashMap<>();
		CountDownLatch latch = new CountDownLatch(1);
		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (plugin != null && plugin.isEnabled() && uuid != null) {
				VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
				user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
				user.updateTempCacheWithColumns(columns);
				int total = user.getTotal(TopVoter.Monthly, atTime);
				if (total > 0) {
					topVoters.put(user.getTopVoterPlayer(), total);
				}
				user.clearTempCache();
			}
		}, count -> latch.countDown());

		try {
			latch.await(10, TimeUnit.MINUTES);
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
		}
		return TopVoterRanking.sortByValues(topVoters, false);
	}

	public LinkedHashMap<TopVoterPlayer, Integer> getTopVotersOfMonth(YearMonth month,
			HashMap<UUID, ArrayList<Column>> columnsByPlayer) {
		LinkedHashMap<TopVoterPlayer, Integer> totals = new LinkedHashMap<>();
		LocalDateTime atTime = month.atDay(15).atTime(0, 0);
		for (Entry<UUID, ArrayList<Column>> playerData : columnsByPlayer.entrySet()) {
			UUID uuid = playerData.getKey();
			if (uuid == null) {
				continue;
			}
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
			user.updateTempCacheWithColumns(playerData.getValue());
			int total = user.getTotal(TopVoter.Monthly, atTime);
			if (total > 0) {
				totals.put(user.getTopVoterPlayer(), total);
			}
			user.clearTempCache();
		}
		return TopVoterRanking.sortByValues(totals, false);
	}

	public void loadLastMonth() {
		if (!plugin.getGui().isLastMonthGUI()) {
			return;
		}
		plugin.getTopVoterState().getLastMonthTopVoters().clear();
		HashMap<TopVoterPlayer, Integer> totals = new HashMap<>();
		LocalDateTime lastMonthTime = plugin.getTimeChecker().getTime().minusMonths(1);
		boolean useDateTotalsPrimary = plugin.getConfigFile().isUseMonthDateTotalsAsPrimaryTotal();

		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (uuid == null || plugin == null || !plugin.isEnabled()) {
				return;
			}
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
			user.updateTempCacheWithColumns(columns);
			try {
				int total = useDateTotalsPrimary ? user.getTotal(TopVoter.Monthly, lastMonthTime)
						: user.getLastMonthTotal();
				if (total > 0) {
					totals.put(user.getTopVoterPlayer(), total);
				}
			} finally {
				user.clearTempCache();
			}
		}, count -> {
			plugin.getTopVoterState().getLastMonthTopVoters()
					.putAll(TopVoterRanking.sortByValues(new LinkedHashMap<>(totals), false));
			plugin.debug("Loaded last month top voters");
		});
	}

	public void loadPreviousMonthTopVoters() {
		if (!plugin.getConfigFile().isStoreMonthTotalsWithDate()) {
			return;
		}

		LocalDateTime now = plugin.getTimeChecker().getTime();
		YearMonth currentMonth = YearMonth.of(now.getYear(), now.getMonth());
		ArrayList<YearMonth> months = new ArrayList<>();
		ArrayList<String> monthColumns = new ArrayList<>();

		for (String column : plugin.getUserManager().getAllColumns()) {
			YearMonth month = parseMonthColumn(column);
			if (month != null && month.isBefore(currentMonth)) {
				months.add(month);
				monthColumns.add(column);
			}
		}
		if (months.isEmpty()) {
			return;
		}

		Integer[] order = new Integer[months.size()];
		for (int index = 0; index < order.length; index++) {
			order[index] = index;
		}
		java.util.Arrays.sort(order, (left, right) -> months.get(left).compareTo(months.get(right)));
		ArrayList<YearMonth> sortedMonths = new ArrayList<>(months.size());
		ArrayList<String> sortedColumns = new ArrayList<>(months.size());
		for (Integer index : order) {
			sortedMonths.add(months.get(index));
			sortedColumns.add(monthColumns.get(index));
		}
		months.clear();
		months.addAll(sortedMonths);
		monthColumns.clear();
		monthColumns.addAll(sortedColumns);

		LinkedHashMap<YearMonth, LinkedHashMap<TopVoterPlayer, Integer>> previous = plugin.getTopVoterState()
				.getPreviousMonthsTopVoters();
		previous.clear();
		for (YearMonth month : months) {
			plugin.debug("Loading previous month top voters of " + month);
			previous.put(month, new LinkedHashMap<>());
		}

		@SuppressWarnings("unchecked")
		LinkedHashMap<TopVoterPlayer, Integer>[] monthMaps = new LinkedHashMap[months.size()];
		for (int index = 0; index < months.size(); index++) {
			monthMaps[index] = previous.get(months.get(index));
		}
		String[] monthColumnNames = monthColumns.toArray(new String[0]);

		plugin.getUserManager().forEachUserKeys((uuid, columns) -> {
			if (uuid == null) {
				return;
			}
			VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(uuid, false);
			user.userDataFetechMode(UserDataFetchMode.TEMP_ONLY);
			user.updateTempCacheWithColumns(columns);
			try {
				TopVoterPlayer player = null;
				for (int index = 0; index < monthColumnNames.length; index++) {
					int total = user.getData().getInt(monthColumnNames[index], 0, UserDataFetchMode.TEMP_ONLY);
					if (total > 0) {
						if (player == null) {
							player = user.getTopVoterPlayer();
						}
						monthMaps[index].put(player, total);
					}
				}
			} finally {
				user.clearTempCache();
			}
		}, count -> {
			for (int index = 0; index < months.size(); index++) {
				YearMonth month = months.get(index);
				previous.put(month, TopVoterRanking.sortByValues(previous.get(month), false));
			}
			plugin.extraDebug("Previous Months: " + previous.keySet());
		});
	}

	static YearMonth parseMonthColumn(String column) {
		if (column == null || !column.startsWith("MonthTotal-")) {
			return null;
		}
		int firstDash = column.indexOf('-');
		int secondDash = column.indexOf('-', firstDash + 1);
		if (firstDash < 0 || secondDash < 0) {
			return null;
		}
		String month = column.substring(firstDash + 1, secondDash);
		String year = column.substring(secondDash + 1);
		if (!MessageAPI.isInt(year)) {
			return null;
		}
		try {
			return YearMonth.of(Integer.parseInt(year), Month.valueOf(month));
		} catch (RuntimeException ignored) {
			return null;
		}
	}
}
