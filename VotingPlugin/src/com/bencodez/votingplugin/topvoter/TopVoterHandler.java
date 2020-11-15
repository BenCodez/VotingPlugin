package com.bencodez.votingplugin.topvoter;

import java.io.File;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.advancedcore.api.time.events.DayChangeEvent;
import com.bencodez.advancedcore.api.time.events.MonthChangeEvent;
import com.bencodez.advancedcore.api.time.events.PreDateChangedEvent;
import com.bencodez.advancedcore.api.time.events.WeekChangeEvent;
import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.advancedcore.yml.YMLFileHandler;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.config.GUI;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.specialrewards.SpecialRewards;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.user.UserManager;

public class TopVoterHandler implements Listener {
	/** The instance. */
	static TopVoterHandler instance = new TopVoterHandler();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

	/**
	 * Gets the single instance of TopVoter.
	 *
	 * @return single instance of TopVoter
	 */
	public static TopVoterHandler getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new top voter.
	 */
	private TopVoterHandler() {
	}

	public ArrayList<String> getTopVoterBlackList() {
		return Config.getInstance().getBlackList();
	}

	/**
	 * Top voters weekly.
	 *
	 * @return the string[]
	 */

	public String[] getTopVotersWeekly() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<VotingPluginUser> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Weekly).keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = Config.getInstance().getFormatCommandVoteTopLine().replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%", "" + plugin.getTopVoter(TopVoter.Weekly).get(users.get(i)));
			msg.add(line);
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	private HashMap<Integer, String> handlePlaces(Set<String> places) {
		HashMap<Integer, String> place = new HashMap<Integer, String>();
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
		if (GUI.getInstance().isLastMonthGUI()) {
			plugin.getLastMonthTopVoter().clear();

			LinkedHashMap<VotingPluginUser, Integer> totals = new LinkedHashMap<VotingPluginUser, Integer>();
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				int total = user.getLastMonthTotal();
				if (total > 0) {
					totals.put(user, total);
				}
			}

			plugin.getLastMonthTopVoter().putAll(sortByValues(totals, false));

			plugin.debug("Loaded last month top voters");
		}

	}

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onDateChanged(DateChangedEvent event) {
		plugin.setUpdate(true);
		plugin.update();
		loadLastMonth();
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChange(DayChangeEvent event) {
		synchronized (VotingPluginMain.plugin) {
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				if (!user.voteStreakUpdatedToday(LocalDateTime.now().minusDays(1))) {
					if (user.getDayVoteStreak() != 0) {
						user.setDayVoteStreak(0);
					}
				}

				if (user.getHighestDailyTotal() < user.getTotal(TopVoter.Daily)) {
					user.setHighestDailyTotal(user.getTotal(TopVoter.Daily));
				}
				for (String shopIdent : GUI.getInstance().getChestShopIdentifiers()) {
					if (GUI.getInstance().getChestVoteShopResetDaily(shopIdent)) {
						user.setVoteShopIdentifierLimit(shopIdent, 0);
					}
				}
			}

			if (Config.getInstance().getStoreTopVotersDaily()) {
				plugin.getLogger().info("Saving TopVoters Daily");
				storeTopVoters(TopVoter.Daily);
			}

			try {
				if (SpecialRewardsConfig.getInstance().isEnableDailyRewards()) {
					HashMap<Integer, String> places = handlePlaces(
							SpecialRewardsConfig.getInstance().getDailyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;
					@SuppressWarnings("unchecked")
					LinkedHashMap<VotingPluginUser, Integer> clone = (LinkedHashMap<VotingPluginUser, Integer>) plugin
							.getTopVoter(TopVoter.Daily).clone();
					for (VotingPluginUser user : clone.keySet()) {
						if (!Config.getInstance().getTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
							if (Config.getInstance().getTopVoterAwardsTies()) {
								if (user.getTotal(TopVoter.Daily) != lastTotal) {
									i++;
								}
							} else {
								i++;
							}
							if (places.containsKey(i)) {
								user.giveDailyTopVoterAward(i, places.get(i));
							}
						}
						lastTotal = user.getTotal(TopVoter.Daily);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			resetTotals(TopVoter.Daily);
		}
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onMonthChange(MonthChangeEvent event) {
		synchronized (VotingPluginMain.plugin) {
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				if (user.getTotal(TopVoter.Monthly) == 0 && user.getMonthVoteStreak() != 0) {
					user.setMonthVoteStreak(0);
				} else {
					if (!SpecialRewardsConfig.getInstance().isVoteStreakRequirementUsePercentage()
							|| user.hasPercentageTotal(TopVoter.Monthly,
									SpecialRewardsConfig.getInstance().getVoteStreakRequirementMonth(),
									LocalDateTime.now().minusDays(1))) {
						user.addMonthVoteStreak();
						SpecialRewards.getInstance().checkVoteStreak(user, "Month");
					}
				}

				for (String shopIdent :GUI.getInstance().getChestShopIdentifiers()) {
					if (GUI.getInstance().getChestVoteShopResetMonthly(shopIdent)) {
						user.setVoteShopIdentifierLimit(shopIdent, 0);
					}
				}

				user.setLastMonthTotal(user.getTotal(TopVoter.Monthly));

				if (user.getHighestMonthlyTotal() < user.getTotal(TopVoter.Monthly)) {
					user.setHighestMonthlyTotal(user.getTotal(TopVoter.Monthly));
				}
			}

			if (Config.getInstance().getStoreTopVotersMonthly()) {
				plugin.getLogger().info("Saving TopVoters Monthly");
				storeTopVoters(TopVoter.Monthly);
			}

			try {
				if (SpecialRewardsConfig.getInstance().isEnableMonthlyAwards()) {
					HashMap<Integer, String> places = handlePlaces(
							SpecialRewardsConfig.getInstance().getMonthlyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;

					@SuppressWarnings("unchecked")
					LinkedHashMap<VotingPluginUser, Integer> clone = (LinkedHashMap<VotingPluginUser, Integer>) plugin
							.getTopVoter(TopVoter.Monthly).clone();
					for (VotingPluginUser user : clone.keySet()) {

						if (!Config.getInstance().getTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
							if (Config.getInstance().getTopVoterAwardsTies()) {
								if (user.getTotal(TopVoter.Monthly) != lastTotal) {
									i++;
								}
							} else {
								i++;
							}
							if (places.containsKey(i)) {
								user.giveMonthlyTopVoterAward(i, places.get(i));
							}
						}
						lastTotal = user.getTotal(TopVoter.Monthly);
					}

				}
			} catch (Exception e) {
				e.printStackTrace();
			}

			resetTotals(TopVoter.Monthly);

			if (SpecialRewardsConfig.getInstance().getResetMilestonesMonthly()) {
				for (String uuid : UserManager.getInstance().getAllUUIDs()) {
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
					user.setMilestoneCount(0);
					user.setHasGottenMilestone(new HashMap<String, Boolean>());
				}

			}
		}
	}

	@EventHandler(priority = EventPriority.NORMAL, ignoreCancelled = true)
	public void onPreDateChanged(PreDateChangedEvent event) {
		plugin.setUpdate(true);
		plugin.update();
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onWeekChange(WeekChangeEvent event) {
		synchronized (VotingPluginMain.plugin) {
			for (String uuid : UserManager.getInstance().getAllUUIDs()) {
				VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
				if (user.getTotal(TopVoter.Weekly) == 0 && user.getWeekVoteStreak() != 0) {
					user.setWeekVoteStreak(0);
				} else {
					if (!SpecialRewardsConfig.getInstance().isVoteStreakRequirementUsePercentage()
							|| user.hasPercentageTotal(TopVoter.Weekly,
									SpecialRewardsConfig.getInstance().getVoteStreakRequirementWeek(), null)) {
						user.addWeekVoteStreak();
						SpecialRewards.getInstance().checkVoteStreak(user, "Week");
					}
				}

				for (String shopIdent : GUI.getInstance().getChestShopIdentifiers()) {
					if (GUI.getInstance().getChestVoteShopResetWeekly(shopIdent)) {
						user.setVoteShopIdentifierLimit(shopIdent, 0);
					}
				}

				if (user.getHighestWeeklyTotal() < user.getTotal(TopVoter.Weekly)) {
					user.setHighestWeeklyTotal(user.getTotal(TopVoter.Weekly));
				}
			}

			if (Config.getInstance().getStoreTopVotersWeekly()) {
				plugin.getLogger().info("Saving TopVoters Weekly");
				storeTopVoters(TopVoter.Weekly);
			}

			try {
				if (SpecialRewardsConfig.getInstance().isEnableWeeklyAwards()) {
					HashMap<Integer, String> places = handlePlaces(
							SpecialRewardsConfig.getInstance().getWeeklyPossibleRewardPlaces());
					int i = 0;
					int lastTotal = -1;
					@SuppressWarnings("unchecked")
					LinkedHashMap<VotingPluginUser, Integer> clone = (LinkedHashMap<VotingPluginUser, Integer>) plugin
							.getTopVoter(TopVoter.Weekly).clone();
					for (VotingPluginUser user : clone.keySet()) {
						if (!Config.getInstance().getTopVoterIgnorePermission() || !user.isTopVoterIgnore()) {
							if (Config.getInstance().getTopVoterAwardsTies()) {
								if (user.getTotal(TopVoter.Weekly) != lastTotal) {
									i++;
								}
							} else {
								i++;
							}
							if (places.containsKey(i)) {
								user.giveWeeklyTopVoterAward(i, places.get(i));
							}
						}
						lastTotal = user.getTotal(TopVoter.Weekly);
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
			resetTotals(TopVoter.Weekly);
		}
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	public void resetTotals(TopVoter topVoter) {
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotal(topVoter) != 0) {
				user.resetTotals(topVoter);
			}
		}
	}

	public LinkedHashMap<VotingPluginUser, Integer> sortByValues(LinkedHashMap<VotingPluginUser, Integer> topVoterAllTime,
			final boolean order) {

		List<Entry<VotingPluginUser, Integer>> list = new LinkedList<Entry<VotingPluginUser, Integer>>(topVoterAllTime.entrySet());

		// Sorting the list based on values
		Collections.sort(list, new Comparator<Entry<VotingPluginUser, Integer>>() {
			@Override
			public int compare(Entry<VotingPluginUser, Integer> o1, Entry<VotingPluginUser, Integer> o2) {
				if (order) {
					return o1.getValue().compareTo(o2.getValue());
				} else {
					return o2.getValue().compareTo(o1.getValue());

				}
			}
		});

		// Maintaining insertion order with the help of LinkedList
		LinkedHashMap<VotingPluginUser, Integer> sortedMap = new LinkedHashMap<VotingPluginUser, Integer>();
		for (Entry<VotingPluginUser, Integer> entry : list) {
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
		fileName += ".yml";

		YMLFileHandler file = new YMLFileHandler(new File(plugin.getDataFolder(), fileName));
		file.setup();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<VotingPluginUser, Integer> entry : plugin.getTopVoter(top).entrySet()) {
			topVoters.add(count + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
			count++;
		}
		file.getData().set(top.toString(), topVoters);
		file.saveData();
	}

	/**
	 * Top voter all time
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterAllTime(int page) {
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<VotingPluginUser, Integer> entry : plugin.getTopVoter(TopVoter.AllTime).entrySet()) {
			String line = Config.getInstance().getFormatCommandVoteTopLine();
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

		String title = Config.getInstance().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", "All Time");
		msg.add(StringParser.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voter weekly.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterDaily(int page) {
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<VotingPluginUser, Integer> entry : plugin.getTopVoter(TopVoter.Daily).entrySet()) {
			String line = Config.getInstance().getFormatCommandVoteTopLine();
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

		String title = Config.getInstance().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", "Daily");
		msg.add(StringParser.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voter monthly
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterMonthly(int page) {
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<VotingPluginUser, Integer> entry : plugin.getTopVoter(TopVoter.Monthly).entrySet()) {
			String line = Config.getInstance().getFormatCommandVoteTopLine();
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

		String title = Config.getInstance().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", "Monthly");
		msg.add(StringParser.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voters all time
	 *
	 * @return the string[]
	 */
	public String[] topVotersAllTime() {
		ArrayList<String> msg = new ArrayList<String>();
		List<Entry<VotingPluginUser, Integer>> list = new LinkedList<Entry<VotingPluginUser, Integer>>(
				plugin.getTopVoter(TopVoter.AllTime).entrySet());
		int i = 0;
		for (Entry<VotingPluginUser, Integer> entry : list) {
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

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voters daily.
	 *
	 * @return the string[]
	 */

	public String[] topVotersDaily() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<VotingPluginUser> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Daily).keySet());
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

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voters.
	 *
	 * @return the string[]
	 */
	public String[] topVotersMonthly() {
		ArrayList<String> msg = new ArrayList<String>();
		List<Entry<VotingPluginUser, Integer>> list = new LinkedList<Entry<VotingPluginUser, Integer>>(
				plugin.getTopVoter(TopVoter.Monthly).entrySet());
		int i = 0;
		for (Entry<VotingPluginUser, Integer> entry : list) {
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

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voter weekly.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterWeekly(int page) {
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<VotingPluginUser, Integer> entry : plugin.getTopVoter(TopVoter.Weekly).entrySet()) {
			String line = Config.getInstance().getFormatCommandVoteTopLine();
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

		String title = Config.getInstance().getFormatCommandVoteTopTitle();
		title = title.replace("%page%", "" + page);
		title = title.replace("%maxpages%", "" + pageSize);
		title = title.replace("%Top%", "Weekly");
		msg.add(StringParser.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	public synchronized void updateTopVoters(ArrayList<VotingPluginUser> users1) {
		ArrayList<VotingPluginUser> users = new ArrayList<VotingPluginUser>();
		ArrayList<String> blackList = getTopVoterBlackList();
		for (VotingPluginUser user : users1) {
			if (!blackList.contains(user.getPlayerName())) {
				if ((!Config.getInstance().getTopVoterIgnorePermission() || !user.isTopVoterIgnore())
						&& !user.isBanned()) {
					users.add(user);
				}
			}
		}

		plugin.debug("Number of users to check top voter: " + users.size());

		for (TopVoter top : TopVoter.values()) {
			if (Config.getInstance().getLoadTopVoter(top)) {
				LinkedHashMap<VotingPluginUser, Integer> map = new LinkedHashMap<VotingPluginUser, Integer>();

				for (VotingPluginUser user : users) {
					int total = user.getTotal(top);
					if (total > 0) {
						map.put(user, total);
					}
				}

				map = sortByValues(map, false);
				int limitSize = Config.getInstance().getMaxiumNumberOfTopVotersToLoad();
				if (limitSize > 0) {
					ArrayList<VotingPluginUser> listKeys = new ArrayList<VotingPluginUser>(map.keySet());
					if (listKeys.size() > limitSize) {
						for (int i = listKeys.size() - 1; i >= 0 && i >= limitSize; i--) {
							map.remove(listKeys.get(i));
						}
					}

				}
				plugin.getTopVoter(top).clear();
				plugin.getTopVoter().put(top, map);
				plugin.debug(top.toString() + " TopVoter loaded");
			}
		}

		plugin.debug("Updated TopVoter");
	}
}
