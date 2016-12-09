package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.io.File;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.AdvancedCoreHook;
import com.Ben12345rocks.AdvancedCore.Listeners.DayChangeEvent;
import com.Ben12345rocks.AdvancedCore.Listeners.MonthChangeEvent;
import com.Ben12345rocks.AdvancedCore.Listeners.WeekChangeEvent;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Thread.Thread;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.YML.YMLFileHandler;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

public class TopVoterHandler implements Listener {
	/** The instance. */
	static TopVoterHandler instance = new TopVoterHandler();

	/** The plugin. */
	static Main plugin = Main.plugin;

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

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		Thread.getInstance().run(new Runnable() {

			@Override
			public void run() {
				for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
					User user = entry.getKey();
					int votes = entry.getValue();
					if (OtherVoteReward.getInstance().checkMinVotes(user, votes)) {
						if (user.isOnline()) {
							OtherVoteReward.getInstance().giveMinVotesReward(user, true);
						} else {
							user.setOfflineMinVote(user.getOfflineMinVotes() + 1);
						}
					}

				}
				if (Config.getInstance().getStoreTopVotersDaily()) {
					plugin.debug("Storing TopVoters Daily");
					storeDailyTopVoters();
				}

				if (Config.getInstance().getDailyAwardsEnabled()) {
					Set<String> places = Config.getInstance().getDailyPossibleRewardPlaces();
					int i = 0;
					for (User user : plugin.topVoterDaily.keySet()) {

						if (!user.hasTopVoterIgnorePermission()) {
							i++;
							if (places.contains(Integer.toString(i))) {
								user.dailyTopVoterAward(i);
							}
						}
					}
				}
				resetDailyTotals();

			}
		});

	}

	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		Thread.getInstance().run(new Runnable() {

			@Override
			public void run() {
				if (Config.getInstance().getStoreTopVotersWeekly()) {
					plugin.debug("Storing TopVoters Weekly");
					storeWeeklyTopVoters();
				}

				if (Config.getInstance().getWeeklyAwardsEnabled()) {
					Set<String> places = Config.getInstance().getWeeklyPossibleRewardPlaces();
					int i = 0;
					for (User user : plugin.topVoterWeekly.keySet()) {
						if (!user.hasTopVoterIgnorePermission()) {
							i++;
							if (places.contains(Integer.toString(i))) {
								user.weeklyTopVoterAward(i);
							}
						}
					}
				}
				resetWeeklyTotals();
			}
		});

	}

	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		Thread.getInstance().run(new Runnable() {

			@Override
			public void run() {
				if (Config.getInstance().getStoreTopVotersMonthly()) {
					plugin.debug("Storing TopVoters Monthly");
					storeMonthlyTopVoters();
				}

				if (Config.getInstance().getMonthlyAwardsEnabled()) {
					Set<String> places = Config.getInstance().getMonthlyPossibleRewardPlaces();
					int i = 0;
					for (User user : plugin.topVoterMonthly.keySet()) {
						if (!user.hasTopVoterIgnorePermission()) {
							i++;
							if (places.contains(Integer.toString(i))) {
								user.monthlyTopVoterAward(i);
							}
						}
					}
				}
				resetMonthlyTotals();
			}
		});

	}

	public void resetMonthlyTotals() {
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalVotes() != 0) {
				user.resetMonthlyTotalVotes();
			}
		}
	}

	public void resetWeeklyTotals() {
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalVotes() != 0) {
				user.resetWeeklyTotalVotes();
			}
		}
	}

	public void resetDailyTotals() {
		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalVotes() != 0) {
				user.resetDailyTotalVotes();
			}
		}
	}

	public void storeMonthlyTopVoters() {
		String month = LocalDateTime.now().getMonth().toString();
		int year = LocalDateTime.now().getYear();
		YMLFileHandler file = new YMLFileHandler(new File(plugin.getDataFolder(),
				"TopVoter" + File.separator + "Monthly" + File.separator + year + "_" + month + ".yml"));
		file.setup();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<User, Integer> entry : plugin.topVoterMonthly.entrySet()) {
			topVoters.add(count + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
			count++;
		}
		file.getData().set("All", topVoters);
		file.saveData();
	}

	public void storeWeeklyTopVoters() {
		String month = LocalDateTime.now().getMonth().toString();
		int year = LocalDateTime.now().getYear();
		int week = LocalDateTime.now().getDayOfMonth();
		YMLFileHandler file = new YMLFileHandler(new File(plugin.getDataFolder(),
				"TopVoter" + File.separator + "Weekly" + File.separator + year + "_" + month + "_" + week + ".yml"));
		file.setup();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<User, Integer> entry : plugin.topVoterWeekly.entrySet()) {
			topVoters.add(count + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
			count++;
		}
		file.getData().set("All", topVoters);
		file.saveData();
	}

	public void storeDailyTopVoters() {
		String month = LocalDateTime.now().getMonth().toString();
		int year = LocalDateTime.now().getYear();
		int day = LocalDateTime.now().getDayOfMonth();
		YMLFileHandler file = new YMLFileHandler(new File(plugin.getDataFolder(),
				"TopVoter" + File.separator + "Daily" + File.separator + year + "_" + month + "_" + day + ".yml"));
		file.setup();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
			topVoters.add(count + ": " + entry.getKey().getPlayerName() + ": " + entry.getValue());
			count++;
		}
		file.getData().set("All", topVoters);
		file.saveData();
	}

	public ArrayList<String> getTopVoterBlackList() {
		return Config.getInstance().getBlackList();
	}

	public ArrayList<User> getMonthlyTopVotersSorted() {
		ArrayList<User> users = new ArrayList<User>();
		ArrayList<String> blackList = getTopVoterBlackList();

		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalVotes() != 0 && blackList.contains(user.getPlayerName())) {
				users.add(user);
			}
		}

		Collections.sort(users, new Comparator<User>() {

			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalVotes();
				int p2Total = p2.getTotalVotes();

				return Integer.compare(p1Total, p2Total);
			}
		});

		return users;
	}

	public ArrayList<User> getWeeklyTopVotersSorted() {
		ArrayList<User> users = new ArrayList<User>();
		ArrayList<String> blackList = getTopVoterBlackList();

		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalWeeklyAll() != 0 && blackList.contains(user.getPlayerName())) {
				users.add(user);
			}
		}

		Collections.sort(users, new Comparator<User>() {

			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalWeeklyAll();
				int p2Total = p2.getTotalWeeklyAll();

				return Integer.compare(p1Total, p2Total);
			}
		});

		return users;
	}

	public ArrayList<User> getDailyTopVotersSorted() {
		ArrayList<User> users = new ArrayList<User>();
		ArrayList<String> blackList = getTopVoterBlackList();

		for (String uuid : UserManager.getInstance().getAllUUIDs()) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			if (user.getTotalDailyAll() != 0 && blackList.contains(user.getPlayerName())) {
				users.add(user);
			}
		}

		Collections.sort(users, new Comparator<User>() {

			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalDailyAll();
				int p2Total = p2.getTotalDailyAll();

				return Integer.compare(p1Total, p2Total);
			}
		});

		return users;
	}

	public synchronized void updateTopVoters() {
		plugin.topVoterMonthly.clear();
		ArrayList<User> monthlyTopVoters = getMonthlyTopVotersSorted();

		for (User user : monthlyTopVoters) {
			plugin.topVoterMonthly.put(user, user.getTotalVotes());
		}

		plugin.topVoterWeekly.clear();
		ArrayList<User> weeklyTopVoters = getWeeklyTopVotersSorted();

		for (User user : weeklyTopVoters) {
			plugin.topVoterWeekly.put(user, user.getTotalWeeklyAll());
		}

		plugin.topVoterDaily.clear();
		ArrayList<User> dailyTopVoters = getDailyTopVotersSorted();
		for (User user : dailyTopVoters) {
			plugin.topVoterDaily.put(user, user.getTotalDailyAll());
		}

		plugin.debug("Updated TopVoter");
	}

	/**
	 * Top voters.
	 *
	 * @return the string[]
	 */
	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		List<Entry<User, Integer>> list = new LinkedList<Entry<User, Integer>>(plugin.topVoterMonthly.entrySet());
		int i = 0;
		for (Entry<User, Integer> entry : list) {
			String line = "%num%: %player%, %votes%";
			line = line.replace("%num%", "" + (i + 1));
			try {
				line = line.replace("%player%", entry.getKey().getPlayerName());
			} catch (Exception ex) {
				AdvancedCoreHook.getInstance().debug(ex);
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
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(plugin.topVoterDaily.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = "%num%: %player%, %votes%";
			line = line.replace("%num%", "" + (i + 1));
			try {
				line = line.replace("%player%", users.get(i).getPlayerName());
			} catch (Exception ex) {
				AdvancedCoreHook.getInstance().debug(ex);
			}
			line = line.replace("%votes%", "" + plugin.topVoterMonthly.get(users.get(i)));
			msg.add(line);
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Top voters weekly.
	 *
	 * @return the string[]
	 */

	public String[] getCTopVotersWeekly() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(plugin.topVoterWeekly.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = Config.getInstance().getFormatCommandVoteTopLine().replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%", "" + plugin.topVoterWeekly.get(users.get(i)));
			msg.add(line);
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
		for (Entry<User, Integer> entry : plugin.topVoterWeekly.entrySet()) {
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
		msg.add(StringUtils.getInstance().colorize(title));

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
	public String[] topVoterMonthly(int page) {
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = new ArrayList<String>();
		int count = 1;
		for (Entry<User, Integer> entry : plugin.topVoterMonthly.entrySet()) {
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
		msg.add(StringUtils.getInstance().colorize(title));

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
		for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
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
		msg.add(StringUtils.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}
}
