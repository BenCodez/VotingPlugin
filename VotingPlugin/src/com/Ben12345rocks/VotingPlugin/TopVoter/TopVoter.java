package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Listeners.DayChangeEvent;
import com.Ben12345rocks.AdvancedCore.Listeners.MonthChangeEvent;
import com.Ben12345rocks.AdvancedCore.Listeners.WeekChangeEvent;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.OtherRewards.OtherVoteReward;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class TopVoter.
 */
public class TopVoter implements Listener {

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

	/** The instance. */
	static TopVoter instance = new TopVoter();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of TopVoter.
	 *
	 * @return single instance of TopVoter
	 */
	public static TopVoter getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new top voter.
	 */
	private TopVoter() {
	}

	/**
	 * Instantiates a new top voter.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public TopVoter(Main plugin) {
		TopVoter.plugin = plugin;
	}

	public void register() {
		plugin.getServer().getPluginManager().registerEvents(this, plugin);
	}

	@SuppressWarnings("deprecation")
	@EventHandler
	public void onDayChange(DayChangeEvent event) {
		for (Entry<User, Integer> entry : plugin.topVoterDaily.entrySet()) {
			User user = entry.getKey();
			int votes = entry.getValue();
			if (OtherVoteReward.getInstance().checkMinVotes(user, votes)) {
				if (user.isOnline()) {
					OtherVoteReward.getInstance()
							.giveMinVotesReward(user, true);
				} else {
					user.setOfflineMinVote(user.getOfflineMinVotes() + 1);
				}
			}

		}
		TopVoters.getInstance().storeDailyTopVoters(
				new Date().getYear() + 1900, new Date().getMonth(),
				new Date().getDate(), topVoterDailyNoColor());
		if (ConfigTopVoterAwards.getInstance().getDailyAwardsEnabled()) {
			Set<String> places = ConfigTopVoterAwards.getInstance()
					.getDailyPossibleRewardPlaces();
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
		resetTotalsDaily();
	}

	@SuppressWarnings("deprecation")
	@EventHandler
	public void onWeekChange(WeekChangeEvent event) {
		TopVoters.getInstance().storeWeeklyTopVoters(
				new Date().getYear() + 1900, new Date().getMonth(),
				new Date().getDate(), topVoterWeeklyNoColor());
		if (ConfigTopVoterAwards.getInstance().getWeeklyAwardsEnabled()) {
			Set<String> places = ConfigTopVoterAwards.getInstance()
					.getWeeklyPossibleRewardPlaces();
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
		resetTotalsWeekly();
	}

	@SuppressWarnings("deprecation")
	@EventHandler
	public void onMonthChange(MonthChangeEvent event) {
		TopVoters.getInstance().storeMonthlyTopVoters(
				new Date().getYear() + 1900, new Date().getMonth(),
				topVoterNoColor());
		if (ConfigTopVoterAwards.getInstance().getMonthlyAwardsEnabled()) {
			Set<String> places = ConfigTopVoterAwards.getInstance()
					.getMonthlyPossibleRewardPlaces();
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
		resetTotalsMonthly();
	}

	/**
	 * Reset totals daily.
	 */
	public void resetTotalsDaily() {
		for (User user : UserManager.getInstance().getVotingPluginUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotalDaily(voteSite, 0);
			}
		}
	}

	/**
	 * Reset totals monthly.
	 */
	public void resetTotalsMonthly() {
		for (User user : UserManager.getInstance().getVotingPluginUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotal(voteSite, 0);
			}
		}
	}

	/**
	 * Reset totals player.
	 *
	 * @param user
	 *            the user
	 */
	public void resetTotalsPlayer(User user) {
		for (VoteSite voteSite : ConfigVoteSites.getInstance().getVoteSites()) {
			user.setTotal(voteSite, 0);
		}
	}

	/**
	 * Reset totals weekly.
	 */
	public void resetTotalsWeekly() {
		for (User user : UserManager.getInstance().getVotingPluginUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotalWeekly(voteSite, 0);
			}
		}
	}

	/**
	 * Sort by values.
	 *
	 * @param unsortMap
	 *            the unsort map
	 * @param order
	 *            the order
	 * @return the hash map
	 */
	public HashMap<User, Integer> sortByValues(
			HashMap<User, Integer> unsortMap, final boolean order) {

		List<Entry<User, Integer>> list = new LinkedList<Entry<User, Integer>>(
				unsortMap.entrySet());

		// Sorting the list based on values
		Collections.sort(list, new Comparator<Entry<User, Integer>>() {
			@Override
			public int compare(Entry<User, Integer> o1, Entry<User, Integer> o2) {
				if (order) {
					return o1.getValue().compareTo(o2.getValue());
				} else {
					return o2.getValue().compareTo(o1.getValue());

				}
			}
		});

		// Maintaining insertion order with the help of LinkedList
		HashMap<User, Integer> sortedMap = new LinkedHashMap<User, Integer>();
		for (Entry<User, Integer> entry : list) {
			sortedMap.put(entry.getKey(), entry.getValue());
		}

		return sortedMap;
	}

	/**
	 * Top voter daily.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterDaily(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				topVotersDaily());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = format.getCommandVoteTopTitle()
				.replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize).replace("%Top%", "Daily");
		msg.add(Utils.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voter daily no color.
	 *
	 * @return the string[]
	 */
	public String[] topVoterDailyNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterDaily.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = "%num%: %player%, %votes%"
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterDaily.get(users.get(i)));
			msg.add(line);
		}

		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voter monthly.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterMonthly(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				topVoters());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = format.getCommandVoteTopTitle()
				.replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize)
				.replace("%Top%", "Monthly");
		msg.add(Utils.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voter no color.
	 *
	 * @return the string[]
	 */
	public String[] topVoterNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterMonthly.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = "%num%: %player%, %votes%"
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterMonthly.get(users.get(i)));
			msg.add(line);
		}

		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voters.
	 *
	 * @return the string[]
	 */
	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		List<Entry<User, Integer>> list = new LinkedList<Entry<User, Integer>>(
				plugin.topVoterMonthly.entrySet());
		int i = 0;
		for (Entry<User, Integer> entry : list) {
			String line = format.getCommandVoteTopLine()
					.replace("%num%", "" + (i + 1))
					.replace("%player%", entry.getKey().getPlayerName())
					.replace("%votes%", "" + entry.getValue().intValue());

			msg.add(line);
			i++;
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voters daily.
	 *
	 * @return the string[]
	 */

	public String[] topVotersDaily() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterDaily.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = format
					.getCommandVoteTopLine()
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterDaily.get(users.get(i)));
			msg.add(line);
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voters sorted all.
	 *
	 * @return the array list
	 */

	public ArrayList<User> topVotersSortedAll() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();

		ArrayList<User> users = new ArrayList<User>(UserManager.getInstance()
				.getVotingPluginUsers());

		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalVotes() == 0) {
				users.remove(i);
			}
		}
		if (blackList != null) {
			for (int i = users.size() - 1; i >= 0; i--) {
				if (blackList.contains(users.get(i).getPlayerName())) {
					users.remove(i);
				}
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

	/**
	 * Top voters sorted all daily.
	 *
	 * @return the array list
	 */

	public ArrayList<User> topVotersSortedAllDaily() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();

		ArrayList<User> users = new ArrayList<User>(UserManager.getInstance()
				.getVotingPluginUsers());

		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalDailyAll() == 0) {
				users.remove(i);
			}
		}
		if (blackList != null) {
			for (int i = users.size() - 1; i >= 0; i--) {
				if (blackList.contains(users.get(i).getPlayerName())) {
					users.remove(i);
				}
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

	/**
	 * Top voters sorted all weekly.
	 *
	 * @return the array list
	 */

	public ArrayList<User> topVotersSortedAllWeekly() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();

		ArrayList<User> users = new ArrayList<User>(UserManager.getInstance()
				.getVotingPluginUsers());

		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalWeeklyAll() == 0) {
				users.remove(i);
			}
		}
		if (blackList != null) {
			for (int i = users.size() - 1; i >= 0; i--) {
				if (blackList.contains(users.get(i).getPlayerName())) {
					users.remove(i);
				}
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

	/**
	 * Top voters sorted vote site.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the hash map
	 */

	public HashMap<User, Integer> topVotersSortedVoteSite(VoteSite voteSite) {
		ArrayList<User> users = UserManager.getInstance()
				.getVotingPluginUsers();
		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalVotesSite(voteSite) == 0) {
				users.remove(i);
			}
		}
		for (int i = users.size() - 1; i >= 0; i--) {
			for (int j = users.size() - 1; j >= 0; j--) {
				if (i != j) {
					if (users.get(i).getPlayerName() == users.get(j)
							.getPlayerName()) {
						users.remove(j);
					}
				}
			}
		}
		HashMap<User, Integer> topVoter = new HashMap<User, Integer>();
		for (User user : users) {
			topVoter.put(user, user.getTotal(voteSite));
		}

		return sortByValues(topVoter, false);
	}

	/**
	 * Top voters sorted vote site daily.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the hash map
	 */

	public HashMap<User, Integer> topVotersSortedVoteSiteDaily(VoteSite voteSite) {
		ArrayList<User> users = UserManager.getInstance()
				.getVotingPluginUsers();
		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalDaily(voteSite) == 0) {
				users.remove(i);
			}
		}
		for (int i = users.size() - 1; i >= 0; i--) {
			for (int j = users.size() - 1; j >= 0; j--) {
				if (i != j) {
					if (users.get(i).getPlayerName() == users.get(j)
							.getPlayerName()) {
						users.remove(j);
					}
				}
			}
		}
		HashMap<User, Integer> topVoter = new HashMap<User, Integer>();
		for (User user : users) {
			topVoter.put(user, user.getTotalDaily(voteSite));
		}

		return sortByValues(topVoter, false);
	}

	/**
	 * Top voters sorted vote site weekly.
	 *
	 * @param voteSite
	 *            the vote site
	 * @return the hash map
	 */

	public HashMap<User, Integer> topVotersSortedVoteSiteWeekly(
			VoteSite voteSite) {
		ArrayList<User> users = UserManager.getInstance()
				.getVotingPluginUsers();
		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalWeekly(voteSite) == 0) {
				users.remove(i);
			}
		}
		for (int i = users.size() - 1; i >= 0; i--) {
			for (int j = users.size() - 1; j >= 0; j--) {
				if (i != j) {
					if (users.get(i).getPlayerName() == users.get(j)
							.getPlayerName()) {
						users.remove(j);
					}
				}
			}
		}

		HashMap<User, Integer> topVoter = new HashMap<User, Integer>();
		for (User user : users) {
			topVoter.put(user, user.getTotalWeekly(voteSite));
		}

		return sortByValues(topVoter, false);
	}

	/**
	 * Top voters weekly.
	 *
	 * @return the string[]
	 */

	public String[] topVotersWeekly() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterWeekly.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = format
					.getCommandVoteTopLine()
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterWeekly.get(users.get(i)));
			msg.add(line);
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voter weekly.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] topVoterWeekly(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				topVotersWeekly());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = format.getCommandVoteTopTitle()
				.replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize)
				.replace("%Top%", "Weekly");
		msg.add(Utils.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Top voter weekly no color.
	 *
	 * @return the string[]
	 */

	public String[] topVoterWeeklyNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterWeekly.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = "%num%: %player%, %votes%"
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterWeekly.get(users.get(i)));
			msg.add(line);
		}

		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Update top voters.
	 */

	public synchronized void updateTopVoters() {
		plugin.topVoterMonthly.clear();
		ArrayList<User> users = topVotersSortedAll();
		if (users != null) {
			for (User user : users) {
				plugin.topVoterMonthly.put(user, user.getTotalVotes());
			}
			plugin.topVoterMonthly = sortByValues(plugin.topVoterMonthly, false);
		}

		plugin.topVoterWeekly.clear();
		users = topVotersSortedAllWeekly();
		if (users != null) {
			for (User user : users) {
				plugin.topVoterWeekly.put(user, user.getTotalWeeklyAll());
			}
			plugin.topVoterWeekly = sortByValues(plugin.topVoterWeekly, false);
		}

		plugin.topVoterDaily.clear();
		users = topVotersSortedAllDaily();
		if (users != null) {
			for (User user : users) {
				plugin.topVoterDaily.put(user, user.getTotalDailyAll());
			}
			plugin.topVoterDaily = sortByValues(plugin.topVoterDaily, false);
		}

		plugin.debug("Updated TopVoter");

	}

}
