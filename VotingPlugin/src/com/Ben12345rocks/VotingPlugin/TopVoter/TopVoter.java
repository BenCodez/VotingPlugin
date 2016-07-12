package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.OfflinePlayer;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigTopVoterAwards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Data.ServerData;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class TopVoter {

	static ConfigFormat format = ConfigFormat.getInstance();

	static TopVoter instance = new TopVoter();

	static Main plugin = Main.plugin;

	public static TopVoter getInstance() {
		return instance;
	}

	private TopVoter() {
	}

	public TopVoter(Main plugin) {
		TopVoter.plugin = plugin;
	}

	@SuppressWarnings("deprecation")
	public void checkTopVoterAward() {
		if (hasMonthChanged()) {
			plugin.getLogger().info("Month changed!");
			TopVoters.getInstance().storeMonthlyTopVoters(
					new Date().getYear() + 1900, new Date().getMonth(),
					topVoterNoColor());
			if (ConfigTopVoterAwards.getInstance().getMonthlyAwardsEnabled()) {
				Set<String> places = ConfigTopVoterAwards.getInstance()
						.getMonthlyPossibleRewardPlaces();
				int i = 0;
				for (User user : topVotersSortedAll()) {
					OfflinePlayer player = Bukkit.getOfflinePlayer(user
							.getPlayerName());
					if (!player.getPlayer().hasPermission(
							"VotingPlugin.TopVoter.Ignore")) {
						i++;
						if (places.contains(Integer.toString(i))) {
							user.monthlyTopVoterAward(i);
						}
					}
				}
			}
			resetTotals();

		}

		if (hasWeekChanged()) {
			plugin.getLogger().info("Week changed!");
			TopVoters.getInstance().storeWeeklyTopVoters(
					new Date().getYear() + 1900, new Date().getMonth(),
					new Date().getDay(), topVoterWeeklyNoColor());
			if (ConfigTopVoterAwards.getInstance().getWeeklyAwardsEnabled()) {
				Set<String> places = ConfigTopVoterAwards.getInstance()
						.getWeeklyPossibleRewardPlaces();
				int i = 0;
				for (User user : topVotersSortedAllWeekly()) {
					OfflinePlayer player = Bukkit.getOfflinePlayer(user
							.getPlayerName());
					if (!player.getPlayer().hasPermission(
							"VotingPlugin.TopVoter.Ignore")) {
						i++;
						if (places.contains(Integer.toString(i))) {
							user.weeklyTopVoterAward(i);
						}
					}
				}
			}
			resetTotalsWeekly();

		}

		if (hasDayChanged()) {
			plugin.getLogger().info("Week changed!");
			TopVoters.getInstance().storeDailyTopVoters(
					new Date().getYear() + 1900, new Date().getMonth(),
					new Date().getDate(), topVoterDailyNoColor());
			if (ConfigTopVoterAwards.getInstance().getDailyAwardsEnabled()) {
				Set<String> places = ConfigTopVoterAwards.getInstance()
						.getDailyPossibleRewardPlaces();
				int i = 0;
				for (User user : topVotersSortedAllDaily()) {
					OfflinePlayer player = Bukkit.getOfflinePlayer(user
							.getPlayerName());
					if (!player.getPlayer().hasPermission(
							"VotingPlugin.TopVoter.Ignore")) {
						i++;
						if (places.contains(Integer.toString(i))) {
							user.dailyTopVoterAward(i);
						}
					}
				}
			}
			resetTotalsDaily();

		}

	}

	@SuppressWarnings("deprecation")
	public boolean hasMonthChanged() {
		int prevMonth = ServerData.getInstance().getPrevMonth();
		java.util.TimeZone tz = java.util.TimeZone.getTimeZone("UTC");
		java.util.Calendar c = java.util.Calendar.getInstance(tz);
		int month = c.getTime().getMonth();
		ServerData.getInstance().setPrevMonth(month);
		if (prevMonth != month) {
			return true;
		}
		return false;
	}

	@SuppressWarnings("deprecation")
	public boolean hasWeekChanged() {
		int prevDate = ServerData.getInstance().getPrevWeekDay();
		java.util.TimeZone tz = java.util.TimeZone.getTimeZone("UTC");
		java.util.Calendar c = java.util.Calendar.getInstance(tz);
		ServerData.getInstance().setPrevWeekDay(c.getTime().getDate());
		if (prevDate == 0 && c.getTime().getDate() != prevDate) {
			return true;
		}
		return false;
	}

	@SuppressWarnings("deprecation")
	public boolean hasDayChanged() {
		int prevDay = ServerData.getInstance().getPrevDay();
		java.util.TimeZone tz = java.util.TimeZone.getTimeZone("UTC");
		java.util.Calendar c = java.util.Calendar.getInstance(tz);
		int day = c.getTime().getDay();
		ServerData.getInstance().setPrevDay(day);
		if (prevDay != day) {
			return true;
		}
		return false;
	}

	public void resetTotals() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotal(voteSite, 0);
			}
		}
	}

	public void resetTotalsWeekly() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotalWeekly(voteSite, 0);
			}
		}
	}

	public void resetTotalsDaily() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotalDaily(voteSite, 0);
			}
		}
	}

	public void resetTotalsPlayer(User user) {
		for (VoteSite voteSite : ConfigVoteSites.getInstance().getVoteSites()) {
			user.setTotal(voteSite, 0);
		}
	}

	public String[] topVoter(int page) {
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
				.replace("%maxpages%", "" + pageSize);
		msg.add(Utils.getInstance().colorize(title));

		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] topVoterNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = Utils.getInstance().convertSet(
				plugin.topVoterMonthly.keySet());
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

	public String[] topVoterWeeklyNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = Utils.getInstance().convertSet(
				plugin.topVoterWeekly.keySet());
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

	public String[] topVoterDailyNoColor() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = Utils.getInstance().convertSet(
				plugin.topVoterDaily.keySet());
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

	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<User> users = Utils.getInstance().convertSet(
				plugin.topVoterMonthly.keySet());
		for (int i = 0; i < users.size(); i++) {
			String line = format
					.getCommandVoteTopLine()
					.replace("%num%", "" + (i + 1))
					.replace("%player%", users.get(i).getPlayerName())
					.replace("%votes%",
							"" + plugin.topVoterMonthly.get(users.get(i)));
			msg.add(line);
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public ArrayList<User> topVotersSortedAll() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);

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

					if (p1Total < p2Total) {
						return 1;
					}
					if (p1Total > p2Total) {
						return -1;
					}

					return 0;
				}
			});

			return users;
		}
		return null;
	}

	public ArrayList<User> topVotersSortedAllWeekly() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);

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

					if (p1Total < p2Total) {
						return 1;
					}
					if (p1Total > p2Total) {
						return -1;
					}

					return 0;
				}
			});

			return users;
		}
		return null;
	}

	public ArrayList<User> topVotersSortedAllDaily() {
		ArrayList<String> blackList = (ArrayList<String>) ConfigTopVoterAwards
				.getInstance().getBlackList();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);

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

					if (p1Total < p2Total) {
						return 1;
					}
					if (p1Total > p2Total) {
						return -1;
					}

					return 0;
				}
			});

			return users;
		}
		return null;
	}

	public ArrayList<User> topVotersSortedVoteSite(VoteSite voteSite) {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
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
		Collections.sort(users, new Comparator<User>() {
			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalVotesSite(voteSite);
				int p2Total = p2.getTotalVotesSite(voteSite);

				if (p1Total < p2Total) {
					return 1;
				}
				if (p1Total > p2Total) {
					return -1;
				}

				return 0;
			}
		});
		return users;
	}

	public ArrayList<User> topVotersSortedVoteSiteWeekly(VoteSite voteSite) {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
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
		Collections.sort(users, new Comparator<User>() {
			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalWeekly(voteSite);
				int p2Total = p2.getTotalWeekly(voteSite);

				if (p1Total < p2Total) {
					return 1;
				}
				if (p1Total > p2Total) {
					return -1;
				}

				return 0;
			}
		});
		return users;
	}

	public ArrayList<User> topVotersSortedVoteSiteDaily(VoteSite voteSite) {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
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
		Collections.sort(users, new Comparator<User>() {
			@Override
			public int compare(User p1, User p2) {
				int p1Total = p1.getTotalDaily(voteSite);
				int p2Total = p2.getTotalDaily(voteSite);

				if (p1Total < p2Total) {
					return 1;
				}
				if (p1Total > p2Total) {
					return -1;
				}

				return 0;
			}
		});
		return users;
	}

	public void updateTopVoters() {
		plugin.topVoterMonthly.clear();
		ArrayList<User> users = topVotersSortedAll();
		if (users != null) {
			for (User user : users) {
				plugin.topVoterMonthly.put(user, user.getTotalVotes());
			}
		}

		plugin.topVoterWeekly.clear();
		ArrayList<User> users1 = topVotersSortedAllWeekly();
		if (users != null) {
			for (User user : users1) {
				plugin.topVoterWeekly.put(user, user.getTotalVotes());
			}
		}

		plugin.topVoterDaily.clear();
		ArrayList<User> users2 = topVotersSortedAllDaily();
		if (users != null) {
			for (User user : users2) {
				plugin.topVoterDaily.put(user, user.getTotalVotes());
			}
		}

		plugin.debug("Updated TopVoter");

	}

}
