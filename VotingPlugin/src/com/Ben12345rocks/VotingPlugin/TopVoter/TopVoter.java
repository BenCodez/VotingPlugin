package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Set;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
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
			TopVoters.getInstance().storeTopVoters(new Date().getYear() + 1900,
					new Date().getMonth() + 1, topVoterNoColor());
			if (!Config.getInstance().getTopVoterAwardsDisabled()) {
				Set<String> places = ConfigTopVoterAwards.getInstance()
						.getPossibleRewardPlaces();
				int i = 0;
				for (User user : topVotersSortedAll()) {
					i++;
					if (places.contains(Integer.toString(i))) {
						user.topVoterAward(i);
					}
				}
			}
			resetTopVoter();

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

	public void resetTopVoter() {
		for (User user : Data.getInstance().getUsers()) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				user.setTotal(voteSite, 0);
			}
		}
	}

	public String[] topVoter(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				plugin.topVoter);

		int pageSize = (topVoters.size() / pagesize);
		if (topVoters.size() % pagesize != 0) {
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
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);
			for (int i = users.size() - 1; i >= 0; i--) {
				if (users.get(i).getTotalVotes() == 0) {
					users.remove(i);
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
			for (int i = 0; i < users.size(); i++) {
				String line = "%num%: %player%, %votes%"
						.replace("%num%", "" + (i + 1))
						.replace("%player%", users.get(i).getPlayerName())
						.replace("%votes%", "" + users.get(i).getTotalVotes());
				msg.add(line);
			}
		}

		return Utils.getInstance().convertArray(msg);
	}

	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		if (users1 != null) {
			ArrayList<User> users = Utils.getInstance().convertSet(users1);
			for (int i = users.size() - 1; i >= 0; i--) {
				if (users.get(i).getTotalVotes() == 0) {
					users.remove(i);
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
			for (int i = 0; i < users.size(); i++) {
				String line = format.getCommandVoteTopLine()
						.replace("%num%", "" + (i + 1))
						.replace("%player%", users.get(i).getPlayerName())
						.replace("%votes%", "" + users.get(i).getTotalVotes());
				msg.add(line);
			}

		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public ArrayList<User> topVotersSortedAll() {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalVotes() == 0) {
				users.remove(i);
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

	public ArrayList<User> topVotersSortedVoteSite(VoteSite voteSite) {
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		for (int i = users.size() - 1; i >= 0; i--) {
			if (users.get(i).getTotalVotesSite(voteSite) == 0) {
				users.remove(i);
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
}
