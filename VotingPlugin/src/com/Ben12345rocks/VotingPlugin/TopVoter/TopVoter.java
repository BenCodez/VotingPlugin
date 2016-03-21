package com.Ben12345rocks.VotingPlugin.TopVoter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Set;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.UserData.Data;

public class TopVoter {
	
	private TopVoter() {
	}

	static TopVoter instance = new TopVoter();

	public static TopVoter getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;
	
	static ConfigFormat format = ConfigFormat.getInstance();

	public TopVoter(Main plugin) {
		TopVoter.plugin = plugin;
	}
	
	public String[] topVoters() {
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		Collections.sort(users, new Comparator<User>() {
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

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] topVoter(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> msg = new ArrayList<String>();
		Set<User> users1 = Data.getInstance().getUsers();
		ArrayList<User> users = Utils.getInstance().convertSet(users1);
		int pageSize = 1 + users.size() / pagesize;

		String title = format.getCommandVoteTopTitle()
				.replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize);
		msg.add(Utils.getInstance().colorize(title));

		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				plugin.topVoter);

		for (int i = (page - 1) * pagesize; i < topVoters.size()
				&& i < (page - 1) * pagesize + 10; i++) {
			msg.add(topVoters.get(i));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}


}
