package com.Ben12345rocks.VotingPlugin.Commands;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import org.apache.commons.lang3.time.DateUtils;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserData.Data;

public class Commands {

	private Commands() {
	}

	static Commands instance = new Commands();

	public static Commands getInstance() {
		return instance;
	}

	static Main plugin = Main.plugin;

	public Commands(Main plugin) {
		Commands.plugin = plugin;
	}

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	public String[] voteHelp() {
		ArrayList<String> help = (ArrayList<String>) format.getVoteHelp();
		return Utils.getInstance().convertArray(
				Utils.getInstance().colorize(help));
	}

	public String[] adminVoteHelp() {
		ArrayList<String> help = new ArrayList<String>();
		help.add("&3&lAdmin Commands:");
		help.add("&3 () = needed");
		help.add("&3 [] = optional");
		help.add("&3Aliases: "
				+ Utils.getInstance().makeStringList(
						(ArrayList<String>) plugin.getCommand("adminvote")
								.getAliases()));
		help.add("&b&l/adminvote vote (player) (sitename) - Manually trigger a vote");
		help.add("&b&l/adminvote bungeevote (player) (sitename) - Manually send a bungee vote");
		help.add("&b&l/adminvote settotal (player) (sitename) (amount) - Set players total votes");
		// help.add("&b&l/adminvote debug - Toggle debug (Deletes Comments)");
		help.add("&b&l/adminvote reload - Reload Configs");
		help.add("&b&l/adminvote uuid (playername) - Gives you the players uuid");
		help.add("&b&l/adminvote version - Display's Plugin Info");
		help.add("&b&l/adminvote sites [site] - Display vote sites and vote site info");
		// help.add("&b&l/adminvote convert - convert old data file to new data files (won't lag)");
		help.add("&b&l/adminvote help - See this page");

		return Utils.getInstance().convertArray(
				Utils.getInstance().colorize(help));
	}

	public String[] voteCommandTotal(User user) {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(format.getCommandsVoteTotalTitle().replace("%player%",
				playerName));

		// total votes
		int total = 0;

		for (VoteSite voteSite : voteSites) {
			int votes = user.getTotal(voteSite);
			// int votes = Data.getInstance().getTotal(playerName, siteName);
			total += votes;
			msg.add(format.getCommandsVoteTotalLine()
					.replace("%SiteName%", voteSite.getSiteName())
					.replace("%Total%", "" + votes));
		}
		msg.add(format.getCommandsVoteTotalTotal().replace("%Totals%",
				"" + total));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] voteCommandTotalAll() {

		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		ArrayList<String> voteNames = Data.getInstance().getPlayerNames();

		msg.add(format.getCommandsVoteTotalAllTitle());
		int total = 0;
		for (VoteSite voteSite : voteSites) {
			int votes = 0;
			for (String playerName : voteNames) {
				if (playerName != null) {
					User user = new User(playerName);
					votes += user.getTotal(voteSite);
				}
			}
			msg.add(format.getCommandsVoteTotalAllLine()
					.replace("%SiteName%", voteSite.getSiteName())
					.replace("%Total%", "" + votes));
			total += votes;
		}
		msg.add(format.getCommandsVoteTotalAllTotal().replace("%Totals%",
				"" + total));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] voteCommandLast(User user) {

		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(format.getCommandsVoteLastTitle().replace("%player%",
				playerName));

		for (VoteSite voteSite : voteSites) {
			Date date = new Date(user.getTime(voteSite));
			String timeString = new SimpleDateFormat(format.getTimeFormat())
					.format(date);

			msg.add(format
					.getCommandsVoteLastLine()
					.replace("%Month% %Day%, %Year% %Hour%:%Minute% %ampm%",
							"%time%").replace("%time%", timeString)
					.replace("%SiteName%", voteSite.getSiteName()));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	@SuppressWarnings({ "deprecation", "unused" })
	public String[] voteCommandNext(User user) {
		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(Utils.getInstance().colorize(
				format.getCommandsVoteNextTitle().replace("%player%",
						playerName)));

		for (VoteSite voteSite : voteSites) {

			String msgLine = format.getCommandsVoteNextLayout();

			Date date = new Date(user.getTime(voteSite));

			int month = date.getMonth();
			int day = date.getDate();
			int hour = date.getHours();
			int min = date.getMinutes();
			int year = date.getYear();

			int votedelay = configVoteSites
					.getVoteDelay(voteSite.getSiteName());
			if (votedelay == 0) {
				String errorMsg = format.getCommandsVoteNextInfoError();
				msgLine = msgLine.replace("%info%", errorMsg);
			} else {

				Date voteTime = new Date(year, month, day, hour, min);
				Date nextvote = DateUtils.addHours(voteTime, votedelay);

				int cday = new Date().getDate();
				int cmonth = new Date().getMonth();
				int chour = new Date().getHours();
				int cmin = new Date().getMinutes();
				int cyear = new Date().getYear();
				Date currentDate = new Date(cyear, cmonth, cday, chour, cmin);

				if (nextvote == null || day == 0 || hour == 0) {
					String canVoteMsg = format.getCommandsVoteNextInfoCanVote();
					msgLine = msgLine.replace("%info%", canVoteMsg);
				} else {
					if (!currentDate.after(nextvote)) {
						long diff = nextvote.getTime() - currentDate.getTime();

						long diffSeconds = diff / 1000 % 60;
						long diffMinutes = diff / (60 * 1000) % 60;
						long diffHours = diff / (60 * 60 * 1000);
						// long diffDays = diff / (24 * 60 * 60 * 1000);

						String timeMsg = format.getCommandsVoteNextInfoTime();
						timeMsg = timeMsg.replace("%hours%",
								Long.toString(diffHours));
						timeMsg = timeMsg.replace("%minutes%",
								Long.toString(diffMinutes));
						msgLine = msgLine.replace("%info%", timeMsg);
					} else {
						String canVoteMsg = format
								.getCommandsVoteNextInfoCanVote();
						msgLine = msgLine.replace("%info%", canVoteMsg);
					}
				}
			}
			msgLine = msgLine.replace("%SiteName%", voteSite.getSiteName());
			msg.add(Utils.getInstance().colorize(msgLine));

		}
		return Utils.getInstance().convertArray(msg);
	}

	public ArrayList<String> voteURLs() {
		ArrayList<String> sites = new ArrayList<String>();
		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();
		int counter = 0;
		for (VoteSite voteSite : voteSites) {
			counter++;
			String voteURL = configVoteSites.getVoteURL(voteSite.getSiteName());
			String msg = format.getCommandsVoteURLS();
			msg = Utils.getInstance().colorize(msg);
			msg = msg.replace("%num%", Integer.toString(counter));
			msg = msg.replace("%url%", voteURL);
			msg = msg.replace("%SiteName%", voteSite.getSiteName());
			sites.add(msg);
		}
		return sites;
	}

	public String[] playerInfo(User user) {
		ArrayList<String> msg = new ArrayList<String>();

		// title
		msg.add("&cPlayer '" + user.getPlayerName() + "' Info");

		// last vote
		msg.addAll(Utils.getInstance().convertArray(voteCommandLast(user)));

		// next vote
		msg.addAll(Utils.getInstance().convertArray(voteCommandNext(user)));

		// total
		msg.addAll(Utils.getInstance().convertArray(voteCommandTotal(user)));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] voteCommandSites() {
		ArrayList<String> msg = new ArrayList<String>();

		msg.add("&c&lVote Sites:");

		int count = 1;
		for (VoteSite voteSite : ConfigVoteSites.getInstance().getVoteSites()) {
			msg.add("&c" + count + ". &6" + voteSite.getSiteName());
			count++;
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] voteCommandSiteInfo(String voteSiteName) {
		ArrayList<String> msg = new ArrayList<String>();

		if (!ConfigVoteSites.getInstance().getVoteSites()
				.contains(voteSiteName)) {
			msg.add("&cInvalid Vote Site, see /av sites!");
		} else {

			VoteSite voteSite = new VoteSite(voteSiteName);

			msg.add("&c&lVote Site Info for " + voteSiteName + ":");

			msg.add("&cSite: &6" + voteSite.getVoteSiteServiceSite());
			msg.add("&cVoteURL: &6" + voteSite.getVoteURL());
			msg.add("&cVote Delay: &6" + voteSite.getVoteDelay());
			msg.add("&cMoney: &6" + voteSite.getMoneyAmount());

			msg.add("&cItems:");
			for (String item : ConfigVoteSites.getInstance().getItems(
					voteSite.getSiteName())) {
				msg.add("&c- &6" + item);
			}

			msg.add("&cPlayer Commands:");

			try {
				for (String playerCommands : voteSite.getPlayerCommands()) {
					msg.add("&c- " + playerCommands);
				}
			} catch (Exception ex) {
			}

			msg.add("&cConsole Commands:");

			try {
				for (String consoleCommands : voteSite.getConsoleCommands()) {
					msg.add("&c- " + consoleCommands);
				}
			} catch (Exception ex) {
			}
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] commandVoteToday(int page) {
		if (page < 1) {
			page = 1;
		}
		ArrayList<String> msg = new ArrayList<String>();

		msg.add("&cToday's Votes " + page + "/"
				+ ((plugin.voteToday.length / 10) + 1));
		msg.add("&cPlayerName : VoteSite : Time");
		page--;

		for (int i = 10 * page; i < plugin.voteToday.length
				&& i < ((page + 1) * 10); i++) {
			msg.add(plugin.voteToday[i]);
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);

	}

	@SuppressWarnings("deprecation")
	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<User> users = Utils.getInstance().convertSet(
				Data.getInstance().getUsers());
		for (User user : users) {
			for (VoteSite voteSite : ConfigVoteSites.getInstance()
					.getVoteSites()) {
				long time = user.getTime(voteSite);
				if (new Date().getDate() == Utils.getInstance().getDayFromMili(
						time)) {

					String timeString = new SimpleDateFormat(
							format.getTimeFormat()).format(new Date(time));
					msg.add("&6" + user.getPlayerName() + " : "
							+ voteSite.getSiteName() + " : " + timeString);
				}
			}
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

}
