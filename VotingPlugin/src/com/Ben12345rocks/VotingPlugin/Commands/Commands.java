package com.Ben12345rocks.VotingPlugin.Commands;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.apache.commons.lang3.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permission;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.Util.Scoreboards.SimpleScoreboard;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import net.md_5.bungee.api.chat.TextComponent;

// TODO: Auto-generated Javadoc
/**
 * The Class Commands.
 */
public class Commands {

	/** The bonus reward. */
	static ConfigOtherRewards bonusReward = ConfigOtherRewards.getInstance();

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	/** The format. */
	static ConfigFormat format = ConfigFormat.getInstance();

	/** The instance. */
	static Commands instance = new Commands();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Commands.
	 *
	 * @return single instance of Commands
	 */
	public static Commands getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new commands.
	 */
	private Commands() {
	}

	/**
	 * Instantiates a new commands.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public Commands(Main plugin) {
		Commands.plugin = plugin;
	}

	/**
	 * Admin help.
	 *
	 * @param sender
	 *            the sender
	 * @param page
	 *            the page
	 * @return the array list
	 */
	public ArrayList<TextComponent> adminHelp(CommandSender sender, int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<TextComponent> msg = new ArrayList<TextComponent>();
		ArrayList<TextComponent> text = adminHelpText(sender);

		int maxPage = text.size() / pagesize;
		if ((text.size() % pagesize) != 0) {
			maxPage++;
		}

		msg.add(StringUtils.getInstance().stringToComp("&3&lVotingPlugin Admin Help " + (page + 1) + "/" + maxPage));
		msg.add(StringUtils.getInstance().stringToComp("&3&l() = Needed"));
		msg.add(StringUtils.getInstance().stringToComp("&3&lAliases: adminvote, av"));

		for (int i = pagesize * page; (i < text.size()) && (i < ((page + 1) * pagesize)); i++) {
			msg.add(text.get(i));
		}

		return msg;
	}

	/**
	 * Admin help text.
	 *
	 * @param sender
	 *            the sender
	 * @return the array list
	 */
	public ArrayList<TextComponent> adminHelpText(CommandSender sender) {
		ArrayList<TextComponent> msg = new ArrayList<TextComponent>();
		HashMap<String, TextComponent> unsorted = new HashMap<String, TextComponent>();

		boolean requirePerms = ConfigFormat.getInstance().getCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.adminVoteCommand) {
			if (sender.hasPermission(cmdHandle.getPerm()) && requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/av"), cmdHandle.getHelpLine("/av"));
			} else {
				unsorted.put(cmdHandle.getHelpLineCommand("/av"), cmdHandle.getHelpLine("/av"));
			}
		}
		ArrayList<String> unsortedList = new ArrayList<String>();
		unsortedList.addAll(unsorted.keySet());
		Collections.sort(unsortedList, String.CASE_INSENSITIVE_ORDER);
		for (String cmd : unsortedList) {
			msg.add(unsorted.get(cmd));
		}

		return msg;
	}

	/**
	 * Command vote today.
	 *
	 * @param page
	 *            the page
	 * @return the string[]
	 */
	public String[] commandVoteToday(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		if (page < 1) {
			page = 1;
		}
		ArrayList<String> msg = new ArrayList<String>();
		String[] voteToday = voteToday();

		int maxPage = voteToday.length / pagesize;
		if ((voteToday.length % pagesize) != 0) {
			maxPage++;
		}

		msg.add("&cToday's Votes " + page + "/" + maxPage);
		msg.add("&cPlayerName : VoteSite : Time");
		page--;

		for (int i = pagesize * page; (i < voteToday.length) && (i < ((page + 1) * pagesize)); i++) {
			msg.add(voteToday[i]);
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * List perms.
	 *
	 * @return the string[]
	 */
	public String[] listPerms() {
		ArrayList<String> msg = new ArrayList<String>();

		for (Permission perm : plugin.getDescription().getPermissions()) {
			msg.add(perm.getName());
		}

		for (CommandHandler handle : plugin.voteCommand) {
			if (!msg.contains(handle.getPerm())) {
				msg.add(handle.getPerm());
			}
		}

		for (CommandHandler handle : plugin.adminVoteCommand) {
			if (!msg.contains(handle.getPerm())) {
				msg.add(handle.getPerm());
			}
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		Collections.sort(msg, String.CASE_INSENSITIVE_ORDER);

		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Player info.
	 *
	 * @param user
	 *            the user
	 * @return the string[]
	 */
	public String[] playerInfo(User user) {
		ArrayList<String> msg = new ArrayList<String>();

		// title
		msg.add("&cPlayer '" + user.getPlayerName() + "' Info");

		// last vote
		msg.addAll(ArrayUtils.getInstance().convert(voteCommandLast(user)));

		// next vote
		msg.addAll(ArrayUtils.getInstance().convert(voteCommandNext(user)));

		// total
		msg.addAll(ArrayUtils.getInstance().convert(voteCommandTotal(user)));

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Send top voter daily score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterDailyScoreBoard(Player player, int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> topVoters = ArrayUtils.getInstance().convert(TopVoter.getInstance().topVotersDaily());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize).replace("%Top%", "Daily"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(plugin.topVoterDaily.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(), plugin.topVoterDaily.get(users.get(i)));
		}
		scoreboard.build();
		scoreboard.send(player);

		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				SimpleScoreboard clear = new SimpleScoreboard("Empty");
				clear.send(player);
			}
		}, 90);

	}

	/**
	 * Send top voter monthly score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterMonthlyScoreBoard(Player player, int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> topVoters = ArrayUtils.getInstance().convert(TopVoter.getInstance().topVoters());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize).replace("%Top%", "Monthly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(plugin.topVoterMonthly.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
					plugin.topVoterMonthly.get(users.get(i)));
		}
		scoreboard.build();
		scoreboard.send(player);

		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				SimpleScoreboard clear = new SimpleScoreboard("Empty");
				clear.send(player);
			}
		}, 90);

	}

	/**
	 * Send top voter weekly score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterWeeklyScoreBoard(Player player, int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		ArrayList<String> topVoters = ArrayUtils.getInstance().convert(TopVoter.getInstance().topVotersWeekly());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize).replace("%Top%", "Weekly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(plugin.topVoterWeekly.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
			scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(), plugin.topVoterWeekly.get(users.get(i)));
		}
		scoreboard.build();
		scoreboard.send(player);

		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				SimpleScoreboard clear = new SimpleScoreboard("Empty");
				clear.send(player);
			}
		}, 90);

	}

	/**
	 * Send vote today score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendVoteTodayScoreBoard(Player player, int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();

		String[] voteToday = voteToday();

		int maxPage = voteToday.length / pagesize;
		if ((voteToday.length % pagesize) != 0) {
			maxPage++;
		}

		SimpleScoreboard scoreboard = new SimpleScoreboard("&cToday's Votes " + page + "/" + maxPage);

		for (int i = pagesize * page; (i < voteToday.length) && (i < ((page + 1) * pagesize)); i++) {
			scoreboard.add(voteToday[i], i);
		}
		scoreboard.build();
		scoreboard.send(player);

		Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

			@Override
			public void run() {
				SimpleScoreboard clear = new SimpleScoreboard("Empty");
				clear.send(player);
			}
		}, 90);

	}

	/**
	 * Update vote today.
	 */
	@SuppressWarnings({ "deprecation" })
	public void updateVoteToday() {
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils.getInstance()
				.convertSet(Data.getInstance().getUsers());
		plugin.voteToday.clear();

		if (users != null) {
			for (User user : users) {
				HashMap<VoteSite, Date> times = new HashMap<VoteSite, Date>();
				for (VoteSite voteSite : configVoteSites.getVoteSites()) {
					long time = user.getTime(voteSite);
					if ((new Date().getDate() == Utils.getInstance().getDayFromMili(time))
							&& (new Date().getMonth() == Utils.getInstance().getMonthFromMili(time))
							&& (new Date().getYear() == Utils.getInstance().getYearFromMili(time))) {

						times.put(voteSite, new Date(time));

					}
				}
				if (times.keySet().size() > 0) {
					plugin.voteToday.put(user, times);
				}
			}
		}
		plugin.debug("Updated VoteToday");
	}

	public String voteCommandLastLine(User user, VoteSite voteSite) {
		String timeString = voteCommandLastDate(user, voteSite);

		return format.getCommandsVoteLastLine().replace("%Month% %Day%, %Year% %Hour%:%Minute% %ampm%", "%time%")
				.replace("%time%", timeString).replace("%SiteName%", voteSite.getSiteName());
	}

	/**
	 * Vote command last.
	 *
	 * @param user
	 *            the user
	 * @return the string[]
	 */
	public String[] voteCommandLast(User user) {

		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().replaceIgnoreCase(format.getCommandsVoteLastTitle(), "%player%", playerName));

		for (VoteSite voteSite : voteSites) {
			msg.add(voteCommandLastLine(user, voteSite));
		}

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Vote command last date.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the string
	 */
	public String voteCommandLastDate(User user, VoteSite voteSite) {
		Date date = new Date(user.getTime(voteSite));
		String timeString = new SimpleDateFormat(format.getTimeFormat()).format(date);
		return timeString;
	}

	/**
	 * Vote command next.
	 *
	 * @param user
	 *            the user
	 * @return the string[]
	 */
	public String[] voteCommandNext(User user) {
		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().colorize(
				StringUtils.getInstance().replaceIgnoreCase(format.getCommandsVoteNextTitle(), "%player%", playerName)));

		for (VoteSite voteSite : voteSites) {

			String msgLine = format.getCommandsVoteNextLayout();

			msgLine = StringUtils.getInstance().replaceIgnoreCase(msgLine, "%info%", voteCommandNextInfo(user, voteSite));

			msgLine = StringUtils.getInstance().replaceIgnoreCase(msgLine, "%SiteName%", voteSite.getSiteName());
			msg.add(StringUtils.getInstance().colorize(msgLine));

		}
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Vote command next info.
	 *
	 * @param user
	 *            the user
	 * @param voteSite
	 *            the vote site
	 * @return the string
	 */
	@SuppressWarnings("deprecation")
	public String voteCommandNextInfo(User user, VoteSite voteSite) {
		String info = new String();

		Date date = new Date(user.getTime(voteSite));

		int month = date.getMonth();
		int day = date.getDate();
		int hour = date.getHours();
		int min = date.getMinutes();
		int year = date.getYear();

		int votedelay = configVoteSites.getVoteDelay(voteSite.getSiteName());
		if (votedelay == 0) {
			String errorMsg = format.getCommandsVoteNextInfoError();
			info = errorMsg;
		} else {

			Date voteTime = new Date(year, month, day, hour, min);
			Date nextvote = DateUtils.addHours(voteTime, votedelay);

			int cday = new Date().getDate();
			int cmonth = new Date().getMonth();
			int chour = new Date().getHours();
			int cmin = new Date().getMinutes();
			int cyear = new Date().getYear();
			Date currentDate = new Date(cyear, cmonth, cday, chour, cmin);

			if ((nextvote == null) || (day == 0) || (hour == 0)) {
				String canVoteMsg = format.getCommandsVoteNextInfoCanVote();
				info = canVoteMsg;
			} else {
				if (!currentDate.after(nextvote)) {
					long diff = nextvote.getTime() - currentDate.getTime();

					// long diffSeconds = (diff / 1000) % 60;
					long diffMinutes = (diff / (60 * 1000)) % 60;
					long diffHours = diff / (60 * 60 * 1000);
					// long diffDays = diff / (24 * 60 * 60 * 1000);

					String timeMsg = format.getCommandsVoteNextInfoTime();
					timeMsg = StringUtils.getInstance().replaceIgnoreCase(timeMsg, "%hours%", Long.toString(diffHours));
					timeMsg = StringUtils.getInstance().replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					String canVoteMsg = format.getCommandsVoteNextInfoCanVote();
					info = canVoteMsg;
				}
			}
		}
		return info;
	}

	public String voteCommandTotalLine(User user, VoteSite voteSite) {
		String line = format.getCommandsVoteTotalLine();
		return StringUtils.getInstance().replaceIgnoreCase(
				StringUtils.getInstance().replaceIgnoreCase(line, "%Total%", "" + user.getTotal(voteSite)), "%SiteName%",
				voteSite.getSiteName());
	}

	/**
	 * Vote command total.
	 *
	 * @param user
	 *            the user
	 * @return the string[]
	 */
	public String[] voteCommandTotal(User user) {
		ArrayList<String> msg = new ArrayList<String>();
		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().replaceIgnoreCase(format.getCommandsVoteTotalTitle(), "%player%", playerName));

		// total votes
		int total = 0;

		for (VoteSite voteSite : voteSites) {
			int votes = user.getTotal(voteSite);
			// int votes = Data.getInstance().getTotal(playerName, siteName);
			total += votes;
			msg.add(voteCommandTotalLine(user, voteSite));
		}
		msg.add(StringUtils.getInstance().replaceIgnoreCase(format.getCommandsVoteTotalTotal(), "%Totals%", "" + total));

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Vote command total all.
	 *
	 * @return the string[]
	 */
	public String[] voteCommandTotalAll() {

		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = plugin.voteSites;

		ArrayList<String> voteNames = com.Ben12345rocks.AdvancedCore.Data.Data.getInstance().getPlayerNames();

		msg.add(format.getCommandsVoteTotalAllTitle());
		int total = 0;
		for (VoteSite voteSite : voteSites) {
			int votes = 0;
			for (String playerName : voteNames) {
				if (playerName != null) {
					User user = UserManager.getInstance().getVotingPluginUser(playerName);
					votes += user.getTotal(voteSite);
				}
			}
			msg.add(StringUtils.getInstance().replaceIgnoreCase(StringUtils.getInstance()
					.replaceIgnoreCase(format.getCommandsVoteTotalAllLine(), "%SiteName%", voteSite.getSiteName()),
					"%Total%", "" + votes));
			total += votes;
		}
		msg.add(StringUtils.getInstance().replaceIgnoreCase(format.getCommandsVoteTotalAllTotal(), "%Totals%", "" + total));

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Vote help text.
	 *
	 * @param sender
	 *            the sender
	 * @return the array list
	 */
	public ArrayList<TextComponent> voteHelpText(CommandSender sender) {
		ArrayList<TextComponent> texts = new ArrayList<TextComponent>();
		HashMap<String, TextComponent> unsorted = new HashMap<String, TextComponent>();
		texts.add(StringUtils.getInstance().stringToComp(ConfigFormat.getInstance().getCommandsVoteHelpTitle()));

		boolean requirePerms = ConfigFormat.getInstance().getCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.voteCommand) {
			if (sender.hasPermission(cmdHandle.getPerm()) && requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/v"), cmdHandle.getHelpLine("/v"));
			} else {
				unsorted.put(cmdHandle.getHelpLineCommand("/v"), cmdHandle.getHelpLine("/v"));
			}
		}

		ArrayList<String> unsortedList = new ArrayList<String>();
		unsortedList.addAll(unsorted.keySet());
		Collections.sort(unsortedList, String.CASE_INSENSITIVE_ORDER);
		for (String cmd : unsortedList) {
			texts.add(unsorted.get(cmd));
		}
		return texts;
	}

	/**
	 * Vote today.
	 *
	 * @return the string[]
	 */
	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<String>();
		for (User user : plugin.voteToday.keySet()) {

			for (VoteSite voteSite : plugin.voteToday.get(user).keySet()) {
				String timeString = new SimpleDateFormat(format.getTimeFormat())
						.format(plugin.voteToday.get(user).get(voteSite));
				msg.add("&6" + user.getPlayerName() + " : " + voteSite.getSiteName() + " : " + timeString);
			}
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	/**
	 * Vote UR ls.
	 *
	 * @return the string[]
	 */
	public String[] voteURLs() {
		ArrayList<String> sites = new ArrayList<String>();
		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		List<String> title = ConfigFormat.getInstance().getCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (ConfigFormat.getInstance().getCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : voteSites) {
				counter++;
				String voteURL = configVoteSites.getVoteURL(voteSite.getSiteName());
				String msg = format.getCommandsVoteURLS();
				msg = StringUtils.getInstance().colorize(msg);
				msg = StringUtils.getInstance().replaceIgnoreCase(msg, "%num%", Integer.toString(counter));
				msg = StringUtils.getInstance().replaceIgnoreCase(msg, "%url%", voteURL);
				msg = StringUtils.getInstance().replaceIgnoreCase(msg, "%SiteName%", voteSite.getSiteName());
				sites.add(msg);
			}
		}
		sites = ArrayUtils.getInstance().colorize(sites);
		return ArrayUtils.getInstance().convert(sites);
	}
}
