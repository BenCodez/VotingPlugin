package com.Ben12345rocks.VotingPlugin.Commands;

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.permissions.Permission;

import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Objects.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.StringUtils;
import com.Ben12345rocks.AdvancedCore.Util.Scoreboards.SimpleScoreboard;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import net.md_5.bungee.api.chat.TextComponent;

// TODO: Auto-generated Javadoc
/**
 * The Class Commands.
 */
public class Commands {

	/** The config. */
	static Config config = Config.getInstance();

	/** The config vote sites. */
	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

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
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<TextComponent> msg = new ArrayList<TextComponent>();
		ArrayList<TextComponent> text = adminHelpText(sender);

		int maxPage = text.size() / pagesize;
		if ((text.size() % pagesize) != 0) {
			maxPage++;
		}

		msg.add(StringUtils.getInstance().stringToComp("&3&lVotingPlugin Admin Help " + (page + 1) + "/" + maxPage));
		msg.add(StringUtils.getInstance().stringToComp("&3&l() = Needed"));
		msg.add(StringUtils.getInstance().stringToComp("&3&lAliases: adminvote, av"));

		for (int i = pagesize * page; (i < text.size()) && (i < ((page + 1) * pagesize) - 1); i++) {
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

		boolean requirePerms = Config.getInstance().getFormatCommandsVoteHelpRequirePermission();

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
		int pagesize = Config.getInstance().getFormatPageSize();
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

		for (CommandHandler handle : plugin.voteCommand) {
			msg.add(handle.getHelpLineCommand("/v") + " : " + handle.getPerm());
		}

		for (CommandHandler handle : plugin.adminVoteCommand) {
			msg.add(handle.getHelpLineCommand("/av") + " : " + handle.getPerm());
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			msg.add(perm.getName());
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
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<User> users = plugin.convertSet(plugin.topVoterDaily.keySet());

		int pageSize = (users.size() / pagesize);
		if ((users.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(config.getFormatCommandVoteTopTitle()
				.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Daily"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
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
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<User> users = plugin.convertSet(plugin.topVoterMonthly.keySet());

		int pageSize = (users.size() / pagesize);
		if ((users.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(config.getFormatCommandVoteTopTitle()
				.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Monthly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
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
		int pagesize = Config.getInstance().getFormatPageSize();
		ArrayList<User> users = plugin.convertSet(plugin.topVoterWeekly.keySet());

		int pageSize = (users.size() / pagesize);
		if ((users.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = StringUtils.getInstance().colorize(config.getFormatCommandVoteTopTitle()
				.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Weekly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
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
		int pagesize = Config.getInstance().getFormatPageSize();

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
	public void updateVoteToday() {
		ArrayList<String> uuids = UserManager.getInstance().getAllUUIDs();

		plugin.voteToday.clear();

		for (String uuid : uuids) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			HashMap<VoteSite, LocalDateTime> times = new HashMap<VoteSite, LocalDateTime>();
			for (VoteSite voteSite : plugin.getVoteSites()) {
				long time = user.getTime(voteSite);
				if ((LocalDateTime.now().getDayOfMonth() == MiscUtils.getInstance().getDayFromMili(time))
						&& (LocalDateTime.now().getMonthValue() == MiscUtils.getInstance().getMonthFromMili(time))
						&& (LocalDateTime.now().getYear() == MiscUtils.getInstance().getYearFromMili(time))) {

					times.put(voteSite, LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault()));

				}
			}
			if (times.keySet().size() > 0) {
				plugin.voteToday.put(user, times);
			}

		}
		plugin.debug("Updated VoteToday");
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

		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteLastTitle(), "%player%",
				playerName));

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
		String timeString = new SimpleDateFormat(config.getFormatTimeFormat()).format(date);
		return timeString;
	}

	public String voteCommandLastLine(User user, VoteSite voteSite) {
		String timeString = voteCommandLastDate(user, voteSite);

		return config.getFormatCommandsVoteLastLine().replace("%Month% %Day%, %Year% %Hour%:%Minute% %ampm%", "%time%")
				.replace("%time%", timeString).replace("%SiteName%", voteSite.getSiteName());
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

		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().colorize(StringUtils.getInstance()
				.replaceIgnoreCase(config.getFormatCommandsVoteNextTitle(), "%player%", playerName)));

		for (VoteSite voteSite : voteSites) {

			String msgLine = config.getFormatCommandsVoteNextLayout();

			msgLine = StringUtils.getInstance().replaceIgnoreCase(msgLine, "%info%",
					voteCommandNextInfo(user, voteSite));

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
	public String voteCommandNextInfo(User user, VoteSite voteSite) {
		String info = new String();

		long time = user.getTime(voteSite);
		LocalDateTime date = LocalDateTime.now();

		int votedelay = configVoteSites.getVoteDelay(voteSite.getSiteName());
		if (votedelay == 0) {
			String errorMsg = config.getFormatCommandsVoteNextInfoError();
			info = errorMsg;
		} else {

			LocalDateTime nextvote = date.plusHours(votedelay);

			if (time == 0 || date.isAfter(nextvote)) {
				String canVoteMsg = config.getFormatCommandsVoteNextInfoCanVote();
				info = canVoteMsg;
			} else {
				Duration dur = Duration.between(date, nextvote);

				long diffHours = dur.getSeconds() / (60 * 60);
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

				String timeMsg = config.getFormatCommandsVoteNextInfoTime();
				timeMsg = StringUtils.getInstance().replaceIgnoreCase(timeMsg, "%hours%", Long.toString(diffHours));
				timeMsg = StringUtils.getInstance().replaceIgnoreCase(timeMsg, "%minutes%", Long.toString(diffMinutes));
				info = timeMsg;

			}
		}
		return info;
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
		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		String playerName = user.getPlayerName();

		msg.add(StringUtils.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteTotalTitle(), "%player%",
				playerName));

		// total votes
		int total = 0;

		for (VoteSite voteSite : voteSites) {
			int votes = user.getTotal(voteSite);
			// int votes = Data.getInstance().getTotal(playerName, siteName);
			total += votes;
			msg.add(voteCommandTotalLine(user, voteSite));
		}
		msg.add(StringUtils.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteTotalTotal(), "%Totals%",
				"" + total));

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

		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		ArrayList<String> voteNames = UserManager.getInstance().getAllUUIDs();

		msg.add(config.getFormatCommandsVoteTotalAllTitle());
		int total = 0;
		for (VoteSite voteSite : voteSites) {
			int votes = 0;
			for (String uuid : voteNames) {
				if (uuid != null) {
					User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
					votes += user.getTotal(voteSite);
				}
			}
			msg.add(StringUtils.getInstance().replaceIgnoreCase(
					StringUtils.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteTotalAllLine(),
							"%SiteName%", voteSite.getSiteName()),
					"%Total%", "" + votes));
			total += votes;
		}
		msg.add(StringUtils.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteTotalAllTotal(), "%Totals%",
				"" + total));

		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	public String voteCommandTotalLine(User user, VoteSite voteSite) {
		String line = config.getFormatCommandsVoteTotalLine();
		return StringUtils.getInstance().replaceIgnoreCase(
				StringUtils.getInstance().replaceIgnoreCase(line, "%Total%", "" + user.getTotal(voteSite)),
				"%SiteName%", voteSite.getSiteName());
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
		texts.add(StringUtils.getInstance().stringToComp(config.getFormatCommandsVoteHelpTitle()));

		boolean requirePerms = config.getFormatCommandsVoteHelpRequirePermission();

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
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.voteToday.get(user).get(voteSite).format(formatter);
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
		ArrayList<VoteSite> voteSites = plugin.getVoteSites();

		List<String> title = config.getFormatCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (config.getFormatCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : voteSites) {
				counter++;
				String voteURL = configVoteSites.getVoteURL(voteSite.getSiteName());
				String msg = config.getFormatCommandsVoteURLS();
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
