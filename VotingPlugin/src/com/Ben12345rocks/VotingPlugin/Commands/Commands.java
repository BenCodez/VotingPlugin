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

import com.Ben12345rocks.AdvancedCore.CommandAPI.CommandHandler;
import com.Ben12345rocks.AdvancedCore.TimeChecker.TimeChecker;
import com.Ben12345rocks.AdvancedCore.UserManager.UUID;
import com.Ben12345rocks.AdvancedCore.Util.Messages.MessageBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.MiscUtils;
import com.Ben12345rocks.AdvancedCore.Util.Scoreboards.SimpleScoreboard;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
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

		msg.add(StringParser.getInstance().stringToComp("&3&lVotingPlugin Admin Help " + (page + 1) + "/" + maxPage));
		msg.add(StringParser.getInstance().stringToComp("&3&l() = Needed"));
		msg.add(StringParser.getInstance().stringToComp("&3&lAliases: adminvote, av"));

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

		boolean requirePerms = Config.getInstance().getFormatCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.getAdminVoteCommand()) {
			if (sender.hasPermission(cmdHandle.getPerm()) && requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/adminvote"), cmdHandle.getHelpLine("/adminvote"));
			} else {
				unsorted.put(cmdHandle.getHelpLineCommand("/adminvote"), cmdHandle.getHelpLine("/adminvote"));
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

	public String[] best(CommandSender sender, String name) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add(config.getFormatCommandsVoteBestTitle());
		msg.addAll(config.getFormatCommandsVoteBestLines());

		User user = UserManager.getInstance().getVotingPluginUser(name);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("HighestDailyTotal", "" + user.getHighestDailyTotal());
		placeholders.put("HighestWeeklyTotal", "" + user.getHighestWeeklyTotal());
		placeholders.put("HighestMonthlyTotal", "" + user.getHighestMonthlyTotal());

		placeholders.put("player", name);

		msg = ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders);

		return ArrayUtils.getInstance().convert(ArrayUtils.getInstance().colorize(msg));
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

	public String[] listPerms(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("&c&lCommand : Permissions (seperated by |)");

		for (CommandHandler handle : plugin.getVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&a" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : false");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/vote") + " : " + handle.getPerm());
			}

		}

		for (CommandHandler handle : plugin.getAdminVoteCommand()) {
			if (sender instanceof Player) {
				if (handle.hasPerm(sender)) {
					msg.add("&a" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : false");
				}
			} else {
				msg.add(handle.getHelpLineCommand("/av") + " : " + handle.getPerm());
			}
		}

		for (Permission perm : plugin.getDescription().getPermissions()) {
			if (sender instanceof Player) {
				if (sender.hasPermission(perm)) {
					msg.add("&a" + perm.getName() + " : true");
				} else {
					msg.add("&c" + perm.getName() + " : false");
				}
			} else {
				msg.add(perm.getName());
			}
		}

		msg = ArrayUtils.getInstance().colorize(msg);

		return ArrayUtils.getInstance().convert(msg);
	}

	public void listPerms(CommandSender sender, String player, int page) {
		Player p = Bukkit.getPlayer(player);
		if (p != null) {

			ArrayList<String> msg = new ArrayList<String>();
			ArrayList<String> text = new ArrayList<String>();

			for (CommandHandler handle : plugin.getVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&a" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/vote") + " : " + handle.getPerm() + " : false");
				}

			}

			for (CommandHandler handle : plugin.getAdminVoteCommand()) {
				if (handle.hasPerm(p)) {
					msg.add("&a" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : true");
				} else {
					msg.add("&c" + handle.getHelpLineCommand("/av") + " : " + handle.getPerm() + " : false");
				}
			}

			for (Permission perm : plugin.getDescription().getPermissions()) {
				if (p.hasPermission(perm)) {
					msg.add("&a" + perm.getName() + " : true");
				} else {
					msg.add("&c" + perm.getName() + " : false");
				}
			}

			msg = ArrayUtils.getInstance().colorize(msg);

			int pagesize = Config.getInstance().getFormatPageSize();

			int maxPage = msg.size() / pagesize;
			if ((msg.size() % pagesize) != 0) {
				maxPage++;
			}

			text.add("&c&lCommand : Permissions (seperated by |) " + page + "/" + maxPage);

			for (int i = pagesize * page - pagesize; i < msg.size() && i < pagesize * page; i++) {
				text.add(msg.get(i));
			}
			sender.sendMessage(ArrayUtils.getInstance().convert(ArrayUtils.getInstance().colorize(text)));
		} else {
			sender.sendMessage(StringParser.getInstance().colorize("&cPlayer not online: " + player));
		}

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
	 * Send top voter monthly score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterAllTimeScoreBoard(final Player player, int page) {
		if (Main.plugin.getOptions().isSendScoreboards()) {
			int pagesize = Config.getInstance().getFormatPageSize();
			ArrayList<User> users = plugin.convertSet(plugin.getTopVoter(TopVoter.AllTime).keySet());

			int pageSize = (users.size() / pagesize);
			if ((users.size() % pagesize) != 0) {
				pageSize++;
			}

			String title = StringParser.getInstance().colorize(config.getFormatCommandVoteTopTitle()
					.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "All"));

			SimpleScoreboard scoreboard = new SimpleScoreboard(title);

			for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
				scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
						plugin.getTopVoter(TopVoter.AllTime).get(users.get(i)));
			}
			scoreboard.build();
			scoreboard.send(player);

			Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

				@Override
				public void run() {
					player.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
				}
			}, 90);
		}
	}

	/**
	 * Send top voter daily score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterDailyScoreBoard(final Player player, int page) {
		if (Main.plugin.getOptions().isSendScoreboards()) {
			int pagesize = Config.getInstance().getFormatPageSize();
			ArrayList<User> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Daily).keySet());

			int pageSize = (users.size() / pagesize);
			if ((users.size() % pagesize) != 0) {
				pageSize++;
			}

			String title = StringParser.getInstance().colorize(config.getFormatCommandVoteTopTitle()
					.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Daily"));

			SimpleScoreboard scoreboard = new SimpleScoreboard(title);

			for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
				scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
						plugin.getTopVoter(TopVoter.Daily).get(users.get(i)));
			}
			scoreboard.build();
			scoreboard.send(player);

			Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

				@Override
				public void run() {
					player.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
				}
			}, 90);
		}
	}

	/**
	 * Send top voter monthly score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterMonthlyScoreBoard(final Player player, int page) {
		if (Main.plugin.getOptions().isSendScoreboards()) {
			int pagesize = Config.getInstance().getFormatPageSize();
			ArrayList<User> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Monthly).keySet());

			int pageSize = (users.size() / pagesize);
			if ((users.size() % pagesize) != 0) {
				pageSize++;
			}

			String title = StringParser.getInstance().colorize(config.getFormatCommandVoteTopTitle()
					.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Monthly"));

			SimpleScoreboard scoreboard = new SimpleScoreboard(title);

			for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
				scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
						plugin.getTopVoter(TopVoter.Monthly).get(users.get(i)));
			}
			scoreboard.build();
			scoreboard.send(player);

			Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

				@Override
				public void run() {
					player.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
				}
			}, 90);
		}
	}

	/**
	 * Send top voter weekly score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendTopVoterWeeklyScoreBoard(final Player player, int page) {
		if (Main.plugin.getOptions().isSendScoreboards()) {
			int pagesize = Config.getInstance().getFormatPageSize();
			ArrayList<User> users = plugin.convertSet(plugin.getTopVoter(TopVoter.Weekly).keySet());

			int pageSize = (users.size() / pagesize);
			if ((users.size() % pagesize) != 0) {
				pageSize++;
			}

			String title = StringParser.getInstance().colorize(config.getFormatCommandVoteTopTitle()
					.replace("%page%", "" + page).replace("%maxpages%", "" + pageSize).replace("%Top%", "Weekly"));

			SimpleScoreboard scoreboard = new SimpleScoreboard(title);

			for (int i = (page - 1) * pagesize; (i < users.size()) && (i < (((page - 1) * pagesize) + 10)); i++) {
				scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
						plugin.getTopVoter(TopVoter.Weekly).get(users.get(i)));
			}
			scoreboard.build();
			scoreboard.send(player);

			Bukkit.getScheduler().runTaskLater(plugin, new Runnable() {

				@Override
				public void run() {
					player.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
				}
			}, 90);

		}
	}

	/**
	 * Send vote today score board.
	 *
	 * @param player
	 *            the player
	 * @param page
	 *            the page
	 */
	public void sendVoteTodayScoreBoard(final Player player, int page) {
		if (Main.plugin.getOptions().isSendScoreboards()) {
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
					player.setScoreboard(Bukkit.getScoreboardManager().getNewScoreboard());
				}
			}, 90);

		}
	}

	public String[] streak(CommandSender sender, String name) {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add(config.getFormatCommandsVoteStreakTitle());
		msg.addAll(config.getFormatCommandsVoteStreakLines());

		User user = UserManager.getInstance().getVotingPluginUser(name);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("DailyStreak", "" + user.getDayVoteStreak());
		placeholders.put("WeeklyStreak", "" + user.getWeekVoteStreak());
		placeholders.put("MonthlyStreak", "" + user.getMonthVoteStreak());

		placeholders.put("BestDailyStreak", "" + user.getBestDayVoteStreak());
		placeholders.put("BestWeeklyStreak", "" + user.getBestWeekVoteStreak());
		placeholders.put("BestMonthlyStreak", "" + user.getBestMonthVoteStreak());

		placeholders.put("player", name);

		msg = ArrayUtils.getInstance().replacePlaceHolder(msg, placeholders);

		return ArrayUtils.getInstance().convert(ArrayUtils.getInstance().colorize(msg));
	}

	public void updateVoteToday(ArrayList<User> users) {
		plugin.getVoteToday().clear();

		for (User user : users) {
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
				plugin.getVoteToday().put(user, times);
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

		String playerName = user.getPlayerName();

		msg.add(StringParser.getInstance().replaceIgnoreCase(config.getFormatCommandsVoteLastTitle(), "%player%",
				playerName));

		for (VoteSite voteSite : plugin.getVoteSites()) {
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
	@Deprecated
	public String voteCommandLastDate(User user, VoteSite voteSite) {
		long time = user.getTime(voteSite);
		if (time > 0) {
			Date date = new Date(time);
			String timeString = new SimpleDateFormat(config.getFormatTimeFormat()).format(date);
			if (StringParser.getInstance().containsIgnorecase(timeString, "YamlConfiguration")) {
				plugin.getLogger().warning("Detected issue parsing time, check time format");
			}
			return timeString;
		}
		return "";
	}

	public String voteCommandLastDuration(User user, VoteSite voteSite) {
		long time = user.getTime(voteSite);
		if (time > 0) {
			LocalDateTime now = LocalDateTime.now();
			LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault());

			Duration dur = Duration.between(lastVote, now);

			long diffSecond = dur.getSeconds();
			int diffDays = (int) (diffSecond / 60 / 60 / 24);
			int diffHours = (int) (diffSecond / 60 / 60 - diffDays * 24);
			int diffMinutes = (int) (diffSecond / 60 - diffHours * 60 - diffDays * 24 * 60);
			int diffSeconds = (int) (diffSecond - diffMinutes * 60 - diffHours * 60 * 60 - diffDays * 24 * 60 * 60);

			String info = "";
			if (diffDays == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsDay()), "amount", "" + diffDays);
				info += " ";
			} else if (diffDays > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsDays()), "amount", "" + diffDays);
				info += " ";
			}

			if (diffHours == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsHour()), "amount", "" + diffHours);
				info += " ";
			} else if (diffHours > 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsHours()), "amount", "" + diffHours);
				info += " ";
			}

			if (diffMinutes == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsMinute()), "amount", "" + diffMinutes);
				info += " ";
			} else if (diffMinutes > 1) {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						Config.getInstance().getFormatTimeFormatsMinutes()), "amount", "" + diffMinutes);
				info += " ";
			}

			if (diffSeconds == 1) {
				info += StringParser.getInstance()
						.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
								Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
								Config.getInstance().getFormatTimeFormatsSecond()), "amount", "" + diffSeconds);
			} else {
				info += StringParser.getInstance().replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
						Config.getInstance().getFormatCommandsVoteLastTimeFormat(), "TimeType",
						Config.getInstance().getFormatTimeFormatsSeconds()), "amount", "" + diffSeconds);
			}

			info = StringParser.getInstance()
					.replacePlaceHolder(Config.getInstance().getFormatCommandsVoteLastLastVoted(), "times", info);

			return info;
		}
		return Config.getInstance().getFormatCommandsVoteLastNeverVoted();
	}

	public String voteCommandLastLine(User user, VoteSite voteSite) {
		String timeString = voteCommandLastDate(user, voteSite);
		String timeSince = voteCommandLastDuration(user, voteSite);

		HashMap<String, String> placeholders = new HashMap<String, String>();
		placeholders.put("time", timeString);
		placeholders.put("SiteName", voteSite.getDisplayName());
		placeholders.put("timesince", timeSince);

		return StringParser.getInstance().replacePlaceHolder(Config.getInstance().getFormatCommandsVoteLastLine(),
				placeholders);
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

		String playerName = user.getPlayerName();

		msg.add(StringParser.getInstance().colorize(StringParser.getInstance()
				.replaceIgnoreCase(config.getFormatCommandsVoteNextTitle(), "%player%", playerName)));

		for (VoteSite voteSite : plugin.getVoteSites()) {

			String msgLine = config.getFormatCommandsVoteNextLayout();

			msgLine = StringParser.getInstance().replaceIgnoreCase(msgLine, "%info%",
					voteCommandNextInfo(user, voteSite));

			msgLine = StringParser.getInstance().replaceIgnoreCase(msgLine, "%SiteName%", voteSite.getDisplayName());
			msg.add(StringParser.getInstance().colorize(msgLine));

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
		LocalDateTime now = TimeChecker.getInstance().getTime();
		;
		LocalDateTime lastVote = LocalDateTime.ofInstant(Instant.ofEpochMilli(time), ZoneId.systemDefault())
				.plusHours(Main.plugin.getOptions().getTimeHourOffSet());

		if (!voteSite.isVoteDelayDaily()) {
			double votedelay = voteSite.getVoteDelay();
			if (votedelay == 0 && voteSite.getVoteDelayMin() == 0) {
				String errorMsg = config.getFormatCommandsVoteNextInfoError();
				info = errorMsg;
			} else {

				LocalDateTime nextvote = lastVote.plusHours((long) votedelay)
						.plusMinutes((long) voteSite.getVoteDelayMin());

				if (time == 0 || now.isAfter(nextvote)) {
					info = config.getFormatCommandsVoteNextInfoCanVote();
				} else {
					Duration dur = Duration.between(now, nextvote);

					long diffHours = dur.getSeconds() / (60 * 60);
					long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

					String timeMsg = config.getFormatCommandsVoteNextInfoTime();
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%",
							Long.toString(diffHours));
					timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
							Long.toString(diffMinutes));
					info = timeMsg;

				}
			}
		} else {
			LocalDateTime midnight = TimeChecker.getInstance().getTime().plusDays(1).withHour(0).withMinute(0)
					.plusHours((long) voteSite.getTimeOffSet());
			Duration dur = Duration.between(now, midnight);

			if (!dur.isNegative()) {
				int diffHours = (int) (dur.getSeconds() / (60 * 60));
				long diffMinutes = dur.getSeconds() / 60 - diffHours * 60;

				if (diffHours < 0) {
					diffHours = diffHours * -1;
				}
				if (diffHours >= 24) {
					diffHours = diffHours - 24;
				}
				if (diffMinutes < 0) {
					diffMinutes = diffMinutes * -1;
				}

				String timeMsg = config.getFormatCommandsVoteNextInfoVoteDelayDaily();
				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%hours%", Integer.toString(diffHours));
				timeMsg = StringParser.getInstance().replaceIgnoreCase(timeMsg, "%minutes%",
						Long.toString(diffMinutes));
				info = timeMsg;

			} else {
				info = config.getFormatCommandsVoteNextInfoCanVote();
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

		int daily = user.getTotal(TopVoter.Daily);
		int weekly = user.getTotal(TopVoter.Weekly);
		int month = user.getTotal(TopVoter.Monthly);
		int all = user.getTotal(TopVoter.AllTime);

		for (String s : config.getFormatCommandsVoteTotal()) {
			String str = StringParser.getInstance().replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%player%", user.getPlayerName());
			msg.add(str);
		}

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

		ArrayList<String> uuids = UserManager.getInstance().getAllUUIDs();

		int daily = 0;
		int weekly = 0;
		int month = 0;
		int all = 0;

		for (String uuid : uuids) {
			User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
			daily += user.getTotal(TopVoter.Daily);
			weekly += user.getTotal(TopVoter.Weekly);
			month += user.getTotal(TopVoter.Monthly);
			all += user.getTotal(TopVoter.AllTime);
		}

		for (String s : config.getFormatCommandsVoteTotalAll()) {
			String str = StringParser.getInstance().replaceIgnoreCase(s, "%DailyTotal%", "" + daily);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%WeeklyTotal%", "" + weekly);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%MonthlyTotal%", "" + month);
			str = StringParser.getInstance().replaceIgnoreCase(str, "%AllTimeTotal%", "" + all);
			msg.add(str);
		}

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
		texts.add(StringParser.getInstance().stringToComp(config.getFormatCommandsVoteHelpTitle()));

		boolean requirePerms = config.getFormatCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.getVoteCommand()) {
			if (cmdHandle.hasPerm(sender)) {
				unsorted.put(cmdHandle.getHelpLineCommand("/vote"), cmdHandle.getHelpLine("/vote"));
			} else if (!requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/vote"), cmdHandle.getHelpLine("/vote"));
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
		for (User user : plugin.getVoteToday().keySet()) {

			for (VoteSite voteSite : plugin.getVoteToday().get(user).keySet()) {
				DateTimeFormatter formatter = DateTimeFormatter.ofPattern(Config.getInstance().getFormatTimeFormat());
				String timeString = plugin.getVoteToday().get(user).get(voteSite).format(formatter);
				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("player", user.getPlayerName());
				placeholders.put("votesite", voteSite.getKey());
				placeholders.put("time", timeString);
				msg.add(StringParser.getInstance()
						.replacePlaceHolder(Config.getInstance().getFormatCommandsVoteTodayLine(), placeholders));
				// msg.add("&6" + user.getPlayerName() + " : " + voteSite.getKey() + " : " +
				// timeString);
			}
		}
		msg = ArrayUtils.getInstance().colorize(msg);
		return ArrayUtils.getInstance().convert(msg);
	}

	public String[] voteURLs(User user, boolean json) {
		ArrayList<String> sites = new ArrayList<String>();

		List<String> title = config.getFormatCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (config.getFormatCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : plugin.getVoteSites()) {
				counter++;
				String voteURL = voteSite.getVoteURL(json);
				MessageBuilder message = new MessageBuilder(config.getFormatCommandsVoteURLS());
				message.replacePlaceholder("num", Integer.toString(counter)).replacePlaceholder("url", voteURL)
						.replacePlaceholder("SiteName", voteSite.getDisplayName());
				if (user != null && user.getPlayerName() != null) {
					message.replacePlaceholder("player", "" + user.getPlayerName()).replacePlaceholder("Next",
							"" + voteCommandNextInfo(user, voteSite));
				}
				sites.add(message.colorize().getText());
			}
		}
		if (user != null) {
			HashMap<String, String> phs = new HashMap<String, String>();
			phs.put("DailyTotal", "" + user.getTotal(TopVoter.Daily));
			phs.put("WeekTotal", "" + user.getTotal(TopVoter.Weekly));
			phs.put("MonthTotal", "" + user.getTotal(TopVoter.Monthly));
			phs.put("Total", "" + user.getTotal(TopVoter.AllTime));

			sites = ArrayUtils.getInstance().replacePlaceHolder(sites, phs);
		}

		sites = ArrayUtils.getInstance().colorize(sites);
		return ArrayUtils.getInstance().convert(sites);
	}

}
