package com.Ben12345rocks.VotingPlugin.Commands;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

import net.md_5.bungee.api.chat.TextComponent;

import org.apache.commons.lang3.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.permissions.Permission;

import com.Ben12345rocks.AdvancedCore.Utils;
import com.Ben12345rocks.AdvancedCore.Objects.CommandHandler;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Scoreboards.SimpleScoreboard;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigGUI;
import com.Ben12345rocks.VotingPlugin.Config.ConfigOtherRewards;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;

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

		msg.add(Utils.getInstance().stringToComp(
				"&3&lVotingPlugin Admin Help " + (page + 1) + "/" + maxPage));
		msg.add(Utils.getInstance().stringToComp("&3&l() = Needed"));
		msg.add(Utils.getInstance().stringToComp("&3&lAliases: adminvote, av"));

		for (int i = pagesize * page; (i < text.size())
				&& (i < ((page + 1) * pagesize)); i++) {
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

		boolean requirePerms = ConfigFormat.getInstance()
				.getCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.adminVoteCommand) {
			if (sender.hasPermission(cmdHandle.getPerm()) && requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/av"),
						cmdHandle.getHelpLine("/av"));
			} else {
				unsorted.put(cmdHandle.getHelpLineCommand("/av"),
						cmdHandle.getHelpLine("/av"));
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

		for (int i = pagesize * page; (i < voteToday.length)
				&& (i < ((page + 1) * pagesize)); i++) {
			msg.add(voteToday[i]);
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
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

		msg = Utils.getInstance().colorize(msg);
		Collections.sort(msg, String.CASE_INSENSITIVE_ORDER);

		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Open vote GUI.
	 *
	 * @param player
	 *            the player
	 */
	public void openVoteGUI(Player player) {
		BInventory inv = new BInventory("VoteGUI");

		for (String slot : ConfigGUI.getInstance().getVoteGUISlots()) {
			ItemStack item = new ItemStack(Material.STONE);

			try {
				item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteGUISlotMaterial(slot)), ConfigGUI
						.getInstance().getVoteGUISlotAmount(slot),
						(short) ConfigGUI.getInstance()
						.getVoteGUISlotData(slot));
			} catch (Exception ex) {

			}

			item = Utils.getInstance().setSkullOwner(item,
					ConfigGUI.getInstance().getVoteGUISlotSkull(player, slot));

			item = Utils.getInstance().setDurabilty(item,
					ConfigGUI.getInstance().getVoteGUISlotDurability(slot));

			String[] lore = new String[1];

			lore = Utils.getInstance().convertArray(
					ConfigGUI.getInstance().getVoteGUISlotLore(slot));
			if (slot.equalsIgnoreCase("url")) {
				lore = Commands.getInstance().voteURLs();
			} else if (slot.equalsIgnoreCase("next")) {
				lore = Commands.getInstance().voteCommandNext(new User(player));
			} else if (slot.equalsIgnoreCase("last")) {
				lore = Commands.getInstance().voteCommandLast(new User(player));
			} else if (slot.equalsIgnoreCase("total")) {
				lore = Commands.getInstance()
						.voteCommandTotal(new User(player));
			} else if (slot.equalsIgnoreCase("top")) {
				lore = TopVoter.getInstance().topVoterMonthly(1);
			} else if (slot.equalsIgnoreCase("today")) {
				lore = voteToday();
			} else if (slot.equalsIgnoreCase("help")) {
				ArrayList<String> loreSt = new ArrayList<String>();
				loreSt = Utils.getInstance().comptoString(voteHelpText(player));
				lore = Utils.getInstance().convertArray(loreSt);
			}

			inv.addButton(ConfigGUI.getInstance().getVoteGUISlotSlot(slot),
					new BInventoryButton(ConfigGUI.getInstance()
							.getVoteGUISlotName(slot), lore, item) {

				@Override
				public void onClick(ClickEvent event) {
					Player player = (Player) event.getWhoClicked();
					if (player != null) {
						// plugin.debug("Running cmd");
						player.closeInventory();
						player.performCommand(ConfigGUI.getInstance()
								.getVoteGUISlotCommand(slot));

					}

				}
			});
		}

		BInventory.openInventory(player, inv);
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
		msg.addAll(Utils.getInstance().convertArray(voteCommandLast(user)));

		// next vote
		msg.addAll(Utils.getInstance().convertArray(voteCommandNext(user)));

		// total
		msg.addAll(Utils.getInstance().convertArray(voteCommandTotal(user)));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
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
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				TopVoter.getInstance().topVotersDaily());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = Utils.getInstance().colorize(
				format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize)
				.replace("%Top%", "Daily"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterDaily.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
					plugin.topVoterDaily.get(users.get(i)));
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
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				TopVoter.getInstance().topVoters());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = Utils.getInstance().colorize(
				format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize)
				.replace("%Top%", "Monthly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterMonthly.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
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
		ArrayList<String> topVoters = Utils.getInstance().convertArray(
				TopVoter.getInstance().topVotersWeekly());

		int pageSize = (topVoters.size() / pagesize);
		if ((topVoters.size() % pagesize) != 0) {
			pageSize++;
		}

		String title = Utils.getInstance().colorize(
				format.getCommandVoteTopTitle().replace("%page%", "" + page)
				.replace("%maxpages%", "" + pageSize)
				.replace("%Top%", "Weekly"));

		SimpleScoreboard scoreboard = new SimpleScoreboard(title);

		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(plugin.topVoterWeekly.keySet());
		for (int i = (page - 1) * pagesize; (i < topVoters.size())
				&& (i < (((page - 1) * pagesize) + 10)); i++) {
			scoreboard.add("" + (i + 1) + ": " + users.get(i).getPlayerName(),
					plugin.topVoterWeekly.get(users.get(i)));
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

		SimpleScoreboard scoreboard = new SimpleScoreboard("&cToday's Votes "
				+ page + "/" + maxPage);

		for (int i = pagesize * page; (i < voteToday.length)
				&& (i < ((page + 1) * pagesize)); i++) {
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
		ArrayList<User> users = com.Ben12345rocks.VotingPlugin.Utils
				.getInstance().convertSet(Data.getInstance().getUsers());
		plugin.voteToday.clear();

		if (users != null) {
			for (User user : users) {
				HashMap<VoteSite, Date> times = new HashMap<VoteSite, Date>();
				for (VoteSite voteSite : configVoteSites.getVoteSites()) {
					long time = user.getTime(voteSite);
					if ((new Date().getDate() == Utils.getInstance()
							.getDayFromMili(time))
							&& (new Date().getMonth() == Utils.getInstance()
							.getMonthFromMili(time))
							&& (new Date().getYear() == Utils.getInstance()
							.getYearFromMili(time))) {

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

		msg.add(Utils.getInstance().replaceIgnoreCase(
				format.getCommandsVoteLastTitle(), "%player%", playerName));

		for (VoteSite voteSite : voteSites) {
			String timeString = voteCommandLastDate(user, voteSite);

			msg.add(format
					.getCommandsVoteLastLine()
					.replace("%Month% %Day%, %Year% %Hour%:%Minute% %ampm%",
							"%time%").replace("%time%", timeString)
							.replace("%SiteName%", voteSite.getSiteName()));
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
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
		String timeString = new SimpleDateFormat(format.getTimeFormat())
		.format(date);
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

		msg.add(Utils.getInstance().colorize(
				Utils.getInstance().replaceIgnoreCase(
						format.getCommandsVoteNextTitle(), "%player%",
						playerName)));

		for (VoteSite voteSite : voteSites) {

			String msgLine = format.getCommandsVoteNextLayout();

			msgLine = Utils.getInstance().replaceIgnoreCase(msgLine, "%info%",
					voteCommandNextInfo(user, voteSite));

			msgLine = Utils.getInstance().replaceIgnoreCase(msgLine,
					"%SiteName%", voteSite.getSiteName());
			msg.add(Utils.getInstance().colorize(msgLine));

		}
		return Utils.getInstance().convertArray(msg);
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
					timeMsg = Utils.getInstance().replaceIgnoreCase(timeMsg,
							"%hours%", Long.toString(diffHours));
					timeMsg = Utils.getInstance().replaceIgnoreCase(timeMsg,
							"%minutes%", Long.toString(diffMinutes));
					info = timeMsg;
				} else {
					String canVoteMsg = format.getCommandsVoteNextInfoCanVote();
					info = canVoteMsg;
				}
			}
		}
		return info;
	}

	/**
	 * Vote command site info.
	 *
	 * @param voteSiteName
	 *            the vote site name
	 * @return the string[]
	 */
	public String[] voteCommandSiteInfo(String voteSiteName) {
		ArrayList<String> msg = new ArrayList<String>();

		if (!ConfigVoteSites.getInstance().getVoteSiteFile(voteSiteName)
				.exists()) {
			msg.add("&cInvalid Vote Site, see /av sites!");
		} else {

			VoteSite voteSite = plugin.getVoteSite(voteSiteName);

			msg.add("&c&lVote Site Info for " + voteSiteName + ":");

			msg.add("&cSite: &6" + voteSite.getServiceSite());
			msg.add("&cVoteURL: &6" + voteSite.getVoteURL());
			msg.add("&cVote Delay: &6" + voteSite.getVoteDelay());
			msg.add("&cPriority: &6"
					+ ConfigVoteSites.getInstance().getPriority(voteSiteName));
			msg.add("&cRewards:");
			for (String reward : voteSite.getRewards()) {
				msg.add("&6- " + reward);
			}

		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Vote command sites.
	 *
	 * @return the string[]
	 */
	public String[] voteCommandSites() {
		ArrayList<String> msg = new ArrayList<String>();

		msg.add("&c&lVote Sites:");

		int count = 1;
		ArrayList<VoteSite> voteSites = ConfigVoteSites.getInstance()
				.getVoteSites();
		if (voteSites != null) {
			for (VoteSite voteSite : voteSites) {
				msg.add("&c" + count + ". &6" + voteSite.getSiteName());
				count++;
			}
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
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

		msg.add(Utils.getInstance().replaceIgnoreCase(
				format.getCommandsVoteTotalTitle(), "%player%", playerName));

		// total votes
		int total = 0;

		for (VoteSite voteSite : voteSites) {
			int votes = user.getTotal(voteSite);
			// int votes = Data.getInstance().getTotal(playerName, siteName);
			total += votes;
			String line = format.getCommandsVoteTotalLine();
			msg.add(Utils.getInstance().replaceIgnoreCase(
					Utils.getInstance().replaceIgnoreCase(line, "%Total%",
							"" + votes), "%SiteName%", voteSite.getSiteName()));

		}
		msg.add(Utils.getInstance().replaceIgnoreCase(
				format.getCommandsVoteTotalTotal(), "%Totals%", "" + total));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Vote command total all.
	 *
	 * @return the string[]
	 */
	public String[] voteCommandTotalAll() {

		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<VoteSite> voteSites = plugin.voteSites;

		ArrayList<String> voteNames = com.Ben12345rocks.AdvancedCore.Data.Data
				.getInstance().getPlayerNames();

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
			msg.add(Utils.getInstance().replaceIgnoreCase(
					Utils.getInstance().replaceIgnoreCase(
							format.getCommandsVoteTotalAllLine(), "%SiteName%",
							voteSite.getSiteName()), "%Total%", "" + votes));
			total += votes;
		}
		msg.add(Utils.getInstance().replaceIgnoreCase(
				format.getCommandsVoteTotalAllTotal(), "%Totals%", "" + total));

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/*
	 * public String[] voteHelpTextColored() { ArrayList<String> texts = new
	 * ArrayList<String>(); String helpLine =
	 * ConfigFormat.getInstance().getCommandsVoteHelpLine(); for (String msg :
	 * voteHelpText()) { if (msg.split("-").length > 1) { String command =
	 * msg.split("-")[0]; String helpMessage = msg.split("-")[1];
	 * texts.add(helpLine.replace("%Command%", command).replace(
	 * "%HelpMessage%", helpMessage)); } else { String command =
	 * msg.split("-")[0]; texts.add(helpLine.replace("%Command%", command)); } }
	 * texts = Utils.getInstance().colorize(texts); return
	 * Utils.getInstance().convertArray(texts);
	 *
	 * }
	 */

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
		texts.add(Utils.getInstance().stringToComp(
				ConfigFormat.getInstance().getCommandsVoteHelpTitle()));

		boolean requirePerms = ConfigFormat.getInstance()
				.getCommandsVoteHelpRequirePermission();

		for (CommandHandler cmdHandle : plugin.voteCommand) {
			if (sender.hasPermission(cmdHandle.getPerm()) && requirePerms) {
				unsorted.put(cmdHandle.getHelpLineCommand("/v"),
						cmdHandle.getHelpLine("/v"));
			} else {
				unsorted.put(cmdHandle.getHelpLineCommand("/v"),
						cmdHandle.getHelpLine("/v"));
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
	 * Vote reward.
	 *
	 * @param player
	 *            the player
	 * @param siteName
	 *            the site name
	 */
	public void voteReward(Player player, String siteName) {
		BInventory inv = new BInventory("VoteReward");

		if ((siteName == null) || (siteName == "")) {
			int count = 0;
			for (VoteSite voteSite : plugin.voteSites) {
				plugin.debug(voteSite.getSiteName());

				try {
					ItemStack item = new ItemStack(
							Material.getMaterial(ConfigGUI.getInstance()
									.getVoteSiteItemMaterial(
											voteSite.getSiteName())),
											ConfigGUI.getInstance().getVoteSiteItemAmount(
													voteSite.getSiteName()),
													(short) ConfigGUI
													.getInstance()
													.getVoteSiteItemData(voteSite.getSiteName()));
					item = Utils.getInstance().setDurabilty(
							item,
							ConfigGUI.getInstance().getVoteSiteItemDurability(
									voteSite.getSiteName()));

					item = Utils.getInstance().setSkullOwner(
							item,
							ConfigGUI.getInstance().getVoteSiteItemSkull(
									player, voteSite.getSiteName()));

					inv.addButton(
							count,
							new BInventoryButton(
									ConfigGUI.getInstance()
									.getVoteSiteItemName(
											voteSite.getSiteName()),
											Utils.getInstance()
											.convertArray(
													(ArrayList<String>) ConfigGUI
													.getInstance()
													.getVoteSiteItemLore(
															voteSite.getSiteName())),
															item) {

								@Override
								public void onClick(ClickEvent event) {
									Player player = (Player) event
											.getWhoClicked();
									if (player != null) {
										player.closeInventory();
										player.performCommand("vote reward "
												+ voteSite.getSiteName());

									}

								}
							});
					count++;
				} catch (Exception ex) {

				}
			}
		} else {
			for (String itemName : ConfigGUI.getInstance().getVoteSiteItems(
					siteName)) {
				ItemStack item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteSiteItemsMaterial(siteName,
								itemName)), ConfigGUI.getInstance()
								.getVoteSiteItemsAmount(siteName, itemName),
								(short) ConfigGUI.getInstance().getVoteSiteItemsData(
										siteName, itemName));

				item = Utils.getInstance().setDurabilty(
						item,
						ConfigGUI.getInstance().getVoteSiteItemsDurability(
								siteName, itemName));

				item = Utils.getInstance().setSkullOwner(
						item,
						ConfigGUI.getInstance().getVoteSiteItemsSkull(player,
								siteName, itemName));

				inv.addButton(
						ConfigGUI.getInstance().getVoteSiteItemsSlot(siteName,
								itemName),
								new BInventoryButton(ConfigGUI.getInstance()
										.getVoteSiteItemsName(siteName, itemName),
										Utils.getInstance().convertArray(
												(ArrayList<String>) ConfigGUI
												.getInstance()
												.getVoteSiteItemsLore(siteName,
														itemName)), item) {

							@Override
							public void onClick(ClickEvent event) {
								Player player = (Player) event.getWhoClicked();
								if (player != null) {
									player.closeInventory();
								}

							}
						});
			}
		}

		BInventory.openInventory(player, inv);
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
				msg.add("&6" + user.getPlayerName() + " : "
						+ voteSite.getSiteName() + " : " + timeString);
			}
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	/**
	 * Vote URL.
	 *
	 * @param player
	 *            the player
	 */
	public void voteURL(Player player) {
		BInventory inv = new BInventory("VoteURL");

		User user = new User(player);

		int count = 0;
		if (ConfigGUI.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemStack itemAll = new ItemStack(Material.getMaterial(ConfigGUI
					.getInstance().getVoteURLAlreadyVotedItemMaterial()),
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemAmount(),
					(short) ConfigGUI.getInstance()
					.getVoteURLAlreadyVotedItemData());
			itemAll = Utils.getInstance().setDurabilty(
					itemAll,
					ConfigGUI.getInstance()
					.getVoteURLAlreadyVotedItemDurability());
			itemAll = Utils.getInstance().setSkullOwner(
					itemAll,
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemSkull(
							player));
			if (user.canVoteAll()) {
				itemAll = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteURLCanVoteItemMaterial()),
						ConfigGUI.getInstance().getVoteURLCanVoteItemAmount(),
						(short) ConfigGUI.getInstance()
						.getVoteURLCanVoteItemData());
				itemAll = Utils.getInstance().setDurabilty(
						itemAll,
						ConfigGUI.getInstance()
						.getVoteURLCanVoteItemDurability());
				itemAll = Utils.getInstance().setSkullOwner(
						itemAll,
						ConfigGUI.getInstance().getVoteURLCanVoteItemSkull(
								player));
			}

			inv.addButton(count, new BInventoryButton("&4All Voting Sites",
					new String[] { "&cClick Me" }, itemAll) {

				@Override
				public void onClick(ClickEvent event) {
					User user = new User((Player) event.getWhoClicked());
					Player player = (Player) event.getWhoClicked();
					player.closeInventory();
					user.sendMessage(Commands.getInstance().voteURLs());

				}
			});

			count++;
		}

		for (VoteSite voteSite : plugin.voteSites) {
			ItemStack item = new ItemStack(Material.getMaterial(ConfigGUI
					.getInstance().getVoteURLAlreadyVotedItemMaterial()),
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemAmount(),
					(short) ConfigGUI.getInstance()
					.getVoteURLAlreadyVotedItemData());
			item = Utils.getInstance().setDurabilty(
					item,
					ConfigGUI.getInstance()
					.getVoteURLAlreadyVotedItemDurability());
			item = Utils.getInstance().setSkullOwner(
					item,
					ConfigGUI.getInstance().getVoteURLAlreadyVotedItemSkull(
							player));
			ArrayList<String> lore = new ArrayList<String>();
			lore.add(ConfigGUI.getInstance().getVoteURLSeeURL());

			if (user.canVoteSite(voteSite)) {
				item = new ItemStack(Material.getMaterial(ConfigGUI
						.getInstance().getVoteURLCanVoteItemMaterial()),
						ConfigGUI.getInstance().getVoteURLCanVoteItemAmount(),
						(short) ConfigGUI.getInstance()
						.getVoteURLCanVoteItemData());
				item = Utils.getInstance().setDurabilty(
						item,
						ConfigGUI.getInstance()
						.getVoteURLCanVoteItemDurability());
				item = Utils.getInstance().setSkullOwner(
						item,
						ConfigGUI.getInstance().getVoteURLCanVoteItemSkull(
								player));
			} else {
				lore.add(ConfigGUI.getInstance().getVoteURLNextVote()
						.replace("%Info%", voteCommandNextInfo(user, voteSite)));
			}

			inv.addButton(
					count,
					new BInventoryButton(ConfigGUI.getInstance()
							.getVoteURLSiteName()
							.replace("%Name%", voteSite.getSiteName()), Utils
							.getInstance().convertArray(lore), item) {

						@Override
						public void onClick(ClickEvent event) {
							Player player = (Player) event.getWhoClicked();
							if (player != null) {
								player.closeInventory();
								User user = new User(player);
								user.sendMessage(voteSite.getVoteURL());

							}

						}
					});
			count++;
		}

		BInventory.openInventory(player, inv);
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
				String voteURL = configVoteSites.getVoteURL(voteSite
						.getSiteName());
				String msg = format.getCommandsVoteURLS();
				msg = Utils.getInstance().colorize(msg);
				msg = Utils.getInstance().replaceIgnoreCase(msg, "%num%",
						Integer.toString(counter));
				msg = Utils.getInstance().replaceIgnoreCase(msg, "%url%",
						voteURL);
				msg = Utils.getInstance().replaceIgnoreCase(msg, "%SiteName%",
						voteSite.getSiteName());
				sites.add(msg);
			}
		}
		sites = Utils.getInstance().colorize(sites);
		return Utils.getInstance().convertArray(sites);
	}
}
