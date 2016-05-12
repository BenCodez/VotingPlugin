package com.Ben12345rocks.VotingPlugin.Commands;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.commons.lang3.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.ConfigBonusReward;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class Commands {

	static ConfigBonusReward bonusReward = ConfigBonusReward.getInstance();

	static Config config = Config.getInstance();

	static ConfigVoteSites configVoteSites = ConfigVoteSites.getInstance();

	static ConfigFormat format = ConfigFormat.getInstance();

	static Commands instance = new Commands();

	static Main plugin = Main.plugin;

	public static Commands getInstance() {
		return instance;
	}

	private Commands() {
	}

	public Commands(Main plugin) {
		Commands.plugin = plugin;
	}

	public ArrayList<String> adminHelpText() {
		ArrayList<String> msg = new ArrayList<String>();
		msg.add("VotingPlugin Admin Help");
		msg.add("[] = Optional");
		msg.add("() = Needed");
		msg.add("Aliases: adminvote, av");
		msg.add("/adminvote help - See this page");
		msg.add("/adminvote vote (player) (sitename) - Trigger vote");
		msg.add("/adminvote bungeevote (player) (sitename) - Trigger bungee only vote");
		msg.add("/adminvote servervote (player) (sitename) - Trigger server only vote");
		msg.add("/adminvote settotal (player) (sitename) (amount) - Set total votes of a player on votesite");
		msg.add("/adminvote reload - Reload the plugin");
		msg.add("/adminvote uuid (playername) - See uuid of player");
		msg.add("/adminvote version - Version info");
		msg.add("/adminvote sites [site] - List of sites and site info");
		msg.add("Editing Commands");
		msg.add("/adminvote VoteSite (SiteName) Create - Gernerate a votesite");
		msg.add("/adminvote VoteSite (SiteName) AddItem (Item) - Add item in hand to votesite");
		msg.add("/adminvote VoteSite (SiteName) SetMoney (Money) - Set money for votesite");
		msg.add("/adminvote VoteSite (SiteName) SetServiceSite (ServiceSite) - Set servicesite on votesite");
		msg.add("/adminvote VoteSite (SiteName) SetDisabled (Disabled) - Set votesite disabled");
		msg.add("/adminvote VoteSite (SiteName) SetVoteDelay (Delay) - Set votesite votedelay");
		msg.add("/adminvote VoteSite (SiteName) AddCommandPlayer (Command) - Add player command to votesite");
		msg.add("/adminvote VoteSite (SiteName) AddCommandConsole (Command) - Add console command to votesite");
		msg.add("/adminvote VoteSite (SiteName) AddExtraRewardItem (Reward) (Item) - Add ExtraReward item in hand to votesite");
		msg.add("/adminvote VoteSite (SiteName) SetExtraRewardMoney (Reward) (Money) - Set ExtraReward money for votesite");
		msg.add("/adminvote VoteSite (SiteName) SetExtraRewardChance (Reward) (Chance) - Set ExtraReward chance");
		msg.add("/adminvote VoteSite (SiteName) AddExtraRewardCommandPlayer (Reward) (Command) - Add ExtraReward player command to votesite");
		msg.add("/adminvote VoteSite (SiteName) AddExtraRewardCommandConsole (Reward) (Command) - Add ExtraReward console command to votesite");
		msg.add("/adminvote BonusReward AddItem (Item) - Add item in hand");
		msg.add("/adminvote BonusReward SetMoney (Money) - Set money");
		msg.add("/adminvote BonusReward SetGiveBonusReward (Disabled) - Set bonus reward enabled");
		msg.add("/adminvote BonusReward AddCommandPlayer (Command) - Add player command");
		msg.add("/adminvote BonusReward AddCommandConsole (Command) - Add console command");
		msg.add("/adminvote BonusReward AddExtraRewardItem (Reward) (Item) - Add ExtraReward item in hand");
		msg.add("/adminvote BonusReward SetExtraRewardMoney (Reward) (Money) - Set ExtraReward money");
		msg.add("/adminvote BonusReward SetExtraRewardChance (Reward) (Chance) - Set ExtraReward chance");
		msg.add("/adminvote BonusReward AddExtraRewardCommandPlayer (Reward) (Command) - Add ExtraReward player command");
		msg.add("/adminvote BonusReward AddExtraRewardCommandConsole (Reward) (Command) - Add ExtraReward console command");
		msg.add("/adminvote Config SetDebug (true/false) - Set debug");
		msg.add("/adminvote Config SetBroadcastVote (true/false) - Set broadcastvote");
		msg.add("/adminvote Config SetUpdateReminder (true/false) - Set updatereminder");
		msg.add("/adminvote Config SetAllowUnjoined (true/false) - Set allowunjoined");
		msg.add("/adminvote Config SetDisableTopVoterAwards (true/false) - Set disabletopvoterawards");
		msg.add("/adminvote ServerData SetPrevMonth - Set prevmonth, DO NOT USE");
		return msg;
	}

	public String[] adminHelpTextColored() {
		ArrayList<String> texts = new ArrayList<String>();
		for (String msg : adminHelpText()) {
			if (msg.split("-").length > 1) {
				texts.add("&3&l" + msg.split("-")[0] + "-&3"
						+ msg.split("-")[1]);
			} else {
				texts.add("&3&l" + msg.split("-")[0]);
			}
		}
		texts = Utils.getInstance().colorize(texts);
		return Utils.getInstance().convertArray(texts);

	}

	public String[] commandVoteToday(int page) {
		int pagesize = ConfigFormat.getInstance().getPageSize();
		if (page < 1) {
			page = 1;
		}
		ArrayList<String> msg = new ArrayList<String>();

		int maxPage = plugin.voteToday.length / pagesize;
		if (plugin.voteToday.length % pagesize != 0) {
			maxPage++;
		}

		msg.add("&cToday's Votes " + page + "/" + maxPage);
		msg.add("&cPlayerName : VoteSite : Time");
		page--;

		for (int i = pagesize * page; (i < plugin.voteToday.length)
				&& (i < ((page + 1) * pagesize)); i++) {
			msg.add(plugin.voteToday[i]);
		}

		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);

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

	public String voteCommandLastDate(User user, VoteSite voteSite) {
		Date date = new Date(user.getTime(voteSite));
		String timeString = new SimpleDateFormat(format.getTimeFormat())
				.format(date);
		return timeString;
	}

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
			msg.add("&cMoney: &6" + voteSite.getMoney());

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

			msg.add("&4&l&nExtra Rewards:");
			for (String reward : configVoteSites
					.getExtraRewardRewards(voteSiteName)) {
				msg.add("&4&lReward: &c" + reward);
				msg.add("&cChance: &6"
						+ voteSite.getExtraRewardsChance().get(reward));
				msg.add("&cMoney: &6"
						+ voteSite.getExtraRewardsMoney().get(reward));

				ArrayList<String> worlds = voteSite.getExtraRewardsWorld().get(
						reward);
				if (worlds != null) {
					msg.add("&cWorlds: "
							+ Utils.getInstance().makeStringList(worlds));
				}

				msg.add("&cPermission: &6"
						+ voteSite.getExtraRewardsPermission().get(reward));

				msg.add("&cItems:");
				for (String item : ConfigVoteSites.getInstance()
						.getExtraRewardItems(voteSite.getSiteName(), reward)) {
					msg.add("&c- &6" + item);
				}

				msg.add("&cPlayer Commands:");

				try {
					for (String playerCommands : voteSite
							.getExtraRewardsPlayerCommands().get(reward)) {
						msg.add("&c- " + playerCommands);
					}
				} catch (Exception ex) {
				}

				msg.add("&cConsole Commands:");

				try {
					for (String consoleCommands : voteSite
							.getExtraRewardsConsoleCommands().get(reward)) {
						msg.add("&c- " + consoleCommands);
					}
				} catch (Exception ex) {
				}
			}

			msg.add("&c&lCumulative Rewards:");

			msg.add("&cVotes: &6" + voteSite.getCumulativeVotes());
			msg.add("&cMoney: &6" + voteSite.getCumulativeMoney());

			msg.add("&cItems:");
			for (String item : ConfigVoteSites.getInstance()
					.getCumulativeRewardItems(voteSite.getSiteName())) {
				msg.add("&c- &6" + item);
			}

			msg.add("&cPlayer Commands:");

			try {
				for (String playerCommands : voteSite
						.getCumulativePlayerCommands()) {
					msg.add("&c- " + playerCommands);
				}
			} catch (Exception ex) {
			}

			msg.add("&cConsole Commands:");

			try {
				for (String consoleCommands : voteSite
						.getCumulativeConsoleCommands()) {
					msg.add("&c- " + consoleCommands);
				}
			} catch (Exception ex) {
			}
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

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

	public ArrayList<String> voteHelpText() {
		ArrayList<String> texts = new ArrayList<String>();
		texts.add(ConfigFormat.getInstance().getCommandsVoteHelpTitle());
		texts.addAll(ConfigFormat.getInstance().getCommandsVoteHelpLines());
		return texts;
	}

	public String[] voteHelpTextColored() {
		ArrayList<String> texts = new ArrayList<String>();
		for (String msg : voteHelpText()) {
			if (msg.split("-").length > 1) {
				texts.add("&3&l" + msg.split("-")[0] + "-&3"
						+ msg.split("-")[1]);
			} else {
				texts.add("&3&l" + msg.split("-")[0]);
			}
		}
		texts = Utils.getInstance().colorize(texts);
		return Utils.getInstance().convertArray(texts);

	}

	@SuppressWarnings("deprecation")
	public String[] voteToday() {
		ArrayList<String> msg = new ArrayList<String>();

		ArrayList<User> users = Utils.getInstance().convertSet(
				Data.getInstance().getUsers());

		if (users != null) {

			for (User user : users) {
				for (VoteSite voteSite : configVoteSites.getVoteSites()) {
					long time = user.getTime(voteSite);
					if (new Date().getDate() == Utils.getInstance()
							.getDayFromMili(time)) {

						String timeString = new SimpleDateFormat(
								format.getTimeFormat()).format(new Date(time));
						msg.add("&6" + user.getPlayerName() + " : "
								+ voteSite.getSiteName() + " : " + timeString);
					}
				}
			}
		}
		msg = Utils.getInstance().colorize(msg);
		return Utils.getInstance().convertArray(msg);
	}

	public String[] voteURLs() {
		ArrayList<String> sites = new ArrayList<String>();
		ArrayList<VoteSite> voteSites = configVoteSites.getVoteSites();

		List<String> title = ConfigFormat.getInstance().getCommandsVoteTitle();
		if (title != null) {
			sites.addAll(title);
		}
		int counter = 0;
		for (VoteSite voteSite : voteSites) {
			counter++;
			String voteURL = configVoteSites.getVoteURL(voteSite.getSiteName());
			String msg = format.getCommandsVoteURLS();
			msg = Utils.getInstance().colorize(msg);
			msg = Utils.getInstance().replaceIgnoreCase(msg, "%num%",
					Integer.toString(counter));
			msg = Utils.getInstance().replaceIgnoreCase(msg, "%url%", voteURL);
			msg = Utils.getInstance().replaceIgnoreCase(msg, "%SiteName%",
					voteSite.getSiteName());
			sites.add(msg);
		}
		sites = Utils.getInstance().colorize(sites);
		return Utils.getInstance().convertArray(sites);
	}

	public void openVoteSitesListGUI(Player player, int page) {
		String guiName = "VotingPlugin: VoteSites";

		ArrayList<ItemStack> items = new ArrayList<ItemStack>();

		for (VoteSite voteSite : plugin.voteSites) {
			ItemStack item = new ItemStack(Material.STONE);
			item = Utils.getInstance().nameItem(item, voteSite.getSiteName());
			items.add(item);
		}

		Inventory inv = Bukkit.createInventory(null, 54, guiName);

		int slot = 0;

		for (int i = (page - 1) * 45; i < items.size() && slot <= 54; i++) {
			try {
				inv.setItem(slot, items.get(i));
				slot++;
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}

		ItemStack placeHolder = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 7);

		ItemStack prevPage = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 5);

		int maxPages = 1 + page / 45;

		List<String> lore = new ArrayList<String>();
		lore.add("&bCurrent Page: &6" + page);
		lore.add("&bMax Pages: &6" + maxPages);

		prevPage = Utils.getInstance()
				.addLore(
						Utils.getInstance().nameItem(prevPage,
								"&cPrevious Page"), lore);

		ItemStack nextPage = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 5);

		nextPage = Utils.getInstance().addLore(
				Utils.getInstance().nameItem(nextPage, "&cNext Page"), lore);

		ItemStack back = new ItemStack(Material.STAINED_GLASS_PANE, 1,
				(short) 12);

		back = Utils.getInstance().nameItem(back, "&cBack");

		inv.setItem(45, placeHolder);

		inv.setItem(46, placeHolder);

		inv.setItem(47, prevPage);

		inv.setItem(48, placeHolder);
		inv.setItem(49, placeHolder);
		inv.setItem(50, placeHolder);

		inv.setItem(51, nextPage);

		inv.setItem(52, placeHolder);

		inv.setItem(53, back);

		player.openInventory(inv);
	}

}
